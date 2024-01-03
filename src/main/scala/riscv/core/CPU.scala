// mycpu is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util.Cat
import riscv.CPUBundle
import riscv.Parameters

class CPU extends Module {
  val io = IO(new CPUBundle)

  val regs       = Module(new RegisterFile)
  val inst_fetch = Module(new InstructionFetch)
  val id         = Module(new InstructionDecode)
  val ex         = Module(new Execute)
  val mem        = Module(new MemoryAccess)
  val wb         = Module(new WriteBack)

  // WB include MEM and WB
  class WBControl extends Bundle {
    val memory_read_enable  = Bool()
    val memory_write_enable = Bool()
    val wb_reg_write_source = UInt(2.W)
    val reg_write_enable    = Output(Bool())
    val reg_write_address   = Output(UInt(Parameters.PhysicalRegisterAddrWidth))
  }

  class FD_EXBundle extends Bundle {
    val instruction         = UInt(Parameters.InstructionWidth)
    val instruction_address = UInt(Parameters.AddrWidth)
    val immediate           = UInt(Parameters.DataWidth)
    val aluop1_source       = UInt(1.W)
    val aluop2_source       = UInt(1.W)
    val reg_read_address1   = UInt(Parameters.PhysicalRegisterAddrWidth)
    val reg_read_address2   = UInt(Parameters.PhysicalRegisterAddrWidth)
    val wbcontrol           = new WBControl
  }

  class EX_WBBundle extends Bundle {
    val instruction         = UInt(Parameters.InstructionWidth)
    val instruction_address = UInt(Parameters.AddrWidth)
    val mem_alu_result      = UInt(Parameters.DataWidth)
    val reg2_data           = UInt(Parameters.DataWidth)
    val wbcontrol           = new WBControl
  }

  val fd_ex = RegInit(0.U.asTypeOf(new FD_EXBundle))
  val ex_wb = RegInit(0.U.asTypeOf(new EX_WBBundle))

  io.deviceSelect := mem.io.memory_bundle
    .address(Parameters.AddrBits - 1, Parameters.AddrBits - Parameters.SlaveDeviceCountBits)

  // IF/ID
  inst_fetch.io.jump_address_id       := ex.io.if_jump_address
  inst_fetch.io.jump_flag_id          := ex.io.if_jump_flag
  inst_fetch.io.instruction_valid     := io.instruction_valid
  inst_fetch.io.instruction_read_data := io.instruction
  io.instruction_address              := inst_fetch.io.instruction_address

  id.io.instruction := inst_fetch.io.instruction

  // Pipelining, FD-EXE
  fd_ex.instruction         := inst_fetch.io.instruction
  fd_ex.instruction_address := inst_fetch.io.instruction_address
  fd_ex.immediate           := id.io.ex_immediate
  fd_ex.aluop1_source       := id.ex_aluop1_source
  fd_ex.aluop2_source       := id.ex_aluop2_source
  fd_ex.read_address1       := id.io.regs_reg1_read_address
  fd_ex.read_address2       := id.io.regs_reg2_read_address
  fd_ex.wbcontrol.memory_read_enable  := id.io.memory_read_enable
  fd_ex.wbcontrol.memory_write_enable := id.io.memory_write_enable
  fd_ex.wbcontrol.regs_write_source   := id.io.regs_write_source
  fe_ex.wbcontrol.reg_write_enable    := id.io.reg_write_enable
  fe_ex.wbcontrol.wb_reg_write_source := id.io.wb_reg_write_source

  // EXE
  regs.io.read_address1 := fd_ex.regs_reg1_read_address
  regs.io.read_address2 := fd_ex.regs_reg2_read_address
  regs.io.debug_read_address := io.debug_read_address
  io.debug_read_data         := regs.io.debug_read_data

  ex.io.instruction         := fd_ex.instruction
  ex.io.instruction_address := fd_ex.instruction_address
  when(ex_wb.wbcontrol.reg_write_enable && ex_wb.wbcontrol.reg_write_address === fd_ex.reg_read_address1) {
    when(ex_wb.wbcontrol.memory_read_enable) {
      ex.io.reg1_data := mem.io.wb_memory_read_data
    }.otherwise {
      ex.io.reg1_data := ex_wb.mem_alu_result
    }
  }.otherwise {
    ex.io.reg1_data := regs.io.read_data1
  }
  when(ex_wb.wbcontrol.reg_write_enable && ex_wb.wbcontrol.reg_write_address === fd_ex.reg_read_address2) {
    when(ex_wb.wbcontrol.memory_read_enable) {
      ex.io.reg2_data := mem.io.wb_memory_read_data
    }.otherwise {
      ex.io.reg2_data := ex_wb.mem_alu_result
    }
  }.otherwise {
    ex.io.reg2_data := regs.io.read_data1
  }
  ex.io.immediate           := fd_ex.ex_immediate
  ex.io.aluop1_source       := fd_ex.ex_aluop1_source
  ex.io.aluop2_source       := fd_ex.ex_aluop2_source

  // Pipelining, EXE-WB
  ex_wb.instruction         := fd_ex.instruction
  ex_wb.instruction_address := fd_ex.instruction_address
  ex_wb.mem_alu_result      := ex.io.mem_alu_result
  ex_wb.reg2_data           := regs.io.read_data2
  ex_wb.wbcontrol           := fd_ex.wbcontrol

  // WB
  mem.io.alu_result          := ex_wb.mem_alu_result
  mem.io.reg2_data           := ex_wb.read_data2
  mem.io.memory_read_enable  := ex_wb.wbcontrol.memory_read_enable
  mem.io.memory_write_enable := ex_wb.wbcontrol.memory_write_enable
  mem.io.funct3              := ex_wb.instruction(14, 12)
  regs.io.write_enable  := ex_wb.wbcontrol.reg_write_enable
  regs.io.write_address := ex_wb.wbcontrol.reg_write_address
  regs.io.write_data    := wb.io.regs_write_data

  io.memory_bundle.address := Cat(
    0.U(Parameters.SlaveDeviceCountBits.W),
    mem.io.memory_bundle.address(Parameters.AddrBits - 1 - Parameters.SlaveDeviceCountBits, 0)
  )
  io.memory_bundle.write_enable  := mem.io.memory_bundle.write_enable
  io.memory_bundle.write_data    := mem.io.memory_bundle.write_data
  io.memory_bundle.write_strobe  := mem.io.memory_bundle.write_strobe
  mem.io.memory_bundle.read_data := io.memory_bundle.read_data

  wb.io.instruction_address := ex_wb.instruction_address
  wb.io.alu_result          := ex_wb.mem_alu_result
  wb.io.memory_read_data    := mem.io.wb_memory_read_data
  wb.io.regs_write_source   := ex_wb.wbcontrol.wb_reg_write_source
}
