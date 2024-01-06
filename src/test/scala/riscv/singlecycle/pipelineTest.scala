// mycpu is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.singlecycle

import java.nio.ByteBuffer
import java.nio.ByteOrder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import peripheral.InstructionROM
import peripheral.Memory
import peripheral.ROMLoader
import riscv.core.CPU
import riscv.core.ProgramCounter
import riscv.Parameters
import riscv.TestAnnotations


class PipelineTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Pipeline")
  it should "print out the stage register" in {
    test(new CPU).withAnnotations(TestAnnotations.annos) { c =>
      c.io.instruction_valid.poke(true.B)


      // c.io.instruction.poke(0x3e001463L.U) // bne x0, x0, 1000
      // // c.io.instruction.poke(0x3e000463L.U) // beq x0, x0, 1000
      // c.clock.step()
      // c.io.instruction.poke(0x002081b3L.U) // add x3, x1, x2
      // c.clock.step()
      // c.io.instruction.poke(0x0146a583L.U) // lw x11, 20(x13)
      // c.clock.step()

      c.io.instruction.poke(0x00100513L.U) // addi x10, x0, 1
      c.clock.step()
      c.io.instruction.poke(0x00500593L.U) // addi x11, x0, 5
      c.clock.step()
      c.io.instruction.poke(0x40a58633L.U) // sub x12, x11, x10
      c.io.instruction.poke(0x00a58633L.U) // add x12, x11, x10
      c.clock.step(5)
    }
  }
}
