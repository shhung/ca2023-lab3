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
      for (i <- 1 to 3) {
        c.io.instruction.poke(0x002081b3L.U)
        c.clock.step()
      }
    }
  }
}
