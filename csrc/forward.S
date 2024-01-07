.global _start
_start:
    addi    a0, x0, 3
    addi    a1, x0, 5
    add     a2, a0, a1

    addi    a0, x0, 3
    addi    a1, x0, 5
    add     a3, a1, a0

    addi    a0, x0, 0x4
    addi    a1, x0, 0xA8
    sw      a1, 0(a0)
    addi    t0, x0, 4
    lw      t1, 0(a0)
    add     a4, t1, t0

    addi    t0, x0, 4
    lw      t1, 0(a0)
    add     a5, t0, t1

loop:
    j loop
