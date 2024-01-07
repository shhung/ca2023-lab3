.global _start
_start:
    addi    t0, x0, 3
    addi    a0, x0, 0
l1:
    addi    a0, a0, 1
    addi    t0, t0, -1
    bne     t0, x0, l1

loop:
    j loop
