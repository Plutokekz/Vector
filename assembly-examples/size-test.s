    .global  _start
    .section .text

test:
    li       t1, 128
    vsetvli  t0, t1, e32, m8, ta, ma

_start:
    call     test
    li       a7, 93
    li       a0, 0
    ecall