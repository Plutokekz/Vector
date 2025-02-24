assemblyNew = """
.text 
.global main
main:
mv fp, sp
addi sp, sp, -24
li t0, 1
sd t0, 16(sp)
sd t0, 8(sp)
sd t0, 0(sp)
addi sp, sp, -24
j .block_3
.procedure_potenz_0:
addi sp, sp, -8
addi sp, sp, -8
li t0, 1
sd t0, 0(sp)
ld t1, 0(sp)
mv t0, fp
ld t0, -8(t0)
sd t1, -48(t0)
addi sp, sp, 8
addi sp, sp, -8
mv t0, fp
ld t0, -8(t0)
ld t1, -40(t0)
sd t1, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -32(t0)
addi sp, sp, 8
.while_start_1:
addi sp, sp, -8
mv t0, fp
ld t1, -32(t0)
sd t1, 0(sp)
addi sp, sp, -8
li t0, 1
sd t0, 0(sp)
ld t0, 0(sp)
ld t1, 8(sp)
slt t3, t1, t0
sd t3, 8(sp)
addi sp, sp, 8
ld t0, 0(sp)
andi t0, t0, 1
xori t0, t0, 1
sd t0, 0(sp)
ld t0, 0(sp)
addi sp, sp, 8
beq t0, zero, .while_end_2
addi sp, sp, -8
mv t0, fp
ld t0, -8(t0)
ld t1, -48(t0)
sd t1, 0(sp)
addi sp, sp, -8
mv t0, fp
ld t0, -8(t0)
ld t1, -32(t0)
sd t1, 0(sp)
ld t0, 0(sp)
ld t1, 8(sp)
mul t3, t1, t0
sd t3, 8(sp)
addi sp, sp, 8
ld t1, 0(sp)
mv t0, fp
ld t0, -8(t0)
sd t1, -48(t0)
addi sp, sp, 8
addi sp, sp, -8
mv t0, fp
ld t1, -32(t0)
sd t1, 0(sp)
addi sp, sp, -8
li t0, 1
sd t0, 0(sp)
ld t0, 0(sp)
ld t1, 8(sp)
sub t3, t1, t0
sd t3, 8(sp)
addi sp, sp, 8
ld t1, 0(sp)
mv t0, fp
sd t1, -32(t0)
addi sp, sp, 8
j .while_start_1
.while_end_2:
ld ra, -16(fp)
mv sp, fp
ld fp, -8(fp)
jalr zero, 0(ra)
.block_3:
addi sp, sp, -8
li t0, 10
sd t0, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -32(t0)
addi sp, sp, 8
addi sp, sp, -8
li t0, 3
sd t0, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -40(t0)
addi sp, sp, 8
addi sp, sp, -24
sd fp, 16(sp)
auipc ra, 0
addi ra, ra, 32
sd ra, 8(sp)
mv t0, fp
sd t0, 0(sp)
mv fp, sp
addi fp, fp, 24
jal zero, .procedure_potenz_0
addi sp, sp, -8
mv t0, fp
ld t1, -48(t0)
sd t1, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -48(t0)
addi sp, sp, 8
li a0, 0
li a7, 93
ecall
"""

assemblyOld = """
.text
.global main
main:
mv fp, sp
addi sp, sp, -24
li t0, 1
sd t0, 16(sp)
sd t0, 8(sp)
sd t0, 0(sp)
addi sp, sp, -24
j .block_3
.procedure_potenz_0:
addi sp, sp, -8
addi sp, sp, -8
li t0, 1
sd t0, 0(sp)
ld t1, 0(sp)
mv t0, fp
ld t0, -8(t0)
sd t1, -48(t0)
addi sp, sp, 8
addi sp, sp, -8
mv t0, fp
ld t0, -8(t0)
ld t1, -40(t0)
sd t1, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -32(t0)
addi sp, sp, 8
.while_start_1:
addi sp, sp, -8
mv t0, fp
ld t1, -32(t0)
sd t1, 0(sp)
addi sp, sp, -8
li t0, 1
sd t0, 0(sp)
ld t0, 0(sp)
ld t1, 8(sp)
slt t3, t1, t0
sd t3, 8(sp)
addi sp, sp, 8
ld t0, 0(sp)
andi t0, t0, 1
xori t0, t0, 1
sd t0, 0(sp)
ld t0, 0(sp)
addi sp, sp, 8
beq t0, zero, .while_end_2
addi sp, sp, -8
mv t0, fp
ld t0, -8(t0)
ld t1, -48(t0)
sd t1, 0(sp)
addi sp, sp, -8
mv t0, fp
ld t0, -8(t0)
ld t1, -32(t0)
sd t1, 0(sp)
ld t0, 0(sp)
ld t1, 8(sp)
mul t3, t1, t0
sd t3, 8(sp)
addi sp, sp, 8
ld t1, 0(sp)
mv t0, fp
ld t0, -8(t0)
sd t1, -48(t0)
addi sp, sp, 8
addi sp, sp, -8
mv t0, fp
ld t1, -32(t0)
sd t1, 0(sp)
addi sp, sp, -8
li t0, 1
sd t0, 0(sp)
ld t0, 0(sp)
ld t1, 8(sp)
sub t3, t1, t0
sd t3, 8(sp)
addi sp, sp, 8
ld t1, 0(sp)
mv t0, fp
sd t1, -32(t0)
addi sp, sp, 8
j .while_start_1
.while_end_2:
ld ra, -16(fp)
mv sp, fp
ld fp, -8(fp)
jalr zero, 0(ra)
.block_3:
addi sp, sp, -8
li t0, 10
sd t0, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -32(t0)
addi sp, sp, 8
addi sp, sp, -8
li t0, 3
sd t0, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -40(t0)
addi sp, sp, 8
addi sp, sp, -24
sd fp, 16(sp)
auipc ra, 0
addi ra, ra, 32
sd ra, 8(sp)
mv t0, fp
sd t0, 0(sp)
mv fp, sp
addi fp, fp, 24
jal zero, .procedure_potenz_0
addi sp, sp, -8
mv t0, fp
ld t1, -48(t0)
sd t1, 0(sp)
ld t1, 0(sp)
mv t0, fp
sd t1, -48(t0)
addi sp, sp, 8
li a0, 0
li a7, 93
ecall
"""

print(assemblyNew == assemblyOld)