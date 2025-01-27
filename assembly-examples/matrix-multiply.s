    .global    _start
    .section   .text

multiply_matrix:
    li         t1, 3                  # t1 = 3 outer reverse counter, count down how many row we still have to calculate
    li         t2, 3                  # t2 = 3 inner stride starts with 3 for every column offset
# 1, 2,3 start with 1 then select next element at 3 bytes until 3 element are selected
# 4, 5,6
# 7, 8,9

    vsetvli    t0, t1, e8, m1, ta, ma # configure vector extension with 3 elements per vector, e8 = 8bit integer element size and dont care about the tail and maks agnostic
    vmv.v.i    v4, 0                  # Initialize v4 with zeros before first slide so we can build the result row from scalar registers
    mv         t4, a0                 # t4 =(a0)
    mv         t5, a1

outer_loop:
    li         t3, 3                  # t3 = 3 inner reverse counter for every column

    vle8.v     v0, (t4)               # load 3 elements with size 8bit from matrix a (a0) into v0
    mv         t5, a1

inner_loop:
    vlse8.v    v1, (t5), t2           # load 3 elements with size 8bit from matrix b (a1) into v1 (column from b)

    vmul.vv    v1, v0, v1             # multiply row (v0) with column (v1) and save result into v1 only keep lower bits on overflow

    vmv.v.i    v3, 0                  # set first 3 entries of v3 to 0 so we can calulate the sum in the next step and add it to the first element of v3
    vredsum.vs v3, v1, v3             # Sum all elements from v1 into v3[0] + v3[0]
    vmv.x.s    x10, v3                # read element 0 from the vector register ignores the set length of the vector and loads it into a scalar register (x10)
    sb         x10, (a2)              # Store byte result
# i have currently some memory problem
    addi       a2, a2, 1              # Move to next result position
    #vslide1up.vx v4, v4, x10 # load new row element (x10) into new vector (v4) register so we store a row at once into memory


    addi       t3, t3, -1             # substrace 1 from the inner loop counter
    addi       t5, t5, 1              # move pointer of matrix b (a1) to the second column by adding the size of 1 element to it
    bnez       t3, inner_loop         # if counter != 0 run loop again
    vse8.v v4, (a2) # save calulated row (v4) to memory (a2) if inner loop is done
    #addi a2, a2, 3 # move pointer of matrix c (a2) to the second row by adding the size*len to it

# advancing outer loop
    addi       t4, t4, 3              # move pointer of matrix a (a0) to the second row by adding the size*len to it
#addi a1, a1, -3 # reset inner pointer for matrix b (a1)
#mv t5, a1 # substrace 1 from the outer loop counter
    addi       t1, t1, -1
    bnez       t1, outer_loop         # if counter != 0 run loop again
    ret


_start:
    la         a0, matrix_a           # Load address of matrix A
    la         a1, matrix_b           # Load address of matrix B
    la         a2, matrix_c           # Load address of result matrix
    call       multiply_matrix

# Exit program
    li         a7, 93
    li         a0, 0
    ecall

    .section   .rodata                # Read-only data section
    .align     4                      # Align to 4-byte boundary
matrix_a:
    .byte      1, 2, 3
    .byte      4, 5, 6
    .byte      7, 8, 9
matrix_b:
    .byte      1, 2, 3
    .byte      4, 5, 6
    .byte      7, 8, 9

    .section   .data                  # Uninitialized data section
    .align     4                      # Align to 4-byte boundary
    .global    matrix_c
matrix_c:
    .zero      9                      # Space for result matrix (3x3 bytes)