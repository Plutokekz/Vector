# Linear Algebra Compiler for an imperative language in Haskell

## Grammar

* Extension to MI  
* LL(1) Parser  
* EBNF

## Types

* INT (8 \- 128\)  
* FLOAT (8 \- 128\)  
  Specific type is known to compile time, so it is possible to optimize for speed and memory. 

* Vector (Array)  
* Matrix (Array of Arrays)  
* Initially, only static arrays. Can be of type int or float  
* Possibly could include dynamic arrays (and consequently dynamic matrices)

* Sparse  
* Identity  
* Diagonal  
* Upper Triangular  
* Lower Triangular  
* Orthogonal  
  * Eventually could specify matrix operation rules directly in the typing system in Haskell (e.g. Diagonal x Diagonal \= Diagonal)

  Simple matrices that are often used, so we want to directly integrate them into the language as a type. Only the size needs to be given. 


* Use Haskell’s Typing system   
  * Type-level matrix properties  
  * Enforces correct input dimensions for operations (rows matching columns)  
  * Specifies correct output dimensions for operations  
  * Lazy evaluation for large matrices (?)  
    * Eventually out of scope for an imperative language, but could be an interesting feature

## Matrix and Vector Operations

Using the [riscv-v extension](https://github.com/riscvarchive/riscv-v-spec/blob/master/v-spec.adoc) as much as possible. Splitting big vectors and matrices into smaller ones so we can directly make calculations using the extension’s commands on the fragmented matrices / vectors. Simple operations are mostly specified in the Abstract Machine and Assembly Code.

RISC-V:

* Configurable vector length (vlen)  
* Vector registers could hold multiple matrix elements  
* Eventually: automatic adaptation / fragmentation to available vector length


Simple Operations:

* Add  
* Sub  
* Mul  
  * Operations could be “type/property-aware”, using the typing system in haskell could be efficient (for instance Diagonal / Identity matrices have simpler computation)  
  * Catches errors at compile time  
  * `-- Property inference`  
  * `type family InferProperty (op :: Operation)`  
  *                         `(p1 :: Property)`  
  *                         `(p2 :: Property) :: Property where`  
  *    `InferProperty 'Multiply 'UpperTriangular 'UpperTriangular =`  
  *        `'UpperTriangular`  
  *    `InferProperty 'Add 'Symmetric 'Symmetric =`  
  *        `'Symmetric`  
      
* Dot Product  
* Cross Product  
* Transpose

Complex Operations:

* Inverse  
* Projections  
* Orthogonality  
* Normalization / Orthonormalization


Commit where vector extension is used by FFmpeg  
[\[FFmpeg-devel\] \[PATCH v3\] lavc/h264chroma: RISC-V V add motion compensation for 8x8 chroma blocks](https://ffmpeg.org/pipermail/ffmpeg-devel/2023-May/310013.html)  
[The RISC-V Vector ISA Tutorial](https://riscv.org/wp-content/uploads/2024/12/15.20-15.55-18.05.06.VEXT-bcn-v1.pdf)

## Extra extension if there is enough time: ML

Function that should be directly implemented on the Abstract machine level. We want to start with the activation function, maybe we will extend with loss and optimizers. 

### Activation Function

* Sigmoid  
* Relu  
* Softmax  
* ThanH

### Loss

* Binary Cross-Entropy

### Optimizer

* Adam

