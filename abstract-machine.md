
# Abstract Machine

The Abstract Machine or Intermediate representation for V is called MV.
And based on the Abstract Machine, but simplified and optimized in some places.

The Machine consists of two main memory ares $\text{code}$ and $\text{store}$. And works with 
Four registers $I$, $P$, $T$ and $B$.

```python
code = [] # programm 
P = 0
I = code[P]

while (I != HLT):
    P +=1
    # run instruction in I
    I = code[P]

```

## Memory

### Code

Code is an array with length $\text{codemax} + 1$ and is indexed from $0$ to $\text{codemax}$.
The Array stores the binary of the program and is not modified at runtime.

### Store

Store is an array with length $\text{storemax} + 1$ and is indexed from $0$ to $\text{storemax}$.
It holds the computations of the program, acting as the stack. It is heavily
modified at runtime. The stack is managed in cells, so $T$ accesses one cell and $T+1$ accesses the next one. 

Currently, the idea is that each cell hast the same size. This means all cells need to be as big as the biggest one, which is really inefficient. 
Maybe an approach with dynamic sized cells would be better. So, at the beginning of each cell specifies its $\text{size}$ so $T + 1$ knows the offset it has to use for the next cell. 

$$
\begin{array}{cccccc}
0 & 1 & \cdots & T & T+1 & \text{storemax} \\
[\text{data}, & \text{data}, & \text{data}, & \text{data}, & \text{data}, & \text{data}]
\end{array}
$$

#### Push

We can push something on the Stack:
Thereby they lengthen the stack. 
$$T:=T+1;\\\text{store}[T]:=\text{store}[i]$$

#### Pop

Or we can pop something from the stack:
Thereby, they're shortening the stack.
$$T:=T-1;\\\text{store}[i]:=\text{store}[T]$$

## Registers

MV has four registers, one instruction registers and four address registers.

* One Instruction register $I$
* three address register
    * $P$ (Program counter, a code address)
    * $T$ (Top of stack, a store address)
    * $B$ (base of the current segment, a store address)

Instructions can modify the program counter $P$, $P$ holds store address to the following instructions that should be executed. $I$ contains, the instructions that are currently executed. 

## Procedure call / procedure interaction 

For each procedure call, VM will create a local stack on the global stack. 
The stack contains the segment descriptor at the beginning, local variables
where each of the variables has its own cell for accessing. And a stack for the local computations of the procedure call.

### Segment Descriptor

The Address of the first cell of the procedure segment is the base address and is kept in the register $B$. The current Procedure segment is the one on top of the stack which consists of all cells from $\text{store}[B]$ to $\text{store}[T]$

The Segment descriptor consists of three addresses:

* Dynamic Link (DL)
    - is the base address of the immediately preceding segment

* Return Address (RA)
    - is the code address at which the program resumes after completion of the procedure call

* Static Link (SL)
    - is the address of the static predecessor. (It is necessary for accessing the variables global to a procedure call)


#### calculating the staticlink
At compile-time: \
The (static) nesting depth of each block is determined by incrementing a counter when a block is entered and decrementing it when a block is left.
A call to a procedure $P$ is within a block with nesting depth $p$. 
A call to a procedure $Q$ within $P$ is within a block with nesting depth $q$. $s = p - q$ is the step of that call of $Q$.

At run-time:\
Procedure call (or incarnations) with step $0$ yield $DL = B$, the base address of the immediately preceding segment.

Procedure call (or incarnations) with step $s \geq 1$ yield $DL$ being the base address of the sth preceding segment, the dynamic
links making it possible to get back from segments to segment.



The (store) addresses of the variables’ incarnation on the stack are
determined as follows:

I At compile-time:
The offset, or address of a variable relative to the beginning of its procedure segment, is determined. Every variable is associated with its relative address (step, offset), whereby a variable’s step is computed like for a procedure.

I At run-time:
From a variable relative address $(s, o)$, its absolute address is obtained as the offset $o$ from the base address of the $s^{th}$ segment preceding the current (top) segment.

## Instructions

### base

* LOD (load): loads on the stack the content of a relatively
addressed store cell.
* LOD $s o$ ($s$ step, $o$ offset, both integers):

$$T:=T+1;\\ \text{store}[T]:=\text{store}[\text{base}(s)+o]$$

* STO (store): removes the top cell and after saving its contents in a relatively addressed cell.
* STO $s i$ ($i$ non-negative integer):
$$\text{store}[\text{base}(s)+i] := \text{store}[T];\\ T := T - 1$$


* INC (increment): enlarges a segment
* INC $i$ ($i$ non-negative integer):
$$T := T + i$$


* LIT (literal): loads an integer on the stack
* LIT $n$ ($n$ integer):
$$T:=T+1; \text{store}[T] := n$$

* JMP (jump): unconditional jump
* JMP $a$ ($a$ code address):
$$P := a$$


* JOT (jump-on-true): conditional jump
* JOT $a$ ($a$ code address):
$$\text{if} (\text{store}[T] == \text{true})\\ P := a;\\ T := T-1$$

* JOF (jump-on-false): conditional jump
* JOF $a$ ($a$ code address):
$$\text{if} (\text{store}[T] == \text{false})\\ P := a;\\ T := T-1$$

* CAL (call): procedure call
* CAL $s a$ ($s$ step, an integer, $a$ code address)
$$ T := T+1;\\ \text{store}[T+0] := B;\\ \text{store}[T+1] := P;\\ \text{store}[T+2] := \text{base}(s);\\ B := T;\\ T := B+2;\\ P := a$$

* RET (return): return from a procedure
$$P := \text{store}[B+1];\\ T := B-1;\\ B := \text{store}[B]$$

* REA (read): reads and loads an input, it on the stack
$$ T:=T+1;\\ \text{store}[T] := \text{read}()$$

* WRI (write): output the value on top of the stack and remove the top stack cell
$$ \text{write}(\text{store}[T])\\
T := T-1$$
* HLT (halt): halts the abstract machine MI (see MI’s main cycle)
$$\text{HLT}$$


For efficient matrix operations, we would need to know input and output type. And we need to add extra operations for our extra types like triangle and identity matrix. So the Abstract machine doesn't need to think about it. And we can handle the type decisions in Haskell.

### OPR (operator): arithmetic operators 
* OPR +:
$$ \text{store}[T-1] := (\text{store}[T-1] + \text{store}[T]);\\ T := T-1$$
* OPR -:
$$ \text{store}[T-1] := (\text{store}[T-1] - \text{store}[T]);\\ T := T-1$$
* OPR *:
$$ \text{store}[T-1] := (\text{store}[T-1] * \text{store}[T]);\\ T := T-1$$
* OPR /:
$$ \text{store}[T-1] := (\text{store}[T-1] / \text{store}[T]);\\ T := T-1$$
* OPR .*:
$$ \text{store}[T-1] := (\text{store}[T-1] \text{ ElementMult } \text{store}[T]);\\ T := T-1 $$
* OPR ./:
$$ \text{store}[T-1] := (\text{store}[T-1] \text{ ElementDiv } \text{store}[T]);\\ T := T-1 $$
* OPR @:
$$ \text{store}[T-1] := (\text{store}[T-1] \text{ MatrixMult } \text{store}[T]);\\ T := T-1 $$

### OPR (operator): Boolean operators
* OPR NOT:
$$ \text{store}[T]:= \text{not}(\text{store}[T])$$
* OPR ==:
$$ \text{store}[T-1] := (\text{store}[T-1] == \text{store}[T]); T := T-1 $$
* OPR <:
$$ \text{store}[T-1] := (\text{store}[T-1] < \text{store}[T]); T := T-1 $$
* OPR >:
$$ \text{store}[T-1]:= (\text{store}[T-1] > \text{store}[T]); T := T-1 $$
* OPR <=:
$$ \text{store}[T-1] := (\text{store}[T-1] <= \text{store}[T]); T := T-1 $$
* OPR >=:
$$ \text{store}[T-1]:= (\text{store}[T-1] >= \text{store}[T]); T := T-1 $$
