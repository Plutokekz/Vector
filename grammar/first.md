```
First(Digit) = {0,1,2,3,4,5,6,7,8,9}
First(Letter) = {A-z}
First(Name) = First(Letter)
First(Number) = {-} v First(Digit)
First(Factor) = First(Name) v First(Number) v {(}
First(Term) = First(Factor)
First(Expression) = {+,-} v First(Term)
Frst(Condition) = First(Expression) v {NOT}
First(Instruction) = First(Name) v {CALL, READ, WRITE, BEGIN, IF, WHILE}
First(MatrixType) = {SPARSE, IDENTITY, DIAGONAL, UPPERTRIANGULAR, LOWERTRIANGULAR, ORTHOGONAL}
First(Type) = {INT8,INT16,INT32,INT64,INT128,FLOAT8,FLOAT16,FLOAT32,FLOAT64,FLOAT128}
First(Block) = {CONST, VAR, PROCEDURE} v First(Instruction)
First(Program) = {PROGRAM}
```



| Nonterminal | First |
|-------------|-------|
| Digit | {0,1,2,3,4,5,6,7,8,9} |
| Letter | {A-z} |
| Name | First(Letter) |
| Number | {-} ∪ First(Digit) |
| Factor | First(Name) ∪ First(Number) ∪ {(} |
| Term | First(Factor) |
| Expression | {+,-} ∪ First(Term) |
| Condition | First(Expression) ∪ {NOT} |
| Instruction | First(Name) ∪ {CALL, READ, WRITE, BEGIN, IF, WHILE} |
| MatrixType | {SPARSE, IDENTITY, DIAGONAL, UPPERTRIANGULAR, LOWERTRIANGULAR, ORTHOGONAL} |
| Type | {INT8, INT16, INT32, INT64, INT128, FLOAT8, FLOAT16, FLOAT32, FLOAT64, FLOAT128} |
| Block | {CONST, VAR, PROCEDURE} ∪ First(Instruction) |
| Program | {PROGRAM} |