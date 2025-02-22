package main

import (
	"fmt"
	"strings"
	"github.com/willfaught/ebnf"
)

// const Grammar = `Expression = Term { ( "+" | "-" ) Term } .
// Term = Factor { ( "*" | "/" ) Factor } .
// Factor = Number | "(" Expression ")" .
// Number = Digit { Digit } .
// Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
// `

const Grammar = `
Program = "PROGRAM" Name ":" Block "." .

Block =
    {"CONST" Type Name "=" Number {"," Name "=" Number} ";"}
    {"VAR" Type Name {"," Name} ";"}
    {"PROCEDURE" Name ";" Block ";"}
    Instruction .

Type = 
    ("INT8"
    | "INT16"
    | "INT32"
    | "INT64"
    | "INT128"
    | "FLOAT8"
    | "FLOAT16"
    | "FLOAT32"
    | "FLOAT64"
    | "FLOAT128") 
    ("[" Size "]"
    | "[" Size "]" MatrixType) .

Size = Digit {"," Digit} .

Instruction = 
    Name "=" Expression
    | "CALL" Name
    | "READ" Name
    | "WRITE" Expression
    | "BEGIN" Instruction {";" Instruction} "END"
    | "IF" Condition "THEN" Instruction
    | "WHILE" Condition "DO" Instruction .

Condition =
    Expression ("==" | (("<" | ">") ["="])) Expression
    | "NOT" Condition .

Expression = ["+" | "-"] Term {("+" | "-") Term} .

Term = Factor {("*" | "/") Factor} .

Factor = Name | Number | "(" Expression ")" .

Name = Letter {Letter | Digit | "_"} .

Number = ["-"] Digit {Digit} ["." {Digit}] .

Letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" .

Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .

MatrixType = 
    "SPARSE"
    | "IDENTITY"
    | "DIAGONAL"
    | "UPPERTRIANGULAR"
    | "LOWERTRIANGULAR"
    | "ORTHOGONAL" .
`

func main() {
	g, err := ebnf.Parse(Grammar)
	if err != nil {
		fmt.Println("Error parsing grammar:", err)
		return
	} else {
		fmt.Println("Grammar parsed successfully!")
		// fmt.Println(strings.Repeat("-", 100))
		// fmt.Print(g)
	}

	// first := g.First()
	// fmt.Println("FIRST:")
	// fmt.Println(first)
	// PrintGrammarMap(first)

	fmt.Println(strings.Repeat("-", 100))

	// follow := g.Follow()	
	// fmt.Println("FOLLOW:")
	// fmt.Println(follow)
	// PrintGrammarMap(follow)
	fmt.Println(g.Validate())
	fmt.Println(g.Conflict())
}
