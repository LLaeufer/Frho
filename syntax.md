# Labels
label := ([a-z]+[a-zA-Z0-9_]*)

typeVariable, X := ([A-Z]+[A-Z0-9_]*)

blameLabel := "#" label

typeName, alpha, beta := "%" label

labels := label "," labels | label | ""

# Values
constant_value := int | float | bool | string
int := [0-9]+
float := [0-9]+\.[0-9]+
bool := "true" | "false"
string := String flanked by " symbols, not containing any " in the string

# Kinds
kind, K := ty | L
ty := "Ty"
L := "{" labels "}"

# Types
type, A, B := typeVariable | typeName | "int" | "bool" | "float" | "string" | "*" | "{" row "}" | "[" row "]" | "("type")->("type")" | "all (" typeVariable ":" kind ")" type



# Terms
term, M := constant 
         | let
         | variable
         | recordCreation
         | variantCreation
         | recordSelection
         | recordUpdate
         | if
         | bigLam
         | typeApplication
         | cast
         | convertion
         | case
         | print
         | fun
         | rec
         | lam
         | funCall
         | algorithmicFunctions

constant := constant_value
let := "let" label "=" term
variable := label
recordCreation := "{" labeledTerms "}"
variantCreation := "[" labeledTerm "]"
recordSelection := lightTerm "." label
recordUpdate := lightTerm "." label "<-" term
if := "if" termBlock "then" termBlock "else" termBlock
bigLam := "lam <" typeVariable ":" kind ">" : type term
typeApplication := term "<" type ">"
cast := term ":" type "=[" blame "]=>" type
convertion := term ":" type "~[" convertionLabel "]~>" type
case := "case" term "with ([" label ":" label "]->" term ";" label "->" term ")"
print := "print" term
fun := "fun" label "(" label ":" type ")" [: type] termBlock
rec := "rec" label "(" label ":" type ")" [: type] termBlock
lam := "lam" "(" label ":" type ")" [: type] termBlock
funCall := term termBlock
algoritmicFunctions := term "||" term
                     | term "&&" term
                     | "!" term
                     | "-" term
                     | term "==" term
                     | term ">" term
                     | term "<" term
                     | term ">=" term
                     | term "<=" term
                     | term "+" term
                     | term "-" term
                     | term "*" term
                     | term "/" term 
                     (Normal operator binding applies)




# Miscellaneous definitions
fieldOccurence := foAbsent | fOPresent | fOStar
fOAbsent := "-"
fOPresent := "+" type
fOStar := "@"
labeledFO := label ":" fieldOccurence
labeledFOs := labeledFO "," labeledFOs | labeledFO | ""
row := labeledFOs | labeledFOs, * | labeledFOs, typeVariable

labeledTerm := label ":" term
labeledTerms := labeledTerm , labeledTerms | labeledTerm | ""

termBlock := "(" unbracketedTermBlock ")"
unbracketedTermBlock := term ";" unbracketedTermBlock | termBlock

lightTerm := termBlock | variable

blame := blameAbsent | blamePresent
blameAbsent := "-" blameLabel
blamePresent := "+" blameLabel

convertionLabel := convertionLabelAbsent | consertionLabelPresent
convertionLabelAbsent := "-" typeName
convertionLabelPresent := "+" typeName

