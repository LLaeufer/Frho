# Types
Since the syntax defines 3 different endings for record {r} and variant [r] types, Notation to differentiate them is introduced
`r` will stand for a closed row
`r, *` will stand for a dynamic row
`r, X` will stand for a row with a type-variable
`r, ?` will stand for a row with any row end. With the condidion that the row ends with the same subidentifyier are identical.

### Convertion from syntactic occurances to big step occurrances
\- --> Absent

type --> A
------------------- \
+type --> Present(A)

@ --> Star

# RowEnd
rowEnd := Closed | Dynamic | tVariable(var) \
With a closed end having the form {r1}, dynamic having the form {r1, *} and the tVariable(var) having the form {r1, X} with X being any tVariable.

# Occurrences

Occurrence, O := Absent, Present(type), Star

Absent occurrences are stylized as `-`, present occurrences are stylized as `+A` and Star occurrences are stylized as `@`.
Absent occurrences must not exist in the current Record or Variant. Present occurrences are types that exist in the current Record or variant. Star occurrences might or might not exist.

LabelOccurrence, LO := (label, O)

LabelOccurrences, LOs := LO;LOs | {}

LabelOccurrences are basically a list containing LO

# Values
BigStepValues, BSV := vInt(int)
                    | vBool(bool)
                    | vFloat(float)
                    | vString(string)
                    | vAll(label, kind, BSV)
                    | vRecord(map\<label, BSV>)
                    | vVariant(label, BSV)
                    | vFunc(environment, var, term, type)
                    | vDynamic(BSV, groundType, blame)
                    | vCast(BSV, type, blame, type)
                    | vSeal(BSV, type, alpha)

vCast explaination:
vCast(original value, original type, blame label, type which is cast to)

## getBSVKind
getBSVKind gets the kind of BSV a current value is. `vInt(42)` would return `vInt`, `vBool(true)` would return `vBool`

# GroundTypes
GroundTypes are simplified types.

groundType, GT := alpha
                | int 
                | bool 
                | float 
                | string 
                | (\*)->(\*) 
                | {*}
                | [\*]


# BaseTypes
BaseTypes are the most basic types

baseType, BT := int | bool | float | string


# QPoly
Types are called QPoly if they aren't universal types, records types or variant types and contain a dynamic type or a star occurrence.

# Environments
Sigma: Maps alphas to type
Gamma: Maps labels to type
Rho: Maps labels to BSV

# Utility functions
## typeOf(BSV)
typeOf(BSV) maps a BigStepValue to it's appropriate type. This is fairly straight forward, with exceptions for the vCast, vDynamic and vSeal value.
vCast Values get converted to their cast outputs so `vCast(BSV, A, blame, B)` would be `B` a vDynamic would be converted to a `*` and a vSeal(BSV, type, alpha) would be converted to a `tName(alpha)`.

## toMap([...])
toMap converts a vector of tuples to a map.

## isBaseType(A)
Returns whether A is a base type

## isGroundType(A)
Returns whether A is a ground type
