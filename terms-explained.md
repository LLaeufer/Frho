# Terms and their Explanation

## Constant
`value` 
Evaluates to the value of the constant

## Let
`let variable = term`
Let saves the output of a term to a variable. One a variable has been set, it can't be overwritten in the same context. The variable can be overwritten in a sub-context.

`let myvar = 1; let myvar = 2` This is illegal.
`let myvar = 1; (let myvar = 2)` This is legal, since the brackets create a sub-context.

## Variable
`variable`
Evaluates to the value saved under this variable. Causes an exception when called on an empty variable.

## If
`if (decider) then (consequence) else (alternative)`
The decider needs to evaluate to a boolean value, otherwise an exception is raised.
If the decider evaluates to true consequence will be evaluated, otherwise alternative will be evaluated.

## Record Selection
`record.label`
Record evaluates to a record value, label selects the label within the record.
Should both the record and the label exists, this evaluates to the value behind the label in the record. Otherwise, this yields an exception.

## Record Update
`record.label <- term`
Record evaluates to a record value, label selects the label within the record. This sets the label within the record to the value which term evaluates to.

## Case
`case variant with ([variant_label: variable ] -> consequence; alternative_variable -> alternative)`
Variant evaluates to a variant value. If the variant value contains the variant_label, the consequence will be evaluated with the variable set to the value associated with the variant_label.
Alternatively the alternative_variable will be set to the variant value and alternative will be evaluated.

## Print
`print term`
Prints the value the term evaluates to, doesn't alter the program state.

## Functions
`lam (variable: type) (term)`
`lam (variable: type) : output_type (term)`
`rec label (variable: type) (term)`
`rec label (variable: type) : output_type (term)`
`fun label (variable: type) (term)`
`fun label (variable: type) : output_type (term)`

Lam, rec and fun are the three types of functions in Frho.
Lams have a single variable, with their type, and an optional output_type. Should the lam being called the variable will be set to the given value and the term will be evaluated.
Recs also provide themselves as a variable named like the label to the evaluated term.
Funs work like recs, but also save themselves to the current environment, so they can be called by variable.
In case an output_type isn't provided, the typechecker will try to figure out the correct output_type. In case this isn't possible or a wrong output_type will be assumed the typecheck will fail.

## Function Calls
`term(argument)`
Term needs to evaluate to a function, otherwise this results in an exception. The argument gets evaluated to the variable specified in the function definition.

## Type Application
`app term <type>`
The term needs to evaluate to a bigLam value. Replaces the typevariable from the bigLam with the type provided by the type application. Returns the subvalue in the bigLam value.

## BigLambda Notation 
`lam <X: kind> : type term`
Terms like functions can have types, these can be set to the typevariable X, the big lambda notation ensures that only types with the right kind can be replaced. With type denoting the resulting type of the term. This type might or might not contain X.
