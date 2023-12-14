# Wellformedness
|- gamma Checks the wellformedness of every type in the environment
gamma |- A : K Checks the wellformedness of a type and their corresponding type
A Kind can be of form "Ty" or can be a list of labels. Only records, variants, typevariables and dynamic types can have a list of labels as their type. 
Should a list of labels be submitted for the wellformedness check the type MUST NOT contain any of the submitted labels. Nor is it allowed for a record or variant to have two or more lables of the same name.

In other words this would fail gamma |- {label1: +A, label2: +B} : {label1} since the submitted kind must have NO OVERLAP with the kind of the submitted type

## Wellformed typing context
|- gamma

A typing context is wellformed iff all types and typevariables in the typing context are wellformed. And no label is present multiple types in the typing context.

|- {}

A empty set is wellformed. Meaning a typing context not containing any types is wellformed.

|- gamma \
|- x not in gamma \
gamma |- A: Ty \
-------- \
|- gamma, x:A

A typing context gamma containing a label x with a type A is wellformed if gamma without x is wellformed, gamma without x doesn't contain a second x and A is wellformed.

|- gamma \
|- X not in gamma \
------- \
|- gamma, X:K

A typing context gamma containing a typevariable X with a kind K is wellformed if gamma without X is wellformed and gamma without X doesn't contain a second X.

## Wellformed types and rows
gamma |- A : K

### Dynamic types

|- gamma \
-------- \
gamma |- * : K

A dynamic type can have any kind

### Type variables
|- gamma \
X:K in gamma \
------------- \
gamma |- X : K

Since we don't know the type of the typevariable by the time we need to check for it's wellformedness, we only test whether the typevariable along it's appropriate kind is present in gamma.

### Base types
|- gamma \
--------- \
gamma |- baseType : Ty

### Function types
gamma |- A : Ty \
gamma |- B : Ty \
--------------- \
gamma |- (A)->(B) : Ty

### Univeral types
gamma, X:K |- A : Ty \
-------------------- \
gamma |- all (X:K) A : Ty

## Wellformed row types and rows

gamma |- r : {} \
--------------- \
gamma |- {r} : Ty

gamma |- r : {} \
--------------- \
gamma |- [r] : Ty

gamma |- O \
gamma |- r :(L union {label}) \
label not in L \
----------------------- \
gamma |- label: O;r : L

|- gamma \
X:L1 in gamma \
------------- \
gamma |- X\L2 : L1 union L2

This rule checks whether a typevar for a subset of Kind-Labels exists in gamma. This is used for rows that contain typevariables, like in type lambdas.

### Wellformed fields

|- gamma \
-------- \
gamma |- -

|- A : Ty \
--------- \
gamma |- +A

|- gamma \
-------- \
gamma |- @

# Consistent    A ~ B
Consistency describes the relationship between two types. It's important to note that these relationships are NOT transitive. Meaning `int ~ *` and `* ~ bool` are two consistent type relationships, but `int ~ bool` is not a consistent type relationship.

## Types are refective
gamma |- A ~ A

Meaning a type is always consistent with itself. Meaning you can always cast a type to itself.

## Types are consistent with dynamic types
gamma |- A ~ *

A type can be upcast to the dynamic type. These casts also always work while evaluating.

gamma |- * ~ A

While downcasts are also consistent, meaning casting a dynamic type to another type is always allowed by the typechecker. In contrast to upcasts which are also unproblematic during evaluation, downcasts can cause blame during the evaluation. This can't be checked by the typechecker though.

## Function consistency
gamma |- A1 ~ A2 \
gamma |- B1 ~ B2 \
-------------------------------- \
gamma |- (A1)->(B1) ~ (A2)->(B2)

Functions in Frho have precisely one input type and one output type. Two functions are consistent iff their inputs and outputs are consistent respectively

## Universal type consistency
gamma |- A1 ~ A2 \
-------------------------------- \
gamma |- all (X:K) A1 ~ all (X:K) A2

Two universal types are consistent, when their typevariable and kind are identical and their associated types are consistent themselves.
This would mean that `all (X:{mylab}) {mylab: +int, X}` and `all (X:{mylab}) *` are consistent.

QPoly(A2) \
X not in A2 \
gamma |- A1 ~ A2 \
-------------------------------- \
gamma |- all (X:K) A1 ~ A2

Universal types can also be cast to other types that behave similar. Similarly behaving types are called quasi-univeral. The QPoly function checks for that.
For instance `all (X: Ty) (int)->({label1: +bool, label2: +X})` can be cast to `(int)->({label1: +bool, label2: +*})`.

QPoly(A1) \
X not in A1 \
gamma |- A1 ~ A2 \
-------------------------------- \
gamma |- A1 ~ all (X:K) A2

Similarly, a quasi-universal type can be cast to a universal type.

## Record and Variant consistency
~{} ({} stands for the empty set) ~L stands for the set of labels L. None of the labels L are allowed to be in the current row!

gamma |- r1 ~{} r2 \
-------------------- \
gamma |- {r1} ~ {r2}

gamma |- r1 ~{} r2 \
-------------------- \
gamma |- [r1] ~ [r2]

### Wellformed rows
gamma |- r : L \
----------------------- \
gamma |- r ~L r

### Dynamic Rows
gamma |- r : L \
----------------------- \
gamma |- r ~L *

gamma |- r : L \
----------------------- \
gamma |- * ~L r

gamma |- O1 ~ O2 \
gamma |- r1 ~(L union {l}) r2 \
l not in L \
----------------------- \
gamma |- label: O1;r1 ~L label: O2;r2

### Consistency for fields
gamma |- - ~ -

gamma |- A ~ B \
-------------- \
gamma +A ~ +B

gamma |- @ ~ O

gamma |- O ~ @

# Convertible A \<phi B

## Static cases
sigma |- * \<phi *

sigma |- X \<phi X

sigma |- alpha \<+beta alpha

sigma |- alpha \<-beta alpha

isBasetype(A)
-------------- \
sigma |- A \<phi A

This doesn't mean that base types can't be sealed. It simply means that if we apply a convertion to a base type, we can get the same base type out again.

## Sealing and unsealing

sigma |- alpha \<+alpha sigma(alpha)

sigma |- A \<-alpha alpha sigma'[alpha = A] 

## Functions

sigma |- A2 \<not(phi) A1 \
sigma |- B1 \<phi B2 \
------------------------------------ \
sigma |- (A1)->(B1) \<phi (A2)->(B2)

## Univeral types
sigma |- A1 \<phi A2 \
---------------------- \
sigma |- all (X:K) A1 \<phi all(X:K) A2

## Records and variants
sigma |- r1 \<phi r2 \
-------------------- \
sigma |- {r1} \<phi {r2}

sigma |- r1 \<phi r2 \
-------------------- \
sigma |- [r1] \<phi [r2]

sigma |- O1 \<phi O2 \
sigma |- r1 \<phi r2 \
-------------------------------------- \
sigma |- label:O1;r1 \<phi label:02;r2

### Occurrence Rules
sigma |- - \<phi -

sigma |- A \<phi B \
-------------------- \
sigma |- +A \<phi +B

sigma |- @ \<phi @

# Type checking

## Variable
|- gamma \
label in gamma \
----------------------------------- \
sigma; gamma |- label --> gamma(label)

This checks whether the requested variable exists and returns its type. Should the requested variable not exists a type check error will be raised.

## Constant
|- gamma \
---------------------------------- \
sigma; gamma |- constant --> typeOf(constant)

Every constant of type `int`, `float`, `bool` and `string` return their type as the type check output.

## Code block

sigma; gamma   |- M1 --> A, sigma', gamma ' \
sigma'; gamma' |- (M2; Ms...) --> B
-------------------------- \
sigma; gamma   |- (M1; M2; Ms...) --> B

sigma; gamma |- M --> A
---------------- \
sigma; gamma |- (M) --> A

Code blocks act combine multiple statements and act as a single statement. While a Code block might alter the state internally in order to keep track of new variables or functions within a code block, a code block does not modify the state of where the code block was used.

## Mathematical & Logical functions
Normal operator binding applies

sigma; gamma |- M --> bool \
sigma; gamma |- N --> bool \
------------- \
sigma; gamma |- M || N --> bool

sigma; gamma |- M --> bool \
sigma; gamma |- N --> bool \
------------- \
sigma; gamma |- M && N --> bool

rho |- M --> bool \
-------------------- \
rho |- !M --> bool

rho |- M --> A \
(A == int) | (A == float) \
-------------------- \
rho |- -M --> A

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B \
------------- \
sigma; gamma |- M == N --> bool

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M > N --> bool

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M < N --> bool

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M >= N --> bool

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M <= N --> bool

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float) | (A == string)) \
------------- \
sigma; gamma |- M + N --> A

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M - N --> A

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M * N --> A

sigma; gamma |- M --> A \
sigma; gamma |- N --> B \
A == B & ((A == int) | (A == float)) \
------------- \
sigma; gamma |- M / N --> A

Frho allows for mathematial operations over ints, floats, and bools. Frho also allows concatinating strings using the + operation.
## Let statement
sigma; gamma |- M --> A \
------------------------------------ \
sigma; gamma |- let label = M --> {}, gamma'[label = A]

The let statement is one of the few statements which alter the type checking context, by inserting the label along with it's associated value into gamma.

## If statement

sigma; gamma |- M_decider --> bool \
sigma; gamma |- M_consequence --> A_consequenceType \
sigma; gamma |- M_alternative --> A_alternativeType \
A_consequenceType == B_alternativeType
--------------------------------------------------- \
sigma; gamma |- if M_decider then M_consequence else M_alternative --> A_consequenceType

A valid if statement has to fulfil two requirements. The decider needs to be a bool, since otherwise it couldn't be determined whether the consequence or alternative should be evaluated during the evaluation phase. Furthermore, both the consequence and alternative need to have the same type, since we can't determine during the type checking phase which of these two code paths will be taken during the later evaluation.

## Function call
sigma; gamma |- M_function --> (A)->(B) \
sigma; gamma |- N_argument --> A \
----------------- \
sigma; gamma |- M_function(N_argument) --> B

## Function
sigma; gamma'[label_argument = A, label_functionName = (A)->(B)] |- M_body --> B 
------------------------------------------------------------------------- \
sigma; gamma |- fun label_functionName (label_argument: A) : B M_body --> {}, gamma'[label_functionName = (A)->(B)]

The function definition is one of the few statements which modify the state of gamma. In order to successfully evaluate M_body to determine the output type of the function type, a variable with the label_argument name and associated type needs to be added.

We have to insert the function with it's type into gamma during the body check, otherwise checking recursive functions would fail.

## Lambda
sigma; gamma'[label_argument = A] |- M_body --> B 
------------------------------------------------------------------------- \
sigma; gamma |- lam (label_argument: A) : B M_body --> (A)->(B)

## Recursive lambda
sigma; gamma |- lam (label_argument: A_input) : A_output (fun label_functionName (label_argument: A_input) : A_output M; label_functionName(label_argument)) --> B \
----------------------------------------- \
sigma; gamma |- rec label_functionName (label_argument: A_input) : A_output M --> B

## Type Lambda
sigma; gamma'[X = K] |- M --> A \
---------------------------------------------------------- \
sigma; gamma |- lam \<X : K> M --> all (X, K) A

## Type Application
sigma; gamma |- M --> all (X, K) B \
gamma |- A : K \
------------------ \ 
sigma; gamma |- app \<A> M --> B[X --> A]

The type application checks whether A doesn't contain any labels defined by K and replaces all occurrances of X in B with A.

## Record Creation
sigma; gamma |- M_i --> A_i   (for 1 <= i <= n) \
----------------------------------------------------------------------- \
sigma; gamma |- { ..., label_i : M_i, ... } --> {..., label_i: +A_i, ...}

During record creation all terms are evaluated and are converted to a record type. Record types created during this record creation only contains present `+` occurrences and is closed, therefore does not contain any typevariable or *.

## Record Update
sigma; gamma |- M --> {r, ?} \
sigma; gamma |- N --> A \
----------------------------- \
sigma; gamma |- M.label = N --> {(r.update(label, +A)), ?}

A record update can either change the type of an existing label or add a new label and occurrance to the type of the record.

## Record Selection
sigma; gamma |- M --> {label: +A;r, ?} \
------------------------- \
sigma; gamma |- M.label --> A

## Variant Creation
sigma; gamma |- M --> A \
-------------------------------------- \
sigma; gamma |- [ label : M ] --> [label: +A]

## Variant Case
sigma; gamma |- M_variant --> [r, ?] \
sigma; gamma[label_variable = getType([r, ?], label)] |- M_consequence --> consequenceType \
sigma; gamma[label_alternative_variable = [(r.update(label, -)), ?]] |- M_alternative --> alternativeType \
consequenceType == alternativeType \
------------------------------------------------------------------------------------------------- \
sigma; gamma |- case M_variant with ([variant_label: label_variable ] -> M_consequence; label_alternative_variable -> M_alternative) --> consequenceType

## Cast
sigma; gamma |- M --> A \
gamma |- B: Ty \
gamma |- A ~ B \
---------------------------------------- \
gamma |- M : A =[p]=> B  --> B

Casts check whether the two types between M is cast are consistent. It's important to note that these cast can later fail during evaluation, since consistency isn't transitive so some casts which are valid during type checking can cause blame during evaluation.

## Convertions
sigma; gamma |- M --> A \
gamma |- B: Ty \
gamma |- A <p B \
---------------------------------------- \
sigma; gamma |- M : A ~[p]~> B  --> B

Opposed to casts, convertions don't fail during evaluation, when they got type checked previously.

