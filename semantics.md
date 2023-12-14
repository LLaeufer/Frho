# Big-step Semantics

## Variable
Evaluates to the value saved under this variable.
rho |- label --> rho(label)

## Constant
typeOf(constant) == int \
---------------------------------- \
rho |- constant --> vInt(constant)

typeOf(constant) == bool \
---------------------------------- \
rho |- constant --> vBool(constant)

typeOf(constant) == float \
---------------------------------- \
rho |- constant --> vFloat(constant)

typeOf(constant) == string \
---------------------------------- \
rho |- constant --> vString(constant)

## Code block
A code block is a set of instruction acting as a single instruction. A code block will not modify it's environment, but will temporarily extend it's environment.

rho  |- M1 --> v, rho' \
rho' |- (M2; Ms...) --> w
-------------------------- \
rho  |- (M1; M2; Ms...) --> w

rho |- M --> v
---------------- \
rho |- (M) --> v

## Mathematical & Logical functions
Normal operator binding applies

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M || N --> vBool(v || w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M && N --> vBool(v && w)

rho |- M --> v \
-------------------- \
rho |- !M --> getBSVKind(v)(!v)

rho |- M --> v \
-------------------- \
rho |- -M --> getBSVKind(v)(-v)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M == N --> vBool(v == w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M > N --> vBool(v > w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M < N --> vBool(v < w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M >= N --> vBool(v >= w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M <= N --> vBool(v <= w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M + N --> getBSVKind(v)(v + w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M - N --> getBSVKind(v)(v - w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M * N --> getBSVKind(v)(v * w)

rho |- M --> v \
rho |- N --> w \
------------- \
rho |- M / N --> getBSVKind(v)(v / w)


## Let statement
Let saves the output of a term to a variable. One a variable has been set, it can't be overwritten in the same context.
This statement is one of the few statements which alter the evaluation state rho.

rho |- M --> v \
------------------------------------ \
rho |- let label = M --> {}, rho'[label = v]

## If statement
The decider needs to evaluate to a boolean value. If the decider evaluates to true consequence will be evaluated, otherwise alternative will be evaluated.

rho |- M_decider --> deciderOutcome \
if deciderOutcome == vBool(true): \
&emsp;rho |- M_consequence --> v \
else: \
&emsp;rho |- M_alternative --> v \
--------------------------------------------------- \
rho |- if M_decider then M_consequence else M_alternative --> v

## Function call
In the simplest case, M_function evaluates to a function value, and N_argument gets evaluated to any value. Then the M_body gets evaluated in the rho' enviroment of the function value, with the value that got evaluated from N_argument inserted as the parameter name. 

rho |- M_function --> vFunc(rho', x', M_body', type') \
rho |- N_argument --> v \
rho'[x' = v] |- M_body' --> w \
----------------- \
rho |- M_function(N_argument) --> w


rho |- M_function --> vCast(u, (A1)->(B1), p, (A2)->(B2)) \
rho |- N_argument --> v \
rho |- (u (v : A2 =[-p]=> A1)) : B1 =[p]=> B2 --> w \
---------------------------------- \
rho |- M_function(N_argument) --> w

## Function
A function definition also alters the state of rho. To ensure consistent function execution function values contain the evaluation state rho, which is then used in the function call to evaluate M_body.

rho |- fun label_functionName (label_argument: A) : B M_body --> {}, rho'[label_functionName = vFunc(rho, label_argument, M_body, (A)->(B))]

## Lambda
Lambdas behave similar to functions, with the exception that they don't modify rho, directly return the function value and aren't recursive since they lack a function name to recursively call themselves.

rho |- lam (label_argument: A) : B M_body --> vFunc(rho, label_argument, M_body, (A)->(B))

## Recursive lambda
Recursive lambdas are lambdas with a function name, so they can recursively call themselves.

rho |- lam (label_argument: A) : B (fun label_functionName (label_argument: A) : B M_body; label_functionName(label_argument)) --> v \
----------------------------------------- \
rho |- rec label_functionName (label_argument: A) : B M_body --> v

## Type Lambda
Terms like functions can have types, these can be set to the typevariable X, the big lambda notation ensures that only types with the right kind can be replaced. With type denoting the resulting type of the term. This type might or might not contain X.

rho |- M --> v
--------------------------------------------- \
rho |- lam \<X : K> M --> vTypeLambda(X, K, v)

## Type Application
The term needs to evaluate to a type lambda value. Replaces the typevariable from the type lambda with the type provided by the type application. Returns the subvalue in the type lambda value.

rho |- M --> vTypeLambda(X, K, v) \
------------------ \ 
rho |- app \<A> M --> v[X -> A]

We need to apply the new type to the value `v` since we need to access the value in:

### Type application with sealed values

rho, sigma |- M --> vAll(X, K, v), sigma' \
rho, sigma'[alpha = B] |-conv v[X -> alpha] : typeOf(v)[X -> alpha] ~[+alpha]~> typeOf(v)[X -> B] --> w \
--------------------------------------------------------------------- alpha fresh \
rho, sigma |- app \<B> M -->  w, sigma'[alpha = B] \

If the v yielded by M contains a substitution from B to alpha, so basically something like vSeal(sub_v, B, alpha) we also convert all X's to alpha, and then convert all the alphas (both the original alphas and the alphas which were previously Xs) back to B's, by lifting the seal.

This procedure can be simplified by simply substituting all vSeal(sub_v, B, alpha) in v with sub_v and then proceeding as normal.

### Type application of a Universal type to universal type
rho |- M --> vCast(v, (all(X, K) A1), p, (all(X, K) A2)) \
rho |- app \<B> (lam \<X:K> (app \<X> v) : A1 =[p]=> A2) --> w \
------------------------------------------------------------- \
rho |- app \<B> M --> w

## Record Creation
rho |- M_i --> v_i   (for 1 <= i <= n) \
----------------------------------------------------------------------- \
rho |- { ..., label_i : M_i, ... } --> vRecord(toMap([..., (label_i, v_i), ...]))

## Record Update
Record evaluates to a record value, label selects the label within the record. This sets the label within the record to the value which term evaluates to.

rho |- M --> vRecord(recordMap) \
rho |- N --> v \
----------------------------- \
rho |- M.label = N --> vRecord(recordMap.insertOrOverwrite(label, v))

rho |- M --> vCast(v, {r1, ?1}, p, {r2, ?2}) \
rho |- v.label = N --> w \
----------------------------- \
rho |- M.label = N --> vCast(w, {l:+A;(r1.removeIfPresent(l)), ?1}, p ,{l:+A;(r2.removeIfPresent(l)), ?2})

## Record Selection
Record evaluates to a record value, label selects the label within the record.
Should both the record and the label exists, this evaluates to the value behind the label in the record. Otherwise, this yields an exception.

rho |- M --> vRecord(recordMap) \
------------------------- \
rho |- M.label --> recordMap(label)

rho |- M --> vCast(v, {l:+A;r1, ?1}, p, {l:+B;r2, ?2}) \
rho |- (v.label) : A =[p]=> B --> w \
-------------------------------------- \
rho |- M.label --> w

rho |- M --> vCast(vCast(v, {l:-;r1, ?1}, p, {l:@;r2, ?2}), {l:@;r2, ?2}, q, {l:+B;r3, ?3}) \
------------------------------------------------------- \
rho |- M.label --> blame q

rho |- M --> vCast(vCast(v, {l:@;r1, ?1}, p, {l:@;r2, ?2}), {l:@;r2, ?2}, q, {l:+B;r3, ?3}) \
rho |- vCast(v, {l:@;r1, ?1}, q, {l:+B, *}).label --> w \
------------------------------------------------------- \
rho |- M.label --> w

rho |- M --> vCast(vCast(v, {l:+A;r1, ?1}, p, {l:@;r2, ?2}), {l:@;r2, ?2}, q, {l:+B;r3, ?3}) \
rho |- ((v.label) :  A =[p]=> *) : * =[q]=> B --> w \
------------------------------------------------------- \
rho |- M.label --> w

## Variant Creation
rho |- M --> v \
-------------------------------------- \
rho |- [ label : M ] --> vVariant(label, v)

## Variant Case
Variant evaluates to a variant value. If the variant value contains the variant_label, the consequence will be evaluated with the variable set to the value associated with the variant_label.
Alternatively the alternative_variable will be set to the variant value and alternative will be evaluated.

rho |- M_variant --> vVariant(label', v) \
if label == variant_label: \
&emsp;rho[label_variable = v] |- M_consequence --> w \
else: \
&emsp;rho[label_alternative_variable -> vVariant(label', v)] |- M_alternative --> w \
------------------------------------------------------------------------------------------------- \
rho |- case M_variant with ([variant_label: label_variable ] -> M_consequence; label_alternative_variable -> M_alternative) --> w

## Cast semantics

To simplify the definition of the cast semantics of Frho we will use an auxiliary function: `rho |-cast v : A =[p]=> B --> v'`

We use an auxiliary function `rho |-cast v : A =[p]=> B  --> v'` ...

rho |- M --> v \
rho |-cast v : A =[p]=> B  --> w \
---------------------------------------- \
rho |- M : A =[p]=> B  --> w

### Dynamic to dynamic
rho |-cast v : * =[p]=> *  --> v

### GroundType to dynamic
rho |-cast v : G =[p]=> *  -->   VDynamic(v, G, p)

### Consistent A to dynamic
A is a type which is neither the dynamic type nor a ground type, but A ~ G. Meaning that A is consistent with G
rho |-cast v : A =[p]=> * --> vDynamic(vCast(v, A, p, G), G, p)

### Dynamic to groundType
rho |-cast vDynamic(v, G, p) : * =[q]=> G --> v

### Dynamic to consistent A
A is a type which is neither the dynamic type nor a ground type, but A ~ G. Meaning that A is consistent with G
rho |-cast v : * =[p]=> G --> w \
------------------------------------------------------------ \
rho |-cast v : * =[p]=> A --> vCast(w, G, p, A) \

### Incompatible groundTypes result in blame
G != H \
------------------------------------------------------------ \
rho |-cast vDynamic(v, G, p) : * =[q]=> H --> Blame q

### Cast to the same baseType
If A is a baseType casts from and to the same type are okay
rho |-cast v : A p A --> v

### Casts from and to the same seal
rho |-cast v : alpha p alpha --> v

### Casts from a universal type to another type
rho |- v <\*> : A[*/x] =[p]=> B --> w \
--------------------------------------------- if QPoly(B) \
rho |-cast v : all (X:K) A =[p]=> B --> w \

### Casts to a universal type
---------------------------------------------------------------------- if QPoly(A) \
rho |-cast v : A =[p]=> all (X:K) B  -->  lam \<X:K> (v : A =[p]=> B)

### Variant Casts
rho |-cast vCast(vVariant(l, v), [l:+A1;r1, ?1], p, [l:+A2;r2, ?2]) --> vVariant(l, (v : A1 =[p]=> A2))

rho |- cast vCast(vCast(vVariant(l, v), [l:+A1;r1, ?1], q, A), A, p, [l:-;r2, ?2]) --> blame p

## Convertions semantics

Similarly to the auxiliary function for casts we will use the auxiliary function `rho |-conv v : A ~[+/-alpha]~> B` for convertions of following type: \
rho |- M --> v \
rho |-conv v : A  ~[+/-alpha]~> B --> w \
---------------------------------------- \
rho |- M : A ~[+/-alpha]~> B --> w

### Sealing of base types
This is in conflict with the Frho definition a shouldn't be a base type here
We can seal any type!


------------------------------------------------------- A is a base type \
rho |-conv v : A ~[-alpha]~> alpha --> vSeal(v, A, alpha)

### Unsealing a sealed type
rho |-conv VSeal(v, A, alpha) : alpha ~[+alpha] A  -->  v

### Dynamic types can't be sealed
rho |-conv v : * ~[+/-alpha]~> *  -->  v

### Unrelated convertion labels don't affect seals
-------------------------------------------------- if alpha != beta \
rho |-conv v : alpha ~[+/-beta]~> alpha --> v

### Unrelated convertion labels don't affect base types
-------------------------------------------------- if A is a base type \
rho |-conv v : A ~[+/-alpha]~> A --> v

### Sealing functions
rho |-conv v : (A1)->(B1) ~[+/-alpha]~> (A2)->(B2) --> lam (x:A2) (v (x : A2 ~[-/+alpha]~> A1) : B1 ~[+/-alpha]~> B2)

### Sealing universal types

rho |-conv v : (all (X:K) A1) ~[+/-alpha]~> (all (X:K) A2) --> lam /<X:K> (app /<X> v : A1 ~[+/-alpha]~> A2)
