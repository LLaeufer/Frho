use crate::values::*;
use crate::utils::*;
use crate::logicterms::*;
use crate::types::*;
use std::sync::Arc;
use std::fmt::{self};

pub type TermBlock = Arc<Vec<Term>>;

fn iterate_term_block(data: TermBlock) -> impl Iterator<Item = Term> {
    let len = data.len();
    (0..len).map(move |i| data[i].clone())
}

pub type RawVariant = (Label, Box<Term>);
pub type VariantType = (Var, Type);

#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    Constant(Value),
    Variable(Var),
    LogicGate(LogicTerm),

    Block(TermBlock),

    // Variable-name, code-body that results in a value
    Let(Label, Box<Term>),

    // Condition Consequence Alternative
    If(Box<Term>, Box<Term>, Box<Term>),

    // Function Label Parameters Function-body
    Function(Label, Option<Type>, Vec<VariantType>, Box<Term>),

    AnonymousFunction(Vec<VariantType>, Box<Term>),

    // Label is only used for self-recursion
    RecursiveAnonymousFunction(Label, Option<Type>, Vec<VariantType>, Box<Term>),

    // Function to call, Parameter values
    FunctionCall(Box<Term>, Vec<Term>),

    TypeApplication(Box<Term>, Label),

    RecordConstruction(Vec<RawVariant>),

    RecordUpdate(Box<Term>, Label, Box<Term>),

    RecordSelection(Box<Term>, Label),

    VariantConstruction(RawVariant),

    // Lets implement this as a logic gate then we can use it simply within a if statement
    // Box<Term>: The variant we want to check
    // Label: the label we want to check is in the variant
    // Var1: Should be the label in the variant we set the var to the value of the label
    // Box<Term>1: We execute this with var1 applied
    // Var2: if the label should not be in the variant, we take the label of the variant and set it to the value of the original label
    // Box<Term>2: What we execute if variant and label are incompatible


    // Simply a switch case, that can be nested
    VariantCase(Box<Term>, Label, Var, Box<Term>, Var, Box<Term>),

    BigLambda(Label, Type, Box<Term>),

    Promise(Type, Box<Term>),
    Print(Box<Term>),
} 

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Constant(value) => write!(f, "{}", serialize_value_for_term(value)),
            Term::Variable(var) => write!(f, "{}", var),
            Term::LogicGate(logic_term) => write!(f, "{}", logic_term),
            Term::Block(lines) => {
                let mut ret = "".to_string();
                let mut fst = true;
                for line in iterate_term_block(lines.clone()) {
                    if fst {fst = false} else {ret += "; "}
                    ret += string_to_str(&format!("{}", line));
                }
                write!(f, "({})", ret)
            },
            Term::Let(label, term) => write!(f, "let {} = {}", label, format!("{}", term)),
            Term::If(decider, consequence, alterative) => write!(f, "if {} then {} else {}", decider, consequence, alterative),
            Term::Function(label, mby_type, parameter, body) => match mby_type {
                Some(typ) => write!(f, "fun {} ({}) : {} {}", label, serialize_variant_type_vector(parameter), typ, body),
                None => write!(f, "fun {} ({}) {}", label, serialize_variant_type_vector(parameter), body),
            },
            Term::AnonymousFunction(parameter, body) => write!(f, "lam ({}) {}", serialize_variant_type_vector(parameter), body),
            Term::RecursiveAnonymousFunction(label, mby_type, parameter, body) => match mby_type {
                Some(typ) => write!(f, "rec {} ({}) : {} {}", label, serialize_variant_type_vector(parameter), typ, body),
                None => write!(f, "rec {} ({}) {}", label, serialize_variant_type_vector(parameter), body),
            },
            Term::FunctionCall(term, parameter) => write!(f, "{} ({})", term, serialize_term_vector(parameter)),
            Term::TypeApplication(term, label) => write!(f, "unwrap <${}> {}", label, term),
            Term::RecordConstruction(variant_vec) => write!(f, "{{{}}}", serialize_raw_variant_vector(variant_vec)),
            Term::RecordUpdate(record, label, value) => write!(f, "{}.{} <- {}", record, label.clone(), value),
            Term::RecordSelection(record, label) => write!(f, "{}.{}", record, label.clone()),
            Term::VariantConstruction(variant) => write!(f, "[{}]", serialize_raw_variant(variant)),
            Term::VariantCase(term, label, var, consequence , alt_var, alternative) => write!(f, "case {} with ([{}: {}] -> {}; {} -> {})", term, label, var, consequence, alt_var, alternative),
            Term::BigLambda(type_var, typ, term) => write!(f, "wrap <${} : {}> {}", type_var, typ, term),
            Term::Promise(types, block) => write!(f, "promise <{}> {}", types, block),
            Term::Print(term) => write!(f, "print {}", term),
        }

    }
}

crate::tuple_pair_serializer!(serialize_raw_variant, RawVariant, ": ");
crate::vector_serializer_foreign!(serialize_raw_variant_vector, RawVariant, ", ", serialize_raw_variant);
crate::tuple_pair_serializer!(serialize_variant_type, VariantType, ": ");
crate::vector_serializer_foreign!(serialize_variant_type_vector, VariantType, ", ", serialize_variant_type);
crate::vector_serializer!(serialize_term_vector, Term, ", ");

fn serialize_value_for_term(value: &Value) -> String{
    match value {
        Value::VNone => format!("none"),
        Value::VInt(i) => format!("{}", i),
        Value::VBool(b) => if *b {"true".to_string()} else {"false".to_string()},
        Value::VFloat(fl) => {
            let mut float_string = format!("{}", fl);
            if !float_string.contains(".") {float_string += ".0"}
            float_string
        },
        Value::VString(s) => format!("\"{}\"", s),
        Value::VLabel(l) => format!("${}", l),
        _ => "".to_string(),
    }

}