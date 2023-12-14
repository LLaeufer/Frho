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
    Function(Label, Var, Type, Type, Box<Term>),

    AnonymousFunction(Var, Type, Type, Box<Term>),

    // Label is only used for self-recursion
    RecursiveAnonymousFunction(Label, Var, Type, Type, Box<Term>),

    // Function to call, Parameter value
    FunctionCall(Box<Term>, Box<Term>),

    TypeApplication(Box<Term>, Type),

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

    BigLambda(Label, Kind, Box<Term>),



    Cast(Box<Term>, Type, BlameLabel, Type),
    Convertion(Box<Term>, Type, ConvertionLabel, Type),
    Blame(BlameLabel),



    // Private definitions

    // Make typechecker ignore a subterm (for debugging)
    Promise(Type, Box<Term>),

    // Print a subterm
    Print(Box<Term>),

    ConstantType(Type),

    
} 

pub trait ReplaceType {
    fn replace_type(&self, target: &Type, new: &Type) -> Self;
}

impl ReplaceType for Term {
    fn replace_type(&self, target: &Type, new: &Type) -> Self {
        match self {
            // Term::Constant(target) => &Term::Constant(target),
            // Term::Variable(_) => todo!(),
            Term::LogicGate(lt) => Term::LogicGate(lt.replace_type(target, new)),
            Term::Block(terms) => {
                let mut new_terms = vec![];
                for term in terms.iter() {
                    new_terms.push(term.replace_type(target, new))
                }
                Term::Block(Arc::new(new_terms))
            },
            Term::Let(var, term) => Term::Let(var.clone(), Box::new(term.replace_type(target, new))),
            Term::If(decider, consequence, alternative) => Term::If(Box::new(decider.replace_type(target, new)), Box::new(consequence.replace_type(target, new)), Box::new(alternative.replace_type(target, new))),
            Term::Function(label, parameter, parameter_type, output_type, term) => Term::Function(label.clone() , parameter.clone() , parameter_type.replace_target(target, new), output_type.replace_target(target, new), Box::new(term.replace_type(target, new))),
            Term::AnonymousFunction(parameter, parameter_type, output_type, term) => Term::AnonymousFunction(parameter.clone() , parameter_type.replace_target(target, new), output_type.replace_target(target, new), Box::new(term.replace_type(target, new))),
            Term::RecursiveAnonymousFunction(label, parameter, parameter_type, output_type, term) => Term::RecursiveAnonymousFunction(label.clone() , parameter.clone() , parameter_type.replace_target(target, new), output_type.replace_target(target, new), Box::new(term.replace_type(target, new))),
            Term::FunctionCall(fun, term) => Term::FunctionCall(Box::new(fun.replace_type(target, new)), Box::new(term.replace_type(target, new))),
            Term::TypeApplication(term, ta_type) => Term::TypeApplication(Box::new(term.replace_type(target, new)), ta_type.replace_target(target, new)),
            Term::RecordConstruction(label_terms) => {
                let mut new_label_terms = vec![];
                for (label, term) in label_terms.iter() {
                    new_label_terms.push((label.clone(), Box::new(term.replace_type(target, new))))
                }
                Term::RecordConstruction(new_label_terms)
            },
            Term::RecordUpdate(record, label, term) => Term::RecordUpdate(Box::new(record.replace_type(target, new)), label.clone(), Box::new(term.replace_type(target, new))),
            Term::RecordSelection(record, label) => Term::RecordSelection(Box::new(record.replace_type(target, new)), label.clone()),
            Term::VariantConstruction((label, term)) => Term::VariantConstruction((label.clone(), Box::new(term.replace_type(target, new)))),
            Term::VariantCase(decider, label, con_label, con, alt_label, alt) => Term::VariantCase(Box::new(decider.replace_type(target, new)), label.clone(), con_label.clone(), Box::new(con.replace_type(target, new)), alt_label.clone(), Box::new(alt.replace_type(target, new))),
            Term::BigLambda(typevar, kind, term) => if target == &Type::TypeVariable(typevar.clone()) {self.clone()} else {Term::BigLambda(typevar.clone(), kind.clone(), Box::new(term.replace_type(target, new)))},
            Term::Cast(term, type1, cobl, type2) => Term::Cast(Box::new(term.replace_type(target, new)), type1.replace_target(target, new), cobl.clone(), type2.replace_target(target, new)),
            Term::Convertion(term, type1, cobl, type2) => Term::Convertion(Box::new(term.replace_type(target, new)), type1.replace_target(target, new), cobl.clone(), type2.replace_target(target, new)),
            // Term::Blame(_) => todo!(),
            // Term::Promise(_, _) => todo!(),
            Term::Print(term) => Term::Print(Box::new(term.replace_type(target, new))),
            // Term::ConstantType(_) => todo!(),
            _ => self.clone()
        }
    }
}

pub type ConvertionLabel = ConvertionOrBlameLabel;
pub type BlameLabel = ConvertionOrBlameLabel;

#[derive(Clone, PartialEq, Debug)]
pub enum ConvertionOrBlameLabel {
    Absent(Label),
    Present(Label)
}

pub trait ConvertionOrBlameLabelTrait {
    fn to_label(&self) -> Label;
    fn is_present(&self) -> bool;
    fn invert(&self) -> Self;
}

impl ConvertionOrBlameLabelTrait for ConvertionOrBlameLabel {
    fn to_label(&self) -> Label {
        match self {
            ConvertionOrBlameLabel::Absent(lab) => lab.clone(),
            ConvertionOrBlameLabel::Present(lab) => lab.clone(),
        }
    }

    fn is_present(&self) -> bool {
        match self {
            ConvertionOrBlameLabel::Absent(_) => false,
            ConvertionOrBlameLabel::Present(_) => true,
        }
    }

    fn invert(&self) -> Self {
        match self {
            ConvertionOrBlameLabel::Absent(lab) => ConvertionOrBlameLabel::Present(lab.clone()),
            ConvertionOrBlameLabel::Present(lab) => ConvertionOrBlameLabel::Absent(lab.clone()),
        }
    }
}

impl fmt::Display for ConvertionOrBlameLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", if self.is_present() {"+".to_string()} else {"-".to_string()}, self.to_label())
    }
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
            Term::Function(label, parameter, parameter_type, mby_type, body) => 
                write!(f, "{}", write_func(&"fun".to_string(), label, &vec![(parameter.clone(), parameter_type.clone())], mby_type, body)),
            Term::AnonymousFunction(parameter, parameter_type, mby_type, body) =>
                write!(f, "{}", write_func(&"lam".to_string(), &"".to_string(), &vec![(parameter.clone(), parameter_type.clone())], mby_type, body)),
            Term::RecursiveAnonymousFunction(label, parameter, parameter_type,mby_type,  body) =>
                write!(f, "{}", write_func(&"rec".to_string(), label, &vec![(parameter.clone(), parameter_type.clone())], mby_type, body)),
            Term::FunctionCall(term, parameter) => write!(f, "{}{}", term, parameter),
            Term::TypeApplication(term, label) => write!(f, "app {} <{}>", term, label),
            Term::RecordConstruction(variant_vec) => write!(f, "{{{}}}", serialize_raw_variant_vector(variant_vec)),
            Term::RecordUpdate(record, label, value) => write!(f, "{}.{} <- {}", record, label.clone(), value),
            Term::RecordSelection(record, label) => write!(f, "{}.{}", record, label.clone()),
            Term::VariantConstruction(variant) => write!(f, "[{}]", serialize_raw_variant(variant)),
            Term::VariantCase(term, label, var, consequence , alt_var, alternative) => write!(f, "case {} with ([{}: {}] -> {}; {} -> {})", term, label, var, consequence, alt_var, alternative),
            Term::BigLambda(type_var, typ, term) => write!(f, "lam <{} : {}> {}", type_var, typ, term),
            Term::Cast(term, from, blame, to) => write!(f, "{} : {} =[{}]=> {}", term, from, blame, to),
            Term::Convertion(term, from, convertion_label, to) => write!(f, "{} : {} ~[{}]~> {}", term, from, convertion_label, to),
            Term::Blame(label) => write!(f, "blame {}", label),
            Term::Promise(types, block) => write!(f, "promise <{}> {}", types, block),
            Term::Print(term) => write!(f, "print {}", term),
            _ => write!(f, "Unknown: {:?}", self),
        }

    }
}

fn write_func(termkind: &String, label: &Label, parameters: &Vec<(Var, Type)>, output_type: &Type, body: &Term) -> String  {
    format!("{} {}({}) : {} {}", termkind, label, serialize_variant_type_vector(parameters), output_type, body)
}

crate::tuple_pair_serializer!(serialize_raw_variant, RawVariant, ": ");
crate::vector_serializer_foreign!(serialize_raw_variant_vector, RawVariant, ", ", serialize_raw_variant);
crate::tuple_pair_serializer!(serialize_variant_type, VariantType, ": ");
crate::vector_serializer_foreign!(serialize_variant_type_vector, VariantType, ", ", serialize_variant_type);
crate::vector_serializer!(serialize_term_vector, Term, ", ");

fn serialize_value_for_term(value: &Value) -> String{
    match value {
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