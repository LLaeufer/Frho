use crate::utils::*;
use std::collections::HashMap;
use std::fmt::{self};

pub type LabelOccurrence = (Label, FieldOccurrence);

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    TypeVariable(Var),
    NoneType,
    IntType,
    BoolType,
    FloatType,
    StringType,
    LabelType,
    // AllType(Box<Type>), //Universal Type
    AllType, //Universal Type
    TypeContainerType(Var, Box<Type>),
    RecordsType(Vec<LabelOccurrence>),
    VariantType(Vec<LabelOccurrence>),
    FunctionType(Vec<Type>, Box<Type>),
    UnknownType,
    YetUnknownRecursiveType, // Used if the typechecker isn't aware of the correct type for a recursive function yet
    IllegalType,
}

impl FrhoEqual for Type {
    fn equal(&self, second: &Self) -> bool {
        match self {
            Type::RecordsType(_occurances) => equal_internal(self, second) && equal_internal(second, self),
            Type::VariantType(_occurances) => equal_internal(self, second) && equal_internal(second, self),
            _ => self == second,
        }
    }
}

fn equal_internal(type_1: &Type, type_2: &Type) -> bool {
    match type_1 {
        Type::RecordsType(occurances) => match type_2 {
            Type::RecordsType(second_occurances) => {
                let occurances_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(occurances.clone());
                let second_occurances_map:  HashMap<String, FieldOccurrence> = HashMap::from_iter(second_occurances.clone());
                for (key, occurence) in occurances_map {
                    match occurence.clone() {
                        FieldOccurrence::Absent => { /* Nothing todo here */ },
                        FieldOccurrence::Present(_typ) => {
                            let second_occurence = second_occurances_map.get(&key);
                            match second_occurence {
                                Some(snd_occurence) => if occurence != *snd_occurence {return false},
                                None => return false,
                            }
                        },
                    }
                }

                true
            },
            _ => false,
        },
        Type::VariantType(occurances) => match type_2 {
            Type::VariantType(second_occurances) => {
                let occurances_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(occurances.clone());
                let second_occurances_map:  HashMap<String, FieldOccurrence> = HashMap::from_iter(second_occurances.clone());
                for (key, occurence) in occurances_map {
                    match occurence.clone() {
                        FieldOccurrence::Absent => { /* Nothing todo here */ },
                        FieldOccurrence::Present(_typ) => {
                            let second_occurence = second_occurances_map.get(&key);
                            match second_occurence {
                                Some(snd_occurence) => if occurence != *snd_occurence {return false},
                                None => {},
                            }
                        },
                    }
                }

                true
            },
            _ => false,
        },
        _ => type_1 == type_2,
    }
}

pub trait OfType {
    fn is_variant(&self) -> bool;
    fn is_record(&self) -> bool;
    fn is_function(&self) -> bool;
}

impl OfType for Type {
    fn is_variant(&self) -> bool {
        match self {
            Type::VariantType(_) => true,
            _ => false,
        }
    }

    fn is_record(&self) -> bool {
        match self {
            Type::RecordsType(_) => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            Type::FunctionType(_, _) => true,
            _ => false,
        }
    }
}

pub trait TypeUtils {
    fn get_function_input(&self) -> &Vec<Type>;
    fn get_function_output(&self) -> &Type;
}

impl TypeUtils for Type {
    fn get_function_input(&self) -> &Vec<Type> {
        match self {
            Type::FunctionType(input, _) => input,
            _ => panic!("{} is not a function type", self),
        }
    }

    fn get_function_output(&self) -> &Type {
        match self {
            Type::FunctionType(_, output) => output,
            _ => panic!("{} is not a function type", self),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::TypeVariable(var) => write!(f, "VAR (${})", var),
            Type::NoneType => write!(f, "NONE"),
            Type::IntType => write!(f, "INT"),
            Type::BoolType => write!(f, "LABEL"),
            Type::FloatType => write!(f, "FLOAT"),
            Type::StringType => write!(f, "STRING"),
            Type::LabelType => write!(f, "LABEL"),
            Type::AllType => write!(f, "ALL"),
            Type::TypeContainerType(var, typ) => write!(f, "TYPECONTAINER {}: {}", var, typ),
            Type::RecordsType(lab_occ_vec) => write!(f, "RECORD {{{}}}", serialize_label_occurrence_vector(lab_occ_vec)),
            Type::VariantType(lab_occ_vec) => write!(f, "VARIANT [{}]", serialize_label_occurrence_vector(lab_occ_vec)),
            Type::FunctionType(in_types, out_types) => write!(f, "FUNCTION ({}) ({})", serialize_type_vector(in_types), out_types),
            Type::UnknownType => write!(f, "UNKNOWN"),
            Type::YetUnknownRecursiveType => write!(f, "YETUNKNOWNRECURSIVETYPE"),
            Type::IllegalType => write!(f, "ILLEGAL"),
        }
    }
}
crate::vector_serializer!(serialize_type_vector, Type, ",");
crate::tuple_pair_serializer!(serialize_label_occurrence, LabelOccurrence, ": ");
crate::vector_serializer_foreign!(serialize_label_occurrence_vector, LabelOccurrence, ",", serialize_label_occurrence);


#[derive(Clone, PartialEq, Debug)]
pub enum FieldOccurrence {
    Absent,
    Present(Type)
}

impl fmt::Display for FieldOccurrence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FieldOccurrence::Absent => write!(f, "-"),
            FieldOccurrence::Present(typ) => write!(f, "+{}", typ),
        }
    }
}