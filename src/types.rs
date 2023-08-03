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
    FunctionType(Vec<Type>, Vec<Type>),
    UnknownType,
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
                                Some(snd_occurence) => return occurence == *snd_occurence,
                                None => return false,
                            }
                        },
                    }
                }

                false
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
                                Some(snd_occurence) => return occurence == *snd_occurence,
                                None => return false,
                            }
                        },
                    }
                }

                false
            },
            _ => false,
        },
        _ => type_1 == type_2,
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
            Type::VariantType(lab_occ_vec) => write!(f, "VARIANT {{{}}}", serialize_label_occurrence_vector(lab_occ_vec)),
            Type::FunctionType(in_types, out_types) => write!(f, "FUNCTION ({}) ({})", serialize_type_vector(in_types), serialize_type_vector(out_types)),
            Type::UnknownType => write!(f, "UNKNOWN"),
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