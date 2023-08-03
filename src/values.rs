use crate::environment::*;
use crate::terms::*;
use crate::types::*;
use crate::utils::*;
use std::collections::HashMap;
use std::fmt::{self};

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    VNone,
    VInt(i32),
    VBool(bool),
    VFloat(f32),
    VString(String),
    VLabel(String),

    VAll(Box<Value>),
    VRecord(HashMap<Label, Value>),
    VVariant(Label, Box<Value>),

    VFunc(Box<Environment>, Vec<VariantType>, Box<Term>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", serialize_value(self, true))
    }
}

pub fn serialize_value(value: &Value, long_func: bool) -> String{
    match value {
        Value::VNone => format!("VNone"),
        Value::VInt(i) => format!("VInt({})", i),
        Value::VBool(b) => format!("VBool({})", b),
        Value::VFloat(fl) => format!("VFloat({})", fl),
        Value::VString(s) => format!("VString({})", s),
        Value::VLabel(l) => format!("VLabel({})", l),
        Value::VAll(a) => format!("VAll({})", serialize_value(a, long_func)),
        Value::VRecord(m) => format!("VRecord({})", serialize_record_content(m, long_func)),
        Value::VVariant(l, v) => format!("VVariant({}, {})", l, serialize_value(v, long_func)),
        Value::VFunc(e, p, b) => if long_func {format!("VFunc({}, {:?}, {:?} )", e, p, b)} else { "VFunc".to_string() },
    }

}

fn serialize_record_content(map: &HashMap<Label, Value>, long_func: bool) -> String {
    let mut ret = "{".to_string();
    let mut fst = true;
    for (label, value) in map {
        if fst {fst = false} else {ret += ","}
        ret += label;
        ret += ":";
        ret += string_to_str(&serialize_value(value, long_func));
    }
    ret += "}";
    ret
}


pub trait ValueCleanup {
    fn clean(&mut self);
    fn should_clean(&self) -> bool;
    fn deep_clean(&mut self);
}

impl ValueCleanup for Value {
    fn clean(&mut self) {
        match self {
            Value::VAll(inner) => inner.clean(),
            Value::VRecord(map) => {
                for (_, inner) in map {
                    inner.clean();
                }
            },
            Value::VVariant(_, inner) => inner.clean(),
            Value::VFunc(env, _, _) => env.remove_parent(),
            _ => {},
        }
    }

    fn should_clean(&self) -> bool {
        match self {
            Value::VAll(inner) => inner.should_clean(),
            Value::VRecord(map) => {
                for (_, inner) in map {
                    if !inner.should_clean() {return false}
                }
                true
            },
            Value::VVariant(_, inner) => inner.should_clean(),
            Value::VFunc(_, _, _) => false,
            _ => true,
        }
    }

    fn deep_clean(&mut self) {
        // println!("Cleaning {}", self);
        match self {
            Value::VAll(inner) => inner.deep_clean(),
            Value::VRecord(map) => {
                for (_, inner) in map {
                    inner.deep_clean();
                }
            },
            Value::VVariant(_, inner) => inner.deep_clean(),
            Value::VFunc(env, _, _) => env.cleanup(),
            _ => {},
        }
    }

}

pub trait ValueTypes {
    fn of_type(&self, comparison: &Type) -> bool;
    fn get_type(&self) -> Type;
    fn cast_type(&self, types: &Type) -> Result<Value, ValueCastError>;
}

#[derive(PartialEq, Debug, Clone)]
pub enum ValueCastError {
    NotCastable(Value, Type),
}

macro_rules! value_types_matcher {
    ($comparison:ident, $types:ident) => {
        match $comparison {
            Type::$types => true,
            _ => false
        }
    };
}

impl ValueTypes for Value {
    fn of_type(&self, comparison: &Type) -> bool {
        match self {
            Value::VNone => value_types_matcher!(comparison, NoneType),
            Value::VInt(_) => value_types_matcher!(comparison, IntType),
            Value::VBool(_) => value_types_matcher!(comparison, BoolType),
            Value::VFloat(_) => value_types_matcher!(comparison, FloatType),
            Value::VString(_) => value_types_matcher!(comparison, StringType),
            Value::VLabel(_) => value_types_matcher!(comparison, LabelType),
            Value::VAll(_) => value_types_matcher!(comparison, AllType),
            Value::VRecord(_) => todo!(),
            Value::VVariant(_, _) => todo!(),
            Value::VFunc(_, _, _) => todo!(),
        }
    }

    fn get_type(&self) -> Type {
        match self {
            Value::VNone => Type::NoneType,
            Value::VInt(_) => Type::IntType,
            Value::VBool(_) => Type::BoolType,
            Value::VFloat(_) => Type::FloatType,
            Value::VString(_) => Type::StringType,
            Value::VLabel(_) => Type::LabelType,
            Value::VAll(_) => Type::AllType,
            _ => Type::UnknownType
        }
    }

    fn cast_type(&self, types: &Type) -> Result<Value, ValueCastError> {
        match self {
            Value::VAll(actual_value) => {
                if self.of_type(types) {
                    return Ok(self.clone());
                } else {
                    return actual_value.cast_type(types);
                }
            },
            _ => {
                if self.of_type(types) {
                    return Ok(self.clone());
                } else {return Err(ValueCastError::NotCastable(self.clone(), types.clone()))}

            }
        }

    }
}