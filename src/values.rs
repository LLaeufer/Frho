use crate::environment::*;
use crate::terms::*;
use crate::types::*;
use crate::utils::*;
use std::collections::HashMap;
use std::fmt::{self};

pub fn unit_value() -> Value {
    Value::VRecord(HashMap::new())
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    VInt(i32),
    VBool(bool),
    VFloat(f32),
    VString(String),
    VLabel(String),

    VAll(Var, Kind, Box<Value>),
    VRecord(HashMap<Label, Value>),
    VVariant(Label, Box<Value>),

    VFunc(Box<Environment>, Var, Box<Term>, Type),

    VDynamic(Box<Value>, GroundType, BlameLabel), 
    VCast(Box<Value>, Type, BlameLabel, Type),
    VSeal(Box<Value>, Type, TypeName),
    VSealPostConv(Box<Value>, Type, ConvertionLabel, Type),
    VSealRecord(Box<Value>, Type, TypeName, Vec<Label>),
    VSealVariant(Box<Value>, Type, TypeName, Vec<Label>),

}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", serialize_value(self, true))
    }
}

pub fn serialize_value(value: &Value, long_func: bool) -> String{
    match value {
        Value::VInt(i) => format!("VInt({})", i),
        Value::VBool(b) => format!("VBool({})", b),
        Value::VFloat(fl) => format!("VFloat({})", fl),
        Value::VString(s) => format!("VString({})", s),
        Value::VLabel(l) => format!("VLabel({})", l),
        Value::VAll(typevar, kind, value) => if long_func {format!("VAll({}, {}, {})", typevar, kind, value)} else { "VAll".to_string() },
        Value::VRecord(m) => format!("VRecord({})", serialize_record_content(m, long_func)),
        Value::VVariant(l, v) => format!("VVariant({}, {})", l, serialize_value(v, long_func)),
        Value::VFunc(e, p, b, t) => if long_func {format!("VFunc({}, {:?}, {:?}, {} )", e, p, b, t)} else { "VFunc".to_string() },
        Value::VDynamic(v, t, b) => format!("VDynamic ({}, {}, {})", serialize_value(v, long_func), t, b),
        Value::VCast(v, t, b, t2) => format!("VCast ({}, {}, {}, {})", serialize_value(v, long_func), t, b, t2),
        Value::VSeal(v, t, b) => format!("VSeal ({}, {}, {})", v, t, b),
        Value::VSealPostConv(v, i, c, o) => format!("VSealPostConv ({}, {}, {}, {})", serialize_value(v, long_func), i, c, o),
        Value::VSealRecord(v, t, b, labels) => format!("VSealRecord ({}, {}, {}. {})", serialize_value(v, long_func), t, b, serialize_label_vector(labels)),
        Value::VSealVariant(v, t, b, labels) => format!("VSealVariant ({}, {}, {}. {})", serialize_value(v, long_func), t, b, serialize_label_vector(labels)),
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
            Value::VRecord(map) => {
                for (_, inner) in map {
                    inner.clean();
                }
            },
            Value::VVariant(_, inner) => inner.clean(),
            Value::VFunc(env, _, _, _) => env.remove_parent(),
            Value::VDynamic(sub, _, _) => sub.clean(),
            Value::VSeal(sub, _, _) => sub.clean(),
            Value::VSealPostConv(sub, _, _, _) => sub.clean(),
            _ => {},
        }
    }

    fn should_clean(&self) -> bool {
        match self {
            Value::VRecord(map) => {
                for (_, inner) in map {
                    if !inner.should_clean() {return false}
                }
                true
            },
            Value::VVariant(_, inner) => inner.should_clean(),
            Value::VFunc(_, _, _, _) => false,
            Value::VDynamic(sub, _, _) => sub.should_clean(),
            Value::VSeal(sub, _, _) => sub.should_clean(),
            Value::VSealPostConv(sub, _, _, _) => sub.should_clean(),
            _ => true,
        }
    }

    fn deep_clean(&mut self) {
        match self {
            Value::VRecord(map) => {
                for (_, inner) in map {
                    inner.deep_clean();
                }
            },
            Value::VVariant(_, inner) => inner.deep_clean(),
            Value::VFunc(env, _, _, _) => env.cleanup(),
            Value::VDynamic(sub, _, _) => sub.deep_clean(),
            Value::VSeal(sub, _, _) => sub.deep_clean(),
            Value::VSealPostConv(sub, _, _, _) => sub.deep_clean(),
            _ => {},
        }
    }

}

pub trait ValueTypes {
    fn of_type(&self, comparison: &Type) -> bool;
    fn get_type(&self) -> Type;
    fn try_replace_type(&self, new_type: &Type) -> Option<Value>;
}

#[derive(PartialEq, Debug, Clone)]
pub enum ValueCastError {
    NotCastable(Value, Type),
}

macro_rules! value_types_matcher {
    ($comparison:ident, $types:pat) => {
        match $comparison {
            $types => true,
            _ => false
        }
    };
}

impl ValueTypes for Value {
    fn of_type(&self, comparison: &Type) -> bool {
        match self {
            Value::VInt(_) => value_types_matcher!(comparison, Type::BaseType(BaseType::IntType)),
            Value::VBool(_) => value_types_matcher!(comparison, Type::BaseType(BaseType::BoolType)),
            Value::VFloat(_) => value_types_matcher!(comparison, Type::BaseType(BaseType::FloatType)),
            Value::VString(_) => value_types_matcher!(comparison, Type::BaseType(BaseType::StringType)),
            // Value::VLabel(_) => value_types_matcher!(comparison, LabelType),
            // Value::VAll(var, kind, _, types) => match comparison {
            Value::VAll(var, kind, subval) => match comparison {
                Type::UniversalType(tvar, tkind, ttypes) => var == tvar && kind == tkind && subval.get_type() == **ttypes,
                _ => false,
            }
            _ => todo!(),
        }
    }

    fn get_type(&self) -> Type {
        match self {
            Value::VInt(_) => Type::BaseType(BaseType::IntType),
            Value::VBool(_) => Type::BaseType(BaseType::BoolType),
            Value::VFloat(_) => Type::BaseType(BaseType::FloatType),
            Value::VString(_) => Type::BaseType(BaseType::StringType),
            // Value::VAll(var, kind, _, types) => Type::UniversalType(var.clone(), kind.clone(), Box::new(types.clone())),
            Value::VAll(var, kind, subvalue) => Type::UniversalType(var.clone(), kind.clone(), Box::new(subvalue.get_type())),
            Value::VLabel(__) => todo!(),
            Value::VRecord(map) => Type::RecordsType(var_value_map_to_occ_vector(map), RecordAndVariantEnd::Closed).normalize(),
            Value::VVariant(label, variant_type) => Type::VariantType(vec![(label.clone(), FieldOccurrence::Present(variant_type.get_type()))], RecordAndVariantEnd::Closed),
            Value::VFunc(_, _, _, t) => t.clone(),
            Value::VDynamic(_, _, _) => Type::DynType,

            Value::VCast(_subvalue, _from, _blame, to) => to.clone(),
            Value::VSeal(_, _typ, typename) => Type::TypeName(typename.clone()),
            Value::VSealPostConv(_, _, _, post) => post.clone(),
            Value::VSealRecord(_, _typ, typename, _labels) => Type::TypeName(typename.clone()),
            Value::VSealVariant(_, _typ, typename, _labels) => Type::TypeName(typename.clone()),
        }
    }

    fn try_replace_type(&self, new_type: &Type) -> Option<Value>{
        match self {
            Value::VAll(_var, _kind, value) => match new_type {
                Type::UniversalType(var, kind, subtype) => match value.try_replace_type(&subtype) {
                    Some(value) => Some(Value::VAll(var.clone(), kind.clone(), Box::new(value))),
                    None => None
                },
                _ => None,
            },
            Value::VFunc(env, label, term, _funtype) => Some(Value::VFunc(env.clone(), label.clone(), term.clone(), new_type.clone())),

            _ => None
        }
    }
}

fn var_value_map_to_occ_vector(map: &HashMap<String, Value>) -> Vec<LabelOccurrence> {
    Vec::from_iter(map.iter().map(|x| (x.0.clone(), FieldOccurrence::Present(x.1.get_type()))))

}

pub fn unwrap_dynamics(val1: Value, val2: Value) -> (Value, Value, Option<GroundType>, Option<BlameLabel>) {
    let (sub1, gt1, b1) = match val1 {
        Value::VDynamic(sub, gt, b) => (*sub, Some(gt), Some(b)),
        _ => (val1, None, None),
    };
    let (sub2, gt2, b2) = match val2 {
        Value::VDynamic(sub, gt, b) => (*sub, Some(gt), Some(b)),
        _ => (val2, None, None),
    };

    if gt2.is_none() || gt1.as_ref().is_some_and(|x| x.is_base_type_and(&BaseType::FloatType)){
        (sub1, sub2, gt1, b1)
    } else {
        (sub1, sub2, gt2, b2)
    }
}