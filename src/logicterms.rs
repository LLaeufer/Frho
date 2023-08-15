use crate::terms::*;
use crate::values::*;
use crate::types::*;
use std::fmt::{self};

#[derive(PartialEq, Debug, Clone)]
pub enum InvalidValueTranslation {
    Invalid(),
    InvalidOperation(String, String, String),
    Unimplemented(),
}

macro_rules! generic_value_function_template_with_passthrough {
    ($value_type_int:ident, $value_type_float:ident, $func_name:ident, $logic_function:tt, $passthrough:ident) => {
        fn $func_name(&self, second: Value) -> Result<Value, InvalidValueTranslation> {
            match self {
                Value::VInt(i1) => match second {
                    Value::VInt(i2) => Ok(Value::$value_type_int(*i1 $logic_function i2)),
                    Value::VFloat(f2) => Ok(Value::$value_type_float((*i1 as f32) $logic_function f2)),
                    _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", self), stringify!($func_name).to_string(), format!("{}", second))),
                },
                Value::VFloat(f1) => match second {
                    Value::VFloat(f2) => Ok(Value::$value_type_float(*f1 $logic_function f2)),
                    Value::VInt(i2) =>  Ok(Value::$value_type_float(*f1 $logic_function (i2 as f32))),
                    _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", self), stringify!($func_name).to_string(), format!("{}", second))),
                },
                _ => $passthrough(self, &second),
            }
        }
    };
}

macro_rules! generic_value_function_template {
    ($value_type_int:ident, $value_type_float:ident, $func_name:ident, $logic_function:tt) => {
        fn $func_name(&self, second: Value) -> Result<Value, InvalidValueTranslation> {
            match self {
                Value::VInt(i1) => match second {
                    Value::VInt(i2) => Ok(Value::$value_type_int(*i1 $logic_function i2)),
                    Value::VFloat(f2) => Ok(Value::$value_type_float((*i1 as f32) $logic_function f2)),
                    _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", self), stringify!($func_name).to_string(), format!("{}", second))),
                },
                Value::VFloat(f1) => match second {
                    Value::VFloat(f2) => Ok(Value::$value_type_float(*f1 $logic_function f2)),
                    Value::VInt(i2) =>  Ok(Value::$value_type_float(*f1 $logic_function (i2 as f32))),
                    _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", self), stringify!($func_name).to_string(), format!("{}", second))),
                },
                _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", self), stringify!($func_name).to_string(), format!("{}", second))),
            }
        }
    };
}

macro_rules! math_function_template {
    ($func_name:ident, $logic_function:tt) => {
        generic_value_function_template!(VInt, VFloat, $func_name, $logic_function);
    };
}

macro_rules! logic_function_template {
    ($func_name:ident, $logic_function:tt) => {
        generic_value_function_template!(VBool, VBool, $func_name, $logic_function);
    };
}

macro_rules! value_logic_functions_helper {
    ($x:ident) => {
        fn $x(&self, second: Value) -> Result<Value, InvalidValueTranslation>;  
    };
    ($x:ident, $($y:ident),+) => (
        fn $x(&self, second: Value) -> Result<Value, InvalidValueTranslation>;
        value_logic_functions_helper!($($y),+);
    )
}

fn string_handler(first: &Value, second: &Value) -> Result<Value, InvalidValueTranslation> {
    match first {
        Value::VString(f_str) => match second {
            Value::VString(s_str) => {
                let combined = f_str.clone() + &s_str;
                Ok(Value::VString(combined))
            },
            _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", first), "add".to_string(), format!("{}", second))),
        },
        _ => Err(InvalidValueTranslation::InvalidOperation(format!("{}", first), "add".to_string(), format!("{}", second))),
    }
}

pub trait ValueLogicFunctions {
    fn negate(&self) -> Result<Value, InvalidValueTranslation>;
    value_logic_functions_helper!(and, or, add, sub, mul, div, eql, grt, lst, gre, lse);
}

impl ValueLogicFunctions for Value {
    fn negate(&self) -> Result<Value, InvalidValueTranslation> {
        match self {
            Value::VInt(i) => Ok(Self::VInt(-i)),
            Value::VBool(b) => Ok(Self::VBool(!b)),
            Value::VFloat(f) => Ok(Self::VFloat(-f)),
            _ => Err(InvalidValueTranslation::Invalid()),
        }
    }

    fn and(&self, second: Value) -> Result<Value, InvalidValueTranslation> {
        match self {
            Value::VBool(b1) => match second {
                Value::VBool(b2) => Ok(Value::VBool(*b1 && b2)),
                _ => Err(InvalidValueTranslation::Invalid()),
            },
            _ => Err(InvalidValueTranslation::Invalid()),
        }
    }

    fn or(&self, second: Value) -> Result<Value, InvalidValueTranslation> {
        match self {
            Value::VBool(b1) => match second {
                Value::VBool(b2) => Ok(Value::VBool(*b1 || b2)),
                _ => Err(InvalidValueTranslation::Invalid()),
            },
            _ => Err(InvalidValueTranslation::Invalid()),
        }
    }

    // math_function_template!(add, +);
    generic_value_function_template_with_passthrough!(VInt, VFloat, add, + , string_handler);
    math_function_template!(sub, -);
    math_function_template!(mul, *);
    math_function_template!(div, /);

    fn eql(&self, second: Value) -> Result<Value, InvalidValueTranslation> {
        Ok(Value::VBool(*self == second))
    }

    logic_function_template!(grt, >);
    logic_function_template!(lst, <);
    logic_function_template!(gre, >=);
    logic_function_template!(lse, <=);

    
}

#[derive(Clone, PartialEq, Debug)]
pub enum LogicTerm {
    Not(Box<Term>),
    And(Box<Term>, Box<Term>),
    Or(Box<Term>, Box<Term>),
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
    Div(Box<Term>, Box<Term>),
    Eql(Box<Term>, Box<Term>),
    GrT(Box<Term>, Box<Term>),
    LsT(Box<Term>, Box<Term>),
    GrE(Box<Term>, Box<Term>),
    LsE(Box<Term>, Box<Term>),
}

pub trait LogicTermUtils {
    fn fst(&self) -> &Box<Term>;
    fn snd(&self) -> &Box<Term>;
    fn results_in_bool(&self) -> bool;
    fn resulting_type(&self, first: &Type, second: &Type) -> Type;
    fn legal_types(&self, first: &Type, second: &Type) -> bool;
    fn compatible_operation(&self, typ : &Type) -> bool;
    fn operation_name(&self) -> String;
    fn is_binary_operation(&self) -> bool;
}

impl LogicTermUtils for LogicTerm {
    fn fst(&self) -> &Box<Term> {
        match self {
            LogicTerm::Not(f) => f,
            LogicTerm::And(f, _) => f,
            LogicTerm::Or(f, _) => f,
            LogicTerm::Add(f, _) => f,
            LogicTerm::Sub(f, _) => f,
            LogicTerm::Mul(f, _) => f,
            LogicTerm::Div(f, _) => f,
            LogicTerm::Eql(f, _) => f,
            LogicTerm::GrT(f, _) => f,
            LogicTerm::LsT(f, _) => f,
            LogicTerm::GrE(f, _) => f,
            LogicTerm::LsE(f, _) => f,
        }
    }

    fn snd(&self) -> &Box<Term> {
        match self {
            LogicTerm::Not(s) => s,
            LogicTerm::And(_, s) => s,
            LogicTerm::Or(_, s) => s,
            LogicTerm::Add(_, s) => s,
            LogicTerm::Sub(_, s) => s,
            LogicTerm::Mul(_, s) => s,
            LogicTerm::Div(_, s) => s,
            LogicTerm::Eql(_, s) => s,
            LogicTerm::GrT(_, s) => s,
            LogicTerm::LsT(_, s) => s,
            LogicTerm::GrE(_, s) => s,
            LogicTerm::LsE(_, s) => s,
        }
    }

    fn results_in_bool(&self) -> bool {
        match self {
            LogicTerm::Add(_, _) => false,
            LogicTerm::Sub(_, _) => false,
            LogicTerm::Mul(_, _) => false,
            LogicTerm::Div(_, _) => false,
            _ => true,
        }
    }

    fn resulting_type(&self, first: &Type, second: &Type) -> Type {
        macro_rules! math_type_helper {
            ($first:ident, $second:ident) => {
                match $first {
                    Type::IntType => match $second {
                        Type::IntType => Type::IntType,
                        Type::FloatType => Type::FloatType,
                        Type::YetUnknownRecursiveType => Type::YetUnknownRecursiveType,
                        _ => Type::IllegalType,
                    },
                    Type::FloatType => match $second {
                        Type::IntType => Type::FloatType,
                        Type::FloatType => Type::FloatType,
                        Type::YetUnknownRecursiveType => Type::FloatType,
                        _ => Type::IllegalType,
                    },
                    Type::YetUnknownRecursiveType => match $second {
                        Type::IntType => Type::YetUnknownRecursiveType,
                        Type::FloatType => Type::FloatType,
                        _ => Type::YetUnknownRecursiveType,
                    },
                    _ => Type::IllegalType
                }
            };
        }

        macro_rules! comp_type_helper {
            ($first:ident, $second:ident) => {
                match $first {
                    Type::IntType => match $second {
                        Type::IntType => Type::BoolType,
                        Type::FloatType => Type::BoolType,
                        _ => Type::IllegalType,
                    },
                    Type::FloatType => match $second {
                        Type::IntType => Type::BoolType,
                        Type::FloatType => Type::BoolType,
                        _ => Type::IllegalType,
                    },
                    Type::YetUnknownRecursiveType => Type::BoolType,
                    _ => Type::IllegalType
                }
            };
        }

        match self {
            LogicTerm::Not(_) => match first {
                Type::IntType => Type::IntType,
                Type::BoolType => Type::BoolType,
                Type::FloatType => Type::FloatType,
                Type::YetUnknownRecursiveType => Type::YetUnknownRecursiveType,
                _ => Type::IllegalType,
            },
            LogicTerm::And(_, _) => if (first == second && first == &Type::BoolType) || (first == &Type::YetUnknownRecursiveType) || (second == &              Type::YetUnknownRecursiveType) {return Type::BoolType} else {return Type::IllegalType},
            LogicTerm::Or(_, _) =>  if (first == second && first == &Type::BoolType) || (first == &Type::YetUnknownRecursiveType) || (second == &           Type::YetUnknownRecursiveType) {return Type::BoolType} else {return Type::IllegalType},
            LogicTerm::Add(_, _) => match first {
                Type::IntType => match second {
                    Type::IntType => Type::IntType,
                    Type::FloatType => Type::FloatType,
                    Type::YetUnknownRecursiveType => Type::YetUnknownRecursiveType,
                    _ => Type::IllegalType,
                },
                Type::FloatType => match second {
                    Type::IntType => Type::FloatType,
                    Type::FloatType => Type::FloatType,
                    Type::YetUnknownRecursiveType => Type::FloatType,
                    _ => Type::IllegalType,
                },
                Type::YetUnknownRecursiveType => match second {
                    Type::IntType => Type::YetUnknownRecursiveType,
                    Type::FloatType => Type::FloatType,
                    _ => Type::YetUnknownRecursiveType,
                },
                Type::StringType => if second == &Type::StringType || second == &Type::YetUnknownRecursiveType {return Type::StringType} else {return Type::IllegalType},
                _ => Type::IllegalType
            },
            LogicTerm::Sub(_, _) => math_type_helper!(first, second),
            LogicTerm::Mul(_, _) => math_type_helper!(first, second),
            LogicTerm::Div(_, _) => math_type_helper!(first, second),
            LogicTerm::Eql(_, _) => Type::BoolType,
            LogicTerm::GrT(_, _) => comp_type_helper!(first, second),
            LogicTerm::LsT(_, _) => comp_type_helper!(first, second),
            LogicTerm::GrE(_, _) => comp_type_helper!(first, second),
            LogicTerm::LsE(_, _) => comp_type_helper!(first, second),
        }
    }

    fn legal_types(&self, first: &Type, second: &Type) -> bool {
        if self.resulting_type(first, second) == Type::IllegalType {false} else {true}
    }

    fn compatible_operation(&self, typ: &Type) -> bool {
        match typ {
            Type::IntType => match self {
                LogicTerm::And(_, _) => false,
                LogicTerm::Or(_, _) => false,
                _ => true,
            },
            Type::BoolType => match self {
                LogicTerm::Not(_) => true,
                LogicTerm::And(_, _) => true,
                LogicTerm::Or(_, _) => true,
                LogicTerm::Eql(_, _) => true,
                _ => false,
            },
            Type::FloatType => match self {
                LogicTerm::And(_, _) => false,
                LogicTerm::Or(_, _) => false,
                _ => true,
            },
            Type::StringType => match self {
                LogicTerm::Eql(_, _) => true,
                LogicTerm::Add(_, _) => true,
                _ => false,
            }
            Type::YetUnknownRecursiveType => true,
            _ => match self {
                LogicTerm::Eql(_, _) => true,
                _ => false,
            },
        }
    }

    fn operation_name(&self) -> String {
        match self {
            LogicTerm::Not(_) => "Not".to_string(),
            LogicTerm::And(_, _) => "And".to_string(),
            LogicTerm::Or(_, _) => "Or".to_string(),
            LogicTerm::Add(_, _) => "Add".to_string(),
            LogicTerm::Sub(_, _) => "Sub".to_string(),
            LogicTerm::Mul(_, _) => "Mul".to_string(),
            LogicTerm::Div(_, _) => "Div".to_string(),
            LogicTerm::Eql(_, _) => "Eql".to_string(),
            LogicTerm::GrT(_, _) => "GrT".to_string(),
            LogicTerm::LsT(_, _) => "LsT".to_string(),
            LogicTerm::GrE(_, _) => "GrE".to_string(),
            LogicTerm::LsE(_, _) => "LsE".to_string(),
        }
    }

    fn is_binary_operation(&self) -> bool {
        match self {
            LogicTerm::Not(_) => false,
            _ => true,
        }
    }

}


impl fmt::Display for LogicTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicTerm::Not(term) => write!(f, "!{}", term),
            LogicTerm::And(a, b) => write!(f, "{} && {}", a, b),
            LogicTerm::Or(a, b) => write!(f, "{} || {}", a, b),
            LogicTerm::Add(a, b) => write!(f, "{} + {}", a, b),
            LogicTerm::Sub(a, b) => write!(f, "{} - {}", a, b),
            LogicTerm::Mul(a, b) => write!(f, "{} * {}", a, b),
            LogicTerm::Div(a, b) => write!(f, "{} / {}", a, b),
            LogicTerm::Eql(a, b) => write!(f, "{} == {}", a, b),
            LogicTerm::GrT(a, b) => write!(f, "{} > {}", a, b),
            LogicTerm::LsT(a, b) => write!(f, "{} < {}", a, b),
            LogicTerm::GrE(a, b) => write!(f, "{} >= {}", a, b),
            LogicTerm::LsE(a, b) => write!(f, "{} <= {}", a, b),
        }
    }
}
