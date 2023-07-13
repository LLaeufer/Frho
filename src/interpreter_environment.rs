
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::cell::RefCell;

pub type Var = String;
pub type Label = String;

pub type EvaluationResult = Value;


// https://stackoverflow.com/questions/31373255/how-do-i-share-a-mutable-object-between-threads-using-arc
#[derive(Clone, PartialEq, Debug)]
pub struct EnvironmentInternal {
    variables: HashMap<Var, Value>,
    types: HashMap<Var, Type>,
    parent: Option<Arc<RefCell<EnvironmentInternal>>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Environment {
    internal: Arc<RefCell<EnvironmentInternal>>,
}

macro_rules! env_behavior_helper {
    ($kind:ident, $behavior_name:ident, $get:ident, $insert:ident, $keys:ident, $output:ident) => {
        pub trait $behavior_name {
            fn $get(&self, var: &Var) -> Option<$output>;
            fn $insert(&mut self, var: Var, value: $output) -> bool;
            fn $keys(&self) -> HashSet<Var>; 
        }

        impl $behavior_name for EnvironmentInternal {

            fn $get(&self, var: &Var) -> Option<$output> {
                match self.$kind.get(var) {
                    Some(val) => Some(val.clone()),
                    None => match &self.parent {
                        Some(parent) => {
                            let let_parent = parent.borrow();
                            let_parent.$get(var)
                        },
                        None => None
                    }
                }
            }

            fn $insert(&mut self, var: Var, value: $output) -> bool {
                if self.$kind.contains_key(&var) { return false; }
                self.$kind.insert(var, value);
                true
            }

            fn $keys(&self) -> HashSet<Var> {
                match &self.parent {
                    Some(parent) => {
                            let let_parent = parent.borrow();
                            let_parent.$keys().union(&self.$kind.keys().cloned().collect()).cloned().collect()
                    },
                    None =>  self.$kind.keys().cloned().collect(),
                }
            }
        }

        impl $behavior_name for Environment {

            fn $get(&self, var: &Var) -> Option<$output> {
                self.internal.borrow().$get(var)
            }

            fn $insert(&mut self, var: Var, value: $output) -> bool {
                self.internal.borrow_mut().$insert(var, value)
            }

            fn $keys(&self) -> HashSet<Var> {
                self.internal.borrow().$keys()
            }
        }
    };
}

env_behavior_helper!(variables, EnvironmentInternalBehaviorVariables, get, insert, keys, Value);
env_behavior_helper!(types, EnvironmentInternalBehaviorTypes, get_type, insert_type, type_keys, Type);

pub trait EnvironmentBehavior {
    fn new() -> Self;
    fn new_child(&self) -> Self;
    fn new_child_with(&self, with: HashMap<Var, Value>) -> Self;
}

impl EnvironmentBehavior for Environment {
    fn new() -> Self {
        Environment { internal: Arc::new(RefCell::new(EnvironmentInternal {variables: HashMap::new(), types: HashMap::new(), parent: None})) }
    }

    fn new_child(&self) -> Self {
        self.new_child_with(HashMap::new())
    }

    fn new_child_with(&self, with: HashMap<Var, Value>) -> Self {
        Environment { internal: Arc::new(RefCell::new(EnvironmentInternal { variables: with, types: HashMap::new(), parent: Some(self.internal.clone()) })) }
    }
}

type LabelOccurrence = (Label, FieldOccurrence);

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
    RecordsType(Vec<LabelOccurrence>),
    VariantType(Vec<LabelOccurrence>),
    FunctionType(Vec<Type>, Vec<Type>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum FieldOccurrence {
    Absent,
    Present(Type)
}
pub type TermBlock = Arc<Vec<Term>>;

pub type RawVariant = (Label, Box<Term>);

pub type Variant = (Label, Value);

#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    Constant(Value),
    Variable(Var),
    DefineTypeVariable(Var, Type),
    LogicGate(LogicTerm),

    Block(TermBlock),

    Let(Label, Box<Term>),

    // Condition Concequence Alternative
    If(Box<Term>, Box<Term>, Box<Term>),

    // Function Label Parameters Functionbody
    Function(Label, Vec<Var>, Box<Term>),

    AnonymousFunction(Vec<Var>, Box<Term>),

    // PolymorphicValue,

    // ApplyType(Type, Box<Term>),

    // Function to call, Parameter values
    FunctionCall(Box<Term>, Vec<Term>),

    TypeApplication(Box<Term>, Type),

    RecordConstruction(Vec<RawVariant>),

    RecordUpdate(Box<Term>, Box<Term>, Box<Term>),

    RecordSelection(Box<Term>, Box<Term>),

    VariantConstruction(RawVariant),

    // Lets implement this as a logic gate then we can use it simply within a if statement
    // Box<Term>: The variant we want to check
    // Label: the label we want to check is in the variant
    // Var1: Should be the label in the variant we set the var to the value of the label
    // Box<Term>1: We execute this with var1 applied
    // Var2: if the label should not be in the variant, we take the label of the variant and set it to the value of the original label
    // Box<Term>2: What we execute if variant and label are incompatible


    // Das ist quasi ein switch case, bei dem bei einem match das ganze verarbeitet wird und bei keinem match weitergereicht wird
    VariantCase(Box<Term>, Label, Var, Box<Term>, Var, Box<Term>),

    BigLambda(Type, Value),
} 

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

    VFunc(Box<Environment>, Vec<Var>, Box<Term>),
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
        todo!()
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

#[derive(PartialEq, Debug, Clone)]
pub enum InvalidValueTranslation {
    Invalid(),
    Unimplemented(),
}

macro_rules! generic_value_function_template {
    ($value_type_int:ident, $value_type_float:ident, $func_name:ident, $logic_function:tt) => {
        fn $func_name(&self, second: Value) -> Result<Value, InvalidValueTranslation> {
            match self {
                Value::VInt(i1) => match second {
                    Value::VInt(i2) => Ok(Value::$value_type_int(*i1 $logic_function i2)),
                    _ => Err(InvalidValueTranslation::Invalid()),
                },
                Value::VFloat(f1) => match second {
                    Value::VFloat(f2) => Ok(Value::$value_type_float(*f1 $logic_function f2)),
                    _ => Err(InvalidValueTranslation::Invalid()),
                },
                _ => Err(InvalidValueTranslation::Invalid()),
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

    math_function_template!(add, +);
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
