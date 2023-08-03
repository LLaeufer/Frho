use crate::types::*;
use crate::utils::*;
use crate::values::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::cell::RefCell;
use std::fmt::{self};

pub type EvaluationResult = Value;


// https://stackoverflow.com/questions/31373255/how-do-i-share-a-mutable-object-between-threads-using-arc
#[derive(Clone, PartialEq, Debug)]
pub struct EnvironmentInternal {
    variables: HashMap<Var, Value>,
    // types: HashMap<Var, Type>,
    parent: Option<Arc<RefCell<EnvironmentInternal>>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Environment {
    internal: Arc<RefCell<EnvironmentInternal>>,
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut ret = "Environment { Current Layer: (".to_string();
        let mut fst = true;
        for key in self.keys_at_level() {
            if fst {fst = false} else {ret += ","}
            ret += &key;
            ret += ": ";
            let tmp = match self.get(&key) {
                Some(value) => serialize_value(&value, false),
                None => "".to_string(),
            };
            ret += string_to_str(&tmp);
        }

        ret += ") Parent: (";
        let tmp = match self.get_parent() {
            Some(parent) => format!("{}", parent),
            None => "".to_string(),
        };
        ret += string_to_str(&tmp);
        ret += ") }";

        write!(f, "{}", ret)

    }
}


// Only use these for the thread in interpreter.rs otherwise this could do scary things
unsafe impl Send for Environment {}
unsafe impl Sync for Environment {}

macro_rules! env_behavior_helper {
    ($ext_target:ident, $int_target:ident, $kind:ident, $behavior_name:ident, $get:ident, $insert:ident, $remove:ident, $remove_at_level:ident, $keys_at_level:ident, $keys:ident, $output:ident) => {
        pub trait $behavior_name {
            fn $get(&self, var: &Var) -> Option<$output>;
            fn $insert(&mut self, var: Var, value: $output) -> bool;
            fn $remove(&mut self, var: &Var);
            // Removes value from current env level
            fn $remove_at_level(&mut self, var: &Var);
            fn $keys_at_level(&self) -> HashSet<Var>; 
            fn $keys(&self) -> HashSet<Var>; 
        }

        impl $behavior_name for $int_target {

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

            fn $remove(&mut self, var: &Var) {
                match self.$kind.remove(var) {
                    Some(_val) => {},
                    None => match &self.parent {
                        Some(parent) => {
                            let mut mut_parent = parent.borrow_mut();
                            mut_parent.$remove(var);
                        },
                        None => {},
                    },
                }
            }

            fn $remove_at_level(&mut self, var: &Var) {
                self.$kind.remove(var);
            }

            fn $keys_at_level(&self) -> HashSet<Var> {
                self.$kind.keys().cloned().collect()
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

        impl $behavior_name for $ext_target {

            fn $get(&self, var: &Var) -> Option<$output> {
                self.internal.borrow().$get(var)
            }

            fn $insert(&mut self, var: Var, value: $output) -> bool {
                self.internal.borrow_mut().$insert(var, value)
            }

            fn $remove_at_level(&mut self, var: &Var) {
                self.internal.borrow_mut().$remove_at_level(var)
            }

            fn $keys_at_level(&self) -> HashSet<Var> {
                self.internal.borrow().$keys_at_level()
            }

            fn $keys(&self) -> HashSet<Var> {
                self.internal.borrow().$keys()
            }

            fn $remove(&mut self, var: &Var) {
                self.internal.borrow_mut().$remove(var)
            }
        }
    };
}

env_behavior_helper!(Environment, EnvironmentInternal, variables, EnvironmentInternalBehaviorVariables, get, insert, remove, remove_at_level, keys_at_level, keys,Value);
// env_behavior_helper!(types, EnvironmentInternalBehaviorTypes, get_type, insert_type, type_keys, Type);

macro_rules! env_parent_control {
    ($parent_control:ident, $parent_identity:ident) => {
        pub trait $parent_control {
            fn set_parent_if_empty(&mut self, parent: Self) -> bool;
            fn set_parent(&mut self, parent: Self);
            fn remove_parent(&mut self);
            fn get_parent(&self) -> Option<$parent_identity>;
        
        }
        impl $parent_control for $parent_identity {

            fn set_parent_if_empty(&mut self, parent: Self) -> bool {
                let mut internal_self = self.internal.borrow_mut();
                match &internal_self.parent {
                    Some(_) => false,
                    None => {internal_self.parent = Some(parent.internal.clone()); true}
                }
            }
        
            fn set_parent(&mut self, parent: Self) {
                let mut internal_self = self.internal.borrow_mut();
                internal_self.parent = Some(parent.internal.clone());
            }
        
            fn remove_parent(&mut self) {
                let mut internal_self = self.internal.borrow_mut();
                internal_self.parent = None;
            }
        
            fn get_parent(&self) -> Option<$parent_identity> {
                Some($parent_identity { internal: self.internal.borrow().parent.clone()? })
            }
        }
    };
}

env_parent_control!(EnvironmentParentControl, Environment);

pub trait EnvironmentBehavior {
    fn new() -> Self;
    fn new_child(&self) -> Self;
    fn new_child_with(&self, with: HashMap<Var, Value>) -> Self;
    fn cleanup(&mut self); // Do not use this function inside the evaluation, this is just for cleanup afterwards
}

impl EnvironmentBehavior for Environment {
    fn new() -> Self {
        Environment { internal: Arc::new(RefCell::new(EnvironmentInternal {variables: HashMap::new(), parent: None})) }
    }

    fn new_child(&self) -> Self {
        self.new_child_with(HashMap::new())
    }

    fn new_child_with(&self, with: HashMap<Var, Value>) -> Self {
        Environment { internal: Arc::new(RefCell::new(EnvironmentInternal { variables: with, parent: Some(self.internal.clone()) })) }
    }

    fn cleanup(&mut self) {
        while !self.keys().is_empty() {
            for key in self.keys() {
                // println!("Removing: {}", &key);
                match self.get(&key) {
                    Some(mut value) => {self.remove(&key); value.deep_clean()},
                    None => {}
                }
                self.remove(&key)
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeEnvironmentInternal {
    variables: HashMap<Var, Type>,
    type_variables: HashMap<Var, Type>,
    parent: Option<Arc<RefCell<TypeEnvironmentInternal>>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeEnvironment {
    internal: Arc<RefCell<TypeEnvironmentInternal>>,
}

env_behavior_helper!(TypeEnvironment, TypeEnvironmentInternal, variables, TypeEnvironmentInternalBehaviorVariables, get, insert, remove, remove_at_level, keys_at_level, keys, Type);
env_behavior_helper!(TypeEnvironment, TypeEnvironmentInternal, type_variables, TypeEnvironmentInternalBehaviorTypeVariables, type_get, type_insert, type_remove, type_remove_at_level, type_keys_at_level, type_keys, Type);
env_parent_control!(TypeEnvironmentParentControl, TypeEnvironment);

pub trait TypeEnvironmentBehavior {
    fn new() -> Self;
    fn new_child(&self) -> Self;
    fn new_child_with(&self, with: HashMap<Var, Type>, with_type_vars: HashMap<Var, Type>) -> Self;
}

impl TypeEnvironmentBehavior for TypeEnvironment {
    fn new() -> Self {
        TypeEnvironment { internal: Arc::new(RefCell::new(TypeEnvironmentInternal {variables: HashMap::new(), type_variables: HashMap::new(), parent: None})) }
    }

    fn new_child(&self) -> Self {
        self.new_child_with(HashMap::new(), HashMap::new())
    }

    fn new_child_with(&self, with: HashMap<Var, Type>, with_type_vars: HashMap<Var, Type>) -> Self {
        TypeEnvironment { internal: Arc::new(RefCell::new(TypeEnvironmentInternal { variables: with, type_variables: with_type_vars, parent: Some(self.internal.clone()) })) }
    }
}