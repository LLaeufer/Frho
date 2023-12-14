use crate::environment::*;
use crate::values::*;
use crate::terms::*;
use crate::evaluation::*;
use crate::types::*;

use std::collections::HashSet;
use std::collections::HashMap;

macro_rules! tuple_to_map {
    ($name:ident, $typ:ident) => {
        pub fn $name(tup: Vec<(String, $typ)>) -> HashMap<String, $typ> {
            tup.into_iter().collect()
        } 
    };
}

tuple_to_map!(value_tuple_to_map, Value);
tuple_to_map!(type_tuple_to_map, Type);
tuple_to_map!(field_occ_tuple_to_map, FieldOccurrence);

#[macro_export]
macro_rules! map_to_tuples {
    ($name:ident, $typ:ident) => {
        pub fn $name(map: HashMap<String, $typ>) -> Vec<(String, $typ)> {
            map.into_iter().collect()
        } 
    };
}

crate::map_to_tuples!(field_occ_map_to_tuple, FieldOccurrence);

/*pub fn tuple_to_map(tup: Vec<(Var, Value)>) -> HashMap<Var, Value> {
    tup.into_iter().collect()
}*/

pub fn string_to_str(input: &String) -> &str {
    let slice: &str = &*input;
    slice
}

pub fn assert_equal_evaluation(result: Eval, desired: Eval) {
    match result {
        Ok((env, value)) => match desired {
            Ok((desired_env, desired_value)) => {
                assert!(assert_equal_result(value, desired_value));
                assert!(assert_equal_environments(env, desired_env));
            },
            Err(_) => assert!(false),
        },
        Err(err) => match desired {
            Ok(_) => assert!(false),
            Err(desired_err) => assert!(assert_equal_error(err, desired_err)),
        },
    }
}

pub fn assert_equal_result(result_value: Value, desired_value: Value) -> bool {
    result_value == desired_value
}

pub fn assert_equal_environments(result_env: Environment, desired_env: Environment) -> bool {
    let re_keys = result_env.keys();
    let de_keys = desired_env.keys();
    let diff: HashSet<String> = re_keys.difference(&de_keys).cloned().collect();
    if diff.is_empty() {
        for item in re_keys.iter() {
            if result_env.get(item) != desired_env.get(item) { return false; }
        }
        true
    } else {
        false
    }
}

pub fn assert_equal_error(result_err: EvaluationError, desired_err: EvaluationError) -> bool {
    result_err == desired_err
}

pub fn remove_first_and_last_char_from_string(s: String) -> String {
    let mut s = s;
    s.pop();
    if s.len() > 0 { s.remove(0); }
    s
}

#[macro_export]
macro_rules! vector_serializer {
    ($name:ident, $input_type:ident, $separator:expr) => {
        pub fn $name(terms: &Vec<$input_type>) -> String {
            let mut ret = "".to_string();
            let mut fst = true;
            for term in terms {
                if fst {fst = false} else {ret += $separator}
                ret += &*format!("{}", term);
            }
            ret
        }
    };
}

crate::vector_serializer!(serialize_type_vector, Type, ",");
crate::tuple_pair_serializer!(serialize_label_occurrence, LabelOccurrence, ": ");
crate::vector_serializer!(serialize_label_vector, Label, ",");

#[macro_export]
macro_rules! vector_serializer_foreign {
    ($name:ident, $input_type:ident, $separator:expr, $serializer:ident) => {
        pub fn $name(terms: &Vec<$input_type>) -> String {
            let mut ret = "".to_string();
            let mut fst = true;
            for term in terms {
                if fst {fst = false} else {ret += $separator}
                ret += &*$serializer(term);
            }
            ret
        }
    };
}

crate::vector_serializer_foreign!(serialize_label_occurrence_vector, LabelOccurrence, ",", serialize_label_occurrence);

#[macro_export]
macro_rules! tuple_pair_serializer {
    ($name:ident, $pair:ident, $separator:expr) => {
        pub fn $name(pair: &$pair) -> String {
            let (fst, snd) = pair;
            format!("{}", fst) + $separator + &*format!("{}", snd)
        }
    };
}

pub fn new_types_for_variant_type_vector(vtv: Vec<VariantType>, tv: Vec<Type>) -> Vec<VariantType> {
    let (names, _): (Vec<_>, Vec<_>) = vtv.into_iter().unzip();
    names.into_iter().zip(tv).collect::<Vec<_>>()
}

pub type Var = String;
pub type Label = String;
pub type TypeVar = String;
pub type TypeName = String;