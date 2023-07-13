use std::str::FromStr;

use crate::interpreter_environment::*;
use crate::interpreter_utils::*;

pub fn label_or_bool(s: String) -> Value {
    if s == "true".to_string() {
        return Value::VBool(true);
    } else if s == "false".to_string() {
        return Value::VBool(false);
    }
    Value::VLabel(s.to_string())
}


pub fn match_bool(s: String) -> Value {
    if s == "TRUE".to_string() {
        return Value::VBool(true);
    }
    Value::VBool(false)

}
//pub fn record_from_elements(s: Vec<(String, Value)>) -> Value {
//    Value::VRecord(tuple_to_map(s))
//}

macro_rules! merge_helper {
    ($name:ident, $types:ident) => {
        pub fn $name(e: $types, es: Vec<$types> ) -> Vec<$types> {
            let mut ev = vec![e.clone()];
            let mut esm = es.clone();
            ev.append(&mut esm);
            ev.clone()
        }
    };
}

type str_val = (String, Value);

merge_helper!(merge_elements, str_val);
merge_helper!(merge_labels, String);
merge_helper!(merge_terms, Term);

/*pub fn merge_elements(e: (String, Value), es: Vec<(String, Value)> ) -> Vec<(String, Value)> {
    let mut ev = vec![e.clone()];
    let mut esm = es.clone();
    ev.append(&mut esm);
    ev.clone()
}

pub fn merge_labels(e: String, es: Vec<String> ) -> Vec<String> {
    let mut ev = vec![e.clone()];
    let mut esm = es.clone();
    ev.append(&mut esm);
    ev.clone()
}*/

pub fn element_to_variant(e: (String, Value)) -> Value {
    let (lab, val) = e;
    Value::VVariant(lab.clone(), Box::new(val.clone()))
}

pub fn remove_quotes(s: String) -> String {
    let mut s = s;
    s.pop();
    if s.len() > 0 { s.remove(0); }
    s
}