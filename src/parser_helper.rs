use crate::types::*;
use crate::values::*;
use crate::terms::*;

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

type StrTerm = (String, Box<Term>);

merge_helper!(merge_elements, StrTerm);
merge_helper!(merge_labels, String);
merge_helper!(merge_terms, Term);
merge_helper!(merge_types, Type);
merge_helper!(merge_occurences, LabelOccurrence);
merge_helper!(merge_variant_types, VariantType);

pub fn element_to_variant(e: (String, Box<Term>)) -> Term {
    Term::VariantConstruction(e)
}

pub fn remove_quotes(s: String) -> String {
    let mut s = s;
    s.pop();
    if s.len() > 0 { s.remove(0); }
    s
}