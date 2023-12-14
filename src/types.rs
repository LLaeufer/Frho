use crate::terms::*;
use crate::utils::*;
use std::collections::HashSet;
use std::fmt;

pub type LabelOccurrence = (Label, FieldOccurrence);

pub fn unit() -> Type {
    Type::RecordsType(vec![], RecordAndVariantEnd::Closed)
}


#[derive(Clone, PartialEq, Debug)]
pub enum Kind {
    Ty,
    Labels(Vec<Label>)
}

pub fn compatible_type_app_kinds(first: &Kind, second: &Kind) -> bool {
    match first {
        Kind::Ty => if second == &Kind::Ty {true} else {false},
        Kind::Labels(labels) => match second {
            Kind::Ty => false,
            Kind::Labels(second_labels) => {
                let l_set : HashSet<String> = labels.clone().into_iter().collect();
                let sl_set : HashSet<String> = second_labels.clone().into_iter().collect();
                let intersection_set : HashSet<String> = l_set.intersection(&sl_set).cloned().collect::<HashSet<_>>();
                intersection_set.len() == 0
            },
        },
    }
}

pub trait EquivalenceUtils {
    fn normalize(&self) -> Self;
    fn equal(&self, second: &Self) -> bool;
}

impl EquivalenceUtils for Kind {
    fn normalize(&self) -> Kind {
        match self {
            Kind::Labels(labels) => {
                let mut labels = labels.clone();
                labels.sort();
                Kind::Labels(labels)
            },
            _ => self.clone(),
        }
    }

    fn equal(&self, second: &Self) -> bool {
        return self.normalize() == second.normalize()
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Ty => write!(f, "Ty"),
            Kind::Labels(labels) => write!(f, "{{{}}}", serialize_label_vector(labels)),
        }
    }
}


#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BaseType {
    IntType,
    BoolType,
    FloatType,
    StringType,
}

#[derive(Clone, PartialEq, Debug)]
pub enum CastAndConvType {
    // Type of v         input      blame                   output
    CastType(Box<Type>, Box<Type>, ConvertionOrBlameLabel, Box<Type>),
    SealType(Box<Type>, TypeName),
    SealRecordType(Box<Type>, TypeName, Vec<Label>),
    SealVariantType(Box<Type>, TypeName, Vec<Label>),
    DynamicSpecial(Box<Type>, GroundType, BlameLabel),
}



#[derive(Clone, PartialEq, Debug)]
pub enum GroundType {
    TypeName(Var),
    BaseType(BaseType),
    DynFunction,
    DynRecord,
    DynVariant,
}

pub trait IsBaseType {
    fn is_base_type(&self) -> bool;
    fn is_base_type_and(&self, base_type: &BaseType) -> bool;
}

impl IsBaseType for Type {
    fn is_base_type(&self) -> bool {
        match self {
            Type::BaseType(_) => true,
            _ => false,
        }
    }

    fn is_base_type_and(&self, base_type: &BaseType) -> bool {
        match self {
            Type::BaseType(bt) => bt == base_type,
            _ => false,
        }
    }

    
}

impl IsBaseType for GroundType {
    fn is_base_type(&self) -> bool {
        self.to_type().is_base_type()
    }

    fn is_base_type_and(&self, base_type: &BaseType) -> bool {
        self.to_type().is_base_type_and(base_type)
    }
}

pub trait IsTypeName {
    fn is_type_name(&self) -> bool;
}

impl IsTypeName for GroundType {
    fn is_type_name(&self) -> bool {
        self.to_type().is_type_name()
    }
}

pub trait ToType {
    fn to_type(&self) -> Type;
}

impl ToType for GroundType {
    fn to_type(&self) -> Type {
        match self {
            GroundType::TypeName(var) => Type::TypeName(var.clone()),
            GroundType::BaseType(base_type) => Type::BaseType(*base_type),
            GroundType::DynFunction => Type::FunctionType(Box::new(Type::DynType), Box::new(Type::DynType)),
            GroundType::DynRecord => Type::RecordsType(vec![], RecordAndVariantEnd::DynamicEnd),
            GroundType::DynVariant => Type::VariantType(vec![], RecordAndVariantEnd::DynamicEnd),
        }
    }
}

impl fmt::Display for GroundType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_type())
    }
}

pub trait ToGroundTypeOption {
    fn to_ground_type(&self, cast: bool) -> Option<GroundType>;
}

pub trait IsGroundType {
    fn is_ground_type(&self, cast: bool) -> bool;
}

impl ToGroundTypeOption for Type {
    fn to_ground_type(&self, cast: bool) -> Option<GroundType> {
        match self {
            Type::TypeName(var) => Some(GroundType::TypeName(var.clone())),
            Type::BaseType(base_type) => Some(GroundType::BaseType(*base_type)),
            Type::RecordsType(occurances, RecordAndVariantEnd::DynamicEnd) => if (occurances.len() != 0) || cast {None} else {Some(GroundType::DynRecord)},
            Type::VariantType(occurances, RecordAndVariantEnd::DynamicEnd) => if (occurances.len() != 0) || cast {None} else {Some(GroundType::DynVariant)},
            Type::FunctionType(input, output) => {
                if (Type::DynType != **input) && !cast {return None}
                if (Type::DynType != **output) && !cast {return None}
                Some(GroundType::DynFunction)
            },
            _ => None,
        }
    }
}

impl IsGroundType for Type {
    fn is_ground_type(&self, cast: bool) -> bool {
        self.to_ground_type(cast) != None
    }
}


#[derive(Clone, PartialEq, Debug)]
pub enum TechnicalType {
    // Technical types used in the typechecker and other modules
    UnknownType,
    YetUnknownRecursiveType, // Used if the typechecker isn't aware of the correct type for a recursive function yet
    IllegalType,
}

#[derive(Clone, PartialEq, Debug)]
pub enum RecordAndVariantEnd {
    Closed,
    DynamicEnd,
    VariableEnd(String),
}


#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    TypeVariable(Var),
    TypeName(Var),

    // Base Types
    BaseType(BaseType),

    // Base Types End

    DynType, // Dynamic Type


    // Complex types
    UniversalType(Var, Kind, Box<Type>),
    // UniversalTypeUnevaluated(Var, Kind, Box<Type>, Box<TypeEnvironment>, Box<Term>),
    // bool is to make records and variants dynamic
    RecordsType(Vec<LabelOccurrence>, RecordAndVariantEnd),
    VariantType(Vec<LabelOccurrence>, RecordAndVariantEnd),
    FunctionType(Box<Type>, Box<Type>),

    CastAndConvType(CastAndConvType),

    TechnicalType(TechnicalType),
}

impl EquivalenceUtils for Type {
    fn normalize(&self) -> Type {
        match self {
            Type::UniversalType(var, kind, subtype) => Type::UniversalType(var.clone(), kind.normalize(), Box::new(subtype.normalize())),
            //Type::UniversalTypeUnevaluated(var, kind, subtype, env, term) => Type::UniversalTypeUnevaluated(var.clone(), kind.normalize(), Box::new(subtype.normalize()), env.clone(), term.clone()),
            Type::RecordsType(fieldoccs, dynamic) => {
                let mut fieldoccs = fieldoccs.clone();
                fieldoccs.sort_by(|(a_name, _), (b_name, _)| a_name.cmp(b_name));
                Type::RecordsType(fieldoccs, dynamic.clone())
            },
            Type::VariantType(fieldoccs, dynamic) => {
                let mut fieldoccs = fieldoccs.clone();
                fieldoccs.sort_by(|(a_name, _), (b_name, _)| a_name.cmp(b_name));
                Type::VariantType(fieldoccs, dynamic.clone())
            },
            Type::FunctionType(input, output) => Type::FunctionType(Box::new(input.normalize()), Box::new(output.normalize())),
            Type::CastAndConvType(castconv) => Type::CastAndConvType(match castconv {
                CastAndConvType::CastType(subtype, type1, b, type2) => CastAndConvType::CastType(Box::new(subtype.normalize()), Box::new(type1.normalize()), b.clone(), Box::new(type2.normalize())),
                CastAndConvType::SealType(subtype, convlab) => CastAndConvType::SealType(Box::new(subtype.normalize()), convlab.clone()),
                CastAndConvType::SealRecordType(subtype, convlab, labels) => {
                    let mut labels = labels.clone();
                    labels.sort();
                    CastAndConvType::SealRecordType(Box::new(subtype.normalize()), convlab.clone(), labels)},
                CastAndConvType::SealVariantType(subtype, convlab, labels) => {
                    let mut labels = labels.clone();
                    labels.sort();
                    CastAndConvType::SealVariantType(Box::new(subtype.normalize()), convlab.clone(), labels)},
                CastAndConvType::DynamicSpecial(subtype, groundtype, blame) => CastAndConvType::DynamicSpecial(Box::new(subtype.normalize()), groundtype.clone(), blame.clone())
            }),
            _ => self.clone(),
        }
    }

    fn equal(&self, second: &Self) -> bool {
        self.normalize() == second.normalize()
    }
}

/*fn equal_internal(type_1: &Type, type_2: &Type) -> bool {
    match type_1 {
        Type::RecordsType(occurances, dynamic) => match type_2 {
            Type::RecordsType(second_occurances, second_dynamic) => {
                if dynamic != second_dynamic {return false}
                let occurances_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(occurances.clone());
                let second_occurances_map:  HashMap<String, FieldOccurrence> = HashMap::from_iter(second_occurances.clone());
                for (key, occurence) in occurances_map {
                    match occurence.clone() {
                        FieldOccurrence::Absent => { /* Nothing todo here */ },
                        FieldOccurrence::Star => { /* Nothing todo here */ },
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
        Type::VariantType(occurances, _dynamic) => match type_2 {
            Type::VariantType(second_occurances, _dynamic) => {
                let occurances_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(occurances.clone());
                let second_occurances_map:  HashMap<String, FieldOccurrence> = HashMap::from_iter(second_occurances.clone());
                for (key, occurence) in occurances_map {
                    match occurence.clone() {
                        FieldOccurrence::Absent => { /* Nothing todo here */ },
                        FieldOccurrence::Star => { /* Nothing todo here */ },
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
}*/

fn record_or_variant_type_replace_target(subs: &Vec<(String, FieldOccurrence)>, target: &Type, replacement: &Type) -> Vec<(String, FieldOccurrence)> {
    Vec::from_iter(subs.iter().map(|x| (x.0.clone(), if x.1.is_present() {FieldOccurrence::Present(x.1.to_type().expect("record_or_variant_type_replace_target: Error!").replace_target(target, replacement))} else {x.1.clone()})))
}

fn record_or_variant_type_contains_dyn(subs: &Vec<(String, FieldOccurrence)>) -> bool {
    for (_, occs) in subs {
        match occs {
            FieldOccurrence::Absent => {},
            FieldOccurrence::Present(types) => if types.contains_dyn() {return true},
            FieldOccurrence::Star => return true,
        }
    }
    return false;
}

fn record_or_variant_type_contains_typevar(subs: &Vec<(String, FieldOccurrence)>, tv: &String) -> bool {
    for (_, occs) in subs {
        match occs {
            FieldOccurrence::Absent => {},
            FieldOccurrence::Present(types) => if types.contains_typevar(tv) {return true},
            FieldOccurrence::Star => {},
        }
    }
    return false;
}

pub trait TypeUtils {
    fn to_kind(&self) -> Kind;
    fn of_kind(&self, kind: &Kind) -> bool;
    fn is_variant(&self) -> bool;
    fn is_record(&self) -> bool;
    fn is_dyn(&self) -> bool;
    fn is_function(&self) -> bool;
    fn is_universal(&self) -> bool;
    fn is_type_name(&self) -> bool;
    fn is_type_name_with(&self, name: &String) -> bool;
    fn is_type_var(&self) -> bool;
    fn get_name_of_type_name(&self) -> Option<String>;
    fn is_qpoly(&self) -> bool;
    fn contains_dyn(&self) -> bool;
    fn contains_typevar(&self, tv: &String) -> bool; 
    fn replace_target(&self, target: &Type, replacement: &Type) -> Type;
    fn get_function_types(&self) -> Option<(Type, Type)>;
    fn get_universal_types(&self) -> Option<(String, Kind, Type)>;
    fn get_record_fields(&self) -> Option<(Vec<(String, FieldOccurrence)>, RecordAndVariantEnd)>;
    fn get_variant_fields(&self) -> Option<(Vec<(String, FieldOccurrence)>, RecordAndVariantEnd)>;

    fn is_seal(&self) -> bool;
    fn is_seal_record(&self) -> bool;
    fn is_seal_variant(&self) -> bool;
    fn is_cast(&self) -> bool;
}

impl TypeUtils for Type {

    fn to_kind(&self) -> Kind {
        match self {
            Type::RecordsType(fieldoccs, _) => {
                Kind::Labels(fieldoccs.iter().map(|(label, _)| label.clone()).collect())
            },
            Type::VariantType(fieldoccs, _) => {
                Kind::Labels(fieldoccs.iter().map(|(label, _)| label.clone()).collect())
            },
            _ => Kind::Ty,
        }
    }

    fn of_kind(&self, kind: &Kind) -> bool {
        (*kind == Kind::Ty) || (self.to_kind() == *kind)
    }

    fn is_variant(&self) -> bool {
        match self {
            Type::VariantType(_, _) => true,
            _ => false,
        }
    }

    fn is_record(&self) -> bool {
        match self {
            Type::RecordsType(_, _) => true,
            _ => false,
        }
    }

    fn is_dyn(&self) -> bool {
        match self {
            Type::DynType => true,
            Type::CastAndConvType(CastAndConvType::DynamicSpecial(_, _, _)) => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            Type::FunctionType(_, _) => true,
            _ => false,
        }
    }

    fn is_universal(&self) -> bool {
        match self {
            Type::UniversalType(_, _, _) => true,
            _ => false,
        }
    }

    fn is_type_name(&self) -> bool {
        match self {
            Type::TypeName(_) => true,
            _ => false,
        }
    }

    fn is_type_name_with(&self, name: &String) -> bool {
        match self {
            Type::TypeName(tn) => tn == name,
            _ => false,
        }
    }

    fn is_type_var(&self) -> bool {
        match self {
            Type::TypeVariable(_) => true,
            _ => false,
        }
    }

    fn get_name_of_type_name(&self) -> Option<String> {
        match self {
            Type::TypeName(name) => Some(name.clone()),
            _ => None,
        }
    }

    fn is_qpoly(&self) -> bool {
        match self {
            Type::UniversalType(_, _ , _) => false,
            _ => self.contains_dyn(),
        }
    }

    fn contains_dyn(&self) -> bool {
        if self.is_dyn() {return true}
        match self {
            Type::UniversalType(_, _, sub_type) => sub_type.contains_dyn(),
            Type::RecordsType(subs, is_dyn) => (is_dyn == &RecordAndVariantEnd::DynamicEnd) || record_or_variant_type_contains_dyn(subs),
            Type::VariantType(subs, is_dyn) => (is_dyn == &RecordAndVariantEnd::DynamicEnd)|| record_or_variant_type_contains_dyn(subs),
            Type::FunctionType(input, output) => input.contains_dyn() || output.contains_dyn(),
            _ => false,
        }
    }

    fn contains_typevar(&self, tv: &String) -> bool {
        match self {
            Type::UniversalType(_, _, sub_type) => sub_type.contains_typevar(tv),
            Type::RecordsType(subs, _is_dyn) => record_or_variant_type_contains_typevar(subs, tv),
            Type::VariantType(subs, _is_dyn) => record_or_variant_type_contains_typevar(subs, tv),
            Type::FunctionType(input, output) => input.contains_typevar(tv) || output.contains_typevar(tv),
            Type::TypeVariable(var) => var == tv,
            _ => false,
        }
    }

    fn replace_target(&self, target: &Type, replacement: &Type) -> Type {
        if self == target {return replacement.clone()}
        match self {
            // Don't replace a target in a univeral type, these aren't considered "free"
            Type::UniversalType(x, k, sub_type) => Type::UniversalType(x.clone(), k.clone(), Box::new(sub_type.replace_target(target, replacement))),
            Type::RecordsType(subs, end) => {match end {
                RecordAndVariantEnd::VariableEnd(var) => if (&Type::TypeVariable(var.clone()) == target) && replacement.is_record() {
                    match replacement.get_record_fields() {
                        Some((fields, end)) => {
                            let mut newvec = record_or_variant_type_replace_target(subs, target, replacement);
                            newvec.extend(fields.into_iter());
                            return Type::RecordsType(newvec, end)},
                        None => panic!("Error in replace_target"),
                    }
                },
                _ => {},
                }
                Type::RecordsType(record_or_variant_type_replace_target(subs, target, replacement), end.clone())
            },
            Type::VariantType(subs, end) =>{ match end {
                RecordAndVariantEnd::VariableEnd(var) => if (&Type::TypeVariable(var.clone()) == target) && replacement.is_variant() {
                    match replacement.get_variant_fields() {
                        Some((fields, end)) => {
                            let mut newvec = record_or_variant_type_replace_target(subs, target, replacement);
                            newvec.extend(fields.into_iter());
                            return Type::VariantType(newvec, end)},
                        None => panic!("Error in replace_target"),
                    }
                },
                _ => {},
                }
                Type::VariantType(record_or_variant_type_replace_target(subs, target, replacement), end.clone())
            },
            Type::FunctionType(input, output) => Type::FunctionType(Box::new(input.replace_target(target, replacement)), Box::new(output.replace_target(target, replacement))),
            _ => self.clone()
        }
    }


    fn get_function_types(&self) -> Option<(Type, Type)> {
        match self {
            Type::FunctionType(a, b) => Some((*a.clone(), *b.clone())),
            _ => None,
        }
    }

    fn get_universal_types(&self) -> Option<(String, Kind, Type)> {
        match self {
            Type::UniversalType(var, kind, subtype) => Some((var.clone(), kind.clone(), (**subtype).clone())),
            _ => None,
        }
    }

    fn get_record_fields(&self) -> Option<(Vec<(String, FieldOccurrence)>, RecordAndVariantEnd)> {
        match self {
            Type::RecordsType(occs, is_dyn) => Some((occs.clone(), is_dyn.clone())),
            _ => None,
        }
    }

    fn get_variant_fields(&self) -> Option<(Vec<(String, FieldOccurrence)>, RecordAndVariantEnd)> {
        match self {
            Type::VariantType(occs, is_dyn) => Some((occs.clone(), is_dyn.clone())),
            _ => None,
        }
    }

    fn is_cast(&self) -> bool {
        match self {
            Type::CastAndConvType(CastAndConvType::CastType(_, _, _, _)) => true,
            _ => false,
        }
    }

    fn is_seal(&self) -> bool {
        match self {
            Type::CastAndConvType(CastAndConvType::SealType(_, _)) => true,
            _ => false,
        }
    }

    fn is_seal_record(&self) -> bool {
        match self {
            Type::CastAndConvType(CastAndConvType::SealRecordType(_, _, _)) => true,
            _ => false,
        }
    }

    fn is_seal_variant(&self) -> bool {
        match self {
            Type::CastAndConvType(CastAndConvType::SealVariantType(_, _, _)) => true,
            _ => false,
        }
    }


}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BaseType::IntType => write!(f, "int"),
            BaseType::BoolType => write!(f, "bool"),
            BaseType::FloatType => write!(f, "float"),
            BaseType::StringType => write!(f, "string"),
        }
    }
}

impl fmt::Display for CastAndConvType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CastAndConvType::CastType(subtype, from, blame, to) => write!(f, "{}: {}=[{}]=>{}", subtype, from, blame, to),
            CastAndConvType::SealType(typ, typename) => write!(f, "SealType ({}, {})", typ, typename),
            CastAndConvType::SealRecordType(typ, typename, labels) => write!(f, "SealRecordType ({}, {}, {:#?})", typ, typename, labels),
            CastAndConvType::SealVariantType(typ, typename, labels) => write!(f, "SealVariantType ({}, {}, {:#?})", typ, typename, labels),
            CastAndConvType::DynamicSpecial(subtype, groundtype, blame) => write!(f, "DynamicSpecialType ({}, {}, {})", subtype, groundtype, blame),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::TypeVariable(var) => write!(f, "{}", var),
            Type::TypeName(var) => write!(f, "{}", var),
            Type::BaseType(bt) => write!(f, "{}", bt),
            Type::DynType => write!(f, "*"),
            Type::UniversalType(var, kind, types) => write!( f, "all ({}: {}) {}", var, kind, types),
            // Type::UniversalTypeUnevaluated(var, kind, types, _env, _term) => write!( f, "all ({}: {}) {}", var, kind, types),
            Type::RecordsType(lab_occ_vec, end) => match end {
                RecordAndVariantEnd::Closed => {write!(f, "{{{}}}", serialize_label_occurrence_vector(lab_occ_vec))},
                RecordAndVariantEnd::DynamicEnd => {write!(f, "{{{}, *}}", serialize_label_occurrence_vector(lab_occ_vec))},
                RecordAndVariantEnd::VariableEnd(var) => {write!(f, "{{{}, {}}}", serialize_label_occurrence_vector(lab_occ_vec), var.clone())},
            },
            Type::VariantType(lab_occ_vec, end) => match end {
                RecordAndVariantEnd::Closed => {write!(f, "[{}]", serialize_label_occurrence_vector(lab_occ_vec))},
                RecordAndVariantEnd::DynamicEnd => {write!(f, "[{}, *]", serialize_label_occurrence_vector(lab_occ_vec))},
                RecordAndVariantEnd::VariableEnd(var) => {write!(f, "[{}, {}]", serialize_label_occurrence_vector(lab_occ_vec), var.clone())},
            },
            Type::FunctionType(in_type, out_type) => write!(f, "({})->({})", in_type, out_type),
            Type::CastAndConvType(cct) => write!(f, "{}", cct),
            Type::TechnicalType(tt) => write!(f, "{}", tt),
        }
    }
}

impl fmt::Display for TechnicalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TechnicalType::UnknownType => write!(f, "UNKNOWN"),
            TechnicalType::YetUnknownRecursiveType => write!(f, "YETUNKNOWNRECURSIVETYPE"),
            TechnicalType::IllegalType => write!(f, "ILLEGAL"),
        }
    }
}


#[derive(Clone, PartialEq, Debug)]
pub enum FieldOccurrence {
    Absent,
    Present(Type),
    Star,
}

pub trait FieldOccurrenceTrait {
    fn to_type(&self) -> Option<Type>;
    fn is_present(&self) -> bool;
    fn is_absent(&self) -> bool;
    fn is_star(&self) -> bool;
}

impl FieldOccurrenceTrait for FieldOccurrence {
    fn to_type(&self) -> Option<Type> {
        match self {
            FieldOccurrence::Present(typ) => Some(typ.clone()),
            _ => None,
        }
    }

    fn is_present(&self) -> bool {
        match self {
            FieldOccurrence::Present(_) => true,
            _ => false,
        }
    }

    fn is_absent(&self) -> bool {
        match self {
            FieldOccurrence::Absent => true,
            _ => false,
        }
    }

    fn is_star(&self) -> bool {
        match self {
            FieldOccurrence::Star => true,
            _ => false,
        }
    }

}

impl fmt::Display for FieldOccurrence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FieldOccurrence::Absent => write!(f, "-"),
            FieldOccurrence::Present(typ) => write!(f, "+{}", typ),
            FieldOccurrence::Star => write!(f, "@"),
        }
    }
}