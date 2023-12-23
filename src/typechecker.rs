use crate::types::*;
use crate::utils::*;
use crate::environment::*;
use crate::terms::*;
use crate::logicterms::*;
use crate::values::ValueTypes;
// use std::arch::x86_64;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub enum TypeCheckError {
    WrongType(Type, Type),
    WrongKind(Kind, Kind),
    IncompatibleTypes(Type, Type),
    LabelNotPresentInType(Type, Label),
    TypeOrVarNotFound(Label),
    IncompatibleOperation(String, Type),
    IncompatibleBinOperation(Type, String, Type),
    TypeOfFunctionCannotBeDetermined(String),
    CallToYetUnknownRecursiveType,
    TypecheckerHasTermNotImplemented(Term),
    Blame(ConvertionOrBlameLabel),
    NotWellformed(Type, Kind),
    NotConsistent(Type, Type),
    NotConsistentOccurance(FieldOccurrence, FieldOccurrence),
    NotConvertible(Type, Type),
    NotConvertibleOccurance(FieldOccurrence, FieldOccurrence),
    TypeNameConflict(String, Type, Type),
    UnknownError,
}

pub type TypeCheckResult = Result<(TypeEnvironment, Type), TypeCheckError>;

pub fn typecheck_get_kind(env: TypeEnvironment, typ: &Type) -> Result<(TypeEnvironment, Kind), TypeCheckError> {
    match typ {
        Type::TypeVariable(var) => match env.typevar_get(var) {
            Some(typ) => Ok((env, typ)),
            None => Err(TypeCheckError::TypeOrVarNotFound(var.clone())),
        },
        _ => Ok((env, typ.to_kind()))
    }
}

pub fn typecheck(env: TypeEnvironment, term: &Term) -> TypeCheckResult {
    let child = env.new_child();
    let (mut env, result) = match term {
        Term::Block(terms) => typecheck_block_rec(child.clone(), unit(), terms, 0),
        _ => typecheck_single_term(child.clone(), term)
    }?;

    // Make sure that all type names returned by a block are compatible
    let mut all_type_names = result.get_all_type_names();
    let mut atn_counter = 0;
    while atn_counter != all_type_names.len() {
        let tn_type = child.get(&all_type_names[atn_counter]);
        match tn_type {
            Some(some_ty_type) => {
                if !env.insert(all_type_names[atn_counter].clone(), some_ty_type.clone()) {
                    match env.get(&all_type_names[atn_counter]) {
                        Some(original_type) => if !original_type.equal(&some_ty_type) {return Err(TypeCheckError::TypeNameConflict(all_type_names[atn_counter].clone(), original_type,some_ty_type))},
                        None => return Err(TypeCheckError::UnknownError),
                    }
                }
                let mut new_type_names = some_ty_type.get_all_type_names();
                if new_type_names.len() != 0 {all_type_names.append(&mut new_type_names)}
            },
            None => {},
        }
        atn_counter += 1;
    }
    Ok((env, result))
}

pub fn typecheck_verify(env: TypeEnvironment, term: &Term, verify_type: &Type) -> Result<(TypeEnvironment, bool), TypeCheckError> {
    let (env, result) = typecheck(env, term)?;
    return Ok((env, result.normalize() == verify_type.normalize()))
}

pub fn consistent_row(env: &TypeEnvironment, row1: Type, row2: Type) -> Result<(), TypeCheckError> {
    let (mut row1_occs, row1_dyn) = match row1.clone() {
        Type::RecordsType(occs, dynamic) => (occs, dynamic),
        Type::VariantType(occs, dynamic) => (occs, dynamic),
        _ => return Err(TypeCheckError::WrongType(row1, unit())),
    };
    let (mut row2_occs, row2_dyn) = match row2.clone() {
        Type::RecordsType(occs, dynamic) => (occs, dynamic),
        Type::VariantType(occs, dynamic) => (occs, dynamic),
        _ => return Err(TypeCheckError::WrongType(row2, unit())),
    };

    if (row1_occs.len() == 0 && (row1_dyn == RecordAndVariantEnd::DynamicEnd)) || 
       (row2_occs.len() == 0 && (row2_dyn == RecordAndVariantEnd::DynamicEnd)) || 
       (row1_occs.len() == 0 && row2_occs.len() == 0) {return Ok(())}

    if row1_occs.len() == 0 || row2_occs.len() == 0 {return Err(TypeCheckError::UnknownError)}

    let (fst_1_label, fst_1_occ) = &row1_occs[0];
    let (fst_2_label, fst_2_occ) = &row2_occs[0];

    if fst_1_label == fst_2_label {
        // Just return the error if this fails, we dont care about the result
        consistent_occurance(env, fst_1_occ, fst_2_occ)?;

        row1_occs.remove(0);
        row2_occs.remove(0);
        return consistent_row(env, Type::RecordsType(row1_occs, row1_dyn), Type::RecordsType(row2_occs, row2_dyn))
    } else if row1_dyn == RecordAndVariantEnd::DynamicEnd {
        row2_occs.remove(0);
        return consistent_row(env, Type::RecordsType(row1_occs, row1_dyn), Type::RecordsType(row2_occs, row2_dyn))
    } else if row2_dyn == RecordAndVariantEnd::DynamicEnd {
        row1_occs.remove(0);
        return consistent_row(env, Type::RecordsType(row1_occs, row1_dyn), Type::RecordsType(row2_occs, row2_dyn))
    }

    



    Err(TypeCheckError::NotConsistent(row1.clone(), row2.clone()))


}

pub fn consistent_type(env: &TypeEnvironment, type1: &Type, type2: &Type) -> Result<(), TypeCheckError> {
    // CTg_Refl && CTg_DynL && CTg_DynR
    if type1.equal(type2) || type1.equal(&Type::DynType) || type2.equal(&Type::DynType) {return Ok(())}

    // CTg_Fun
    if type1.is_function() && type2.is_function() {
        let (a1, b1) = type1.get_function_types().expect("Error in consistent_type");
        let (a2, b2) = type2.get_function_types().expect("Error in consistent_type");
        consistent_type(env, &a1,& a2)?;
        return consistent_type(env, &b1,& b2)
    }

    // CTg_Poly
    if type1.is_universal() && type2.is_universal() {
        let mut envclone = env.flat_independent_clone();
        let (x1, k1, a1) = type1.get_universal_types().expect("Error in consistent_type");
        let (x2, k2, a2) = type2.get_universal_types().expect("Error in consistent_type");
        if x1==x2 && k1.equal(&k2) {
            envclone.typevar_insert(x1, k1);
            return consistent_type(&envclone, &a1, &a2)
        }
    }

    // CTg_PolyL
    if type1.is_universal() && type2.is_qpoly() {
        let mut envclone = env.flat_independent_clone();
        let (x1, k1, a1) = type1.get_universal_types().expect("Error in consistent_type");
        if type2.contains_typevar(&x1) {return Err(TypeCheckError::NotConsistent(type1.clone(), type2.clone()))}
        envclone.typevar_insert(x1, k1);
        return consistent_type(&envclone, &a1, type2)
    }

    // CTg_PolyR
    if type2.is_universal() && type1.is_qpoly() {
        let mut envclone = env.flat_independent_clone();
        let (x2, k2, a2) = type2.get_universal_types().expect("Error in consistent_type");
        if type1.contains_typevar(&x2) {return Err(TypeCheckError::NotConsistent(type1.clone(), type2.clone()))}
        envclone.typevar_insert(x2, k2);
        return consistent_type(&envclone, type1, &a2)
    }

    if (type1.is_record() && type2.is_record()) || (type1.is_variant() && type2.is_variant()) {
        return consistent_row(env, type1.clone(), type2.clone())
    }

    Err(TypeCheckError::NotConsistent(type1.clone(), type2.clone()))
}

pub fn consistent_occurance(env: &TypeEnvironment, occ1: &FieldOccurrence, occ2: &FieldOccurrence) -> Result<(), TypeCheckError> {
    if (occ1.is_absent() && occ2.is_absent()) || occ1.is_star() || occ2.is_star() {return Ok(())}
    
    if occ1.is_present() && occ2.is_present() {
        let type1 = occ1.to_type().expect("Error in consistent_occurance");
        let type2 = occ2.to_type().expect("Error in consistent_occurance");
        return consistent_type(env, &type1, &type2)
    }

    Err(TypeCheckError::NotConsistentOccurance(occ1.clone(), occ2.clone()))
}

pub fn convertible_type(mut env: TypeEnvironment, type1: &Type, conv: &ConvertionLabel, type2: &Type) -> Result<TypeEnvironment, TypeCheckError> {
    // Cv_Dyn Cv_TVar Cv_Base
    if type1.equal(type2) && (type1.is_dyn() || type1.is_type_var() || type1.is_base_type()) {return Ok(env)}

    // Cv_TName
    if type1.equal(type2) && !type1.is_type_name_with(&conv.to_label()) {return Ok(env)}

    // Cv_TReveal
    if type1.is_type_name_with(&conv.to_label()) && conv.is_present() && env.get(&conv.to_label()).is_some_and(|x| x.equal(type2)){return Ok(env)}

    // Cv_TReveal
    if type1.is_type_name_with(&conv.to_label()) && conv.is_present() && !env.keys().contains(&conv.to_label()){
        env.insert(conv.to_label(), type2.clone());
        return Ok(env)}

    // Cv_TConceal
    if type2.is_type_name_with(&conv.to_label()) && !conv.is_present() && env.insert(conv.to_label(), type1.clone()) {return Ok(env)}

    // Cv_TConceal
    if type2.is_type_name_with(&conv.to_label()) && !conv.is_present() && env.get(&conv.to_label()).is_some_and(|x| x.equal(type1)) {return Ok(env)}

    // Cv_Fun
    if type1.is_function() && type2.is_function() {
        let (a1, b1) = type1.get_function_types().expect("Error in convertible_type");
        let (a2, b2) = type2.get_function_types().expect("Error in convertible_type");
        let env = convertible_type(env, &a2, & conv.invert(), &a1)?;
        return convertible_type(env, &b1, conv, &b2)
    }

    // Cv_Poly
    if type1.is_universal() && type2.is_universal() {
        let (x1, k1, a1) = type1.get_universal_types().expect("Error in convertible_type");
        let (x2, k2, a2) = type2.get_universal_types().expect("Error in convertible_type");

        if x1 == x2 && k1.equal(&k2) {
            return convertible_type(env, &a1, conv, &a2)
        }
    }

    // Cv_Record Cv_Variant
    if (type1.is_record() && type2.is_record()) || (type1.is_variant() && type2.is_variant()) { return convertible_row(env, type1.clone(), conv, type2.clone())}

    // println!("This fails here!");
    Err(TypeCheckError::NotConvertible(type1.clone(), type2.clone()))

}

pub fn convertible_occurance(env: TypeEnvironment, occ1: &FieldOccurrence, conv: &ConvertionLabel, occ2: &FieldOccurrence) -> Result<TypeEnvironment, TypeCheckError> {
    if (occ1.is_absent() && occ2.is_absent()) || (occ1.is_star() && occ2.is_star()) {return Ok(env)}
    
    if occ1.is_present() && occ2.is_present() {
        let type1 = occ1.to_type().expect("Error in consistent_occurance");
        let type2 = occ2.to_type().expect("Error in consistent_occurance");
        return convertible_type(env, &type1, conv, &type2)
    }

    Err(TypeCheckError::NotConvertibleOccurance(occ1.clone(), occ2.clone()))
}

pub fn convertible_row(env: TypeEnvironment, row1: Type, conv: &ConvertionLabel, row2: Type) -> Result<TypeEnvironment, TypeCheckError> {

    let (mut row1_occs, row1_dyn) = match row1.clone() {
        Type::RecordsType(occs, dynamic) => (occs, dynamic),
        Type::VariantType(occs, dynamic) => (occs, dynamic),
        _ => return Err(TypeCheckError::WrongType(row1, unit())),
    };
    let (mut row2_occs, row2_dyn) = match row2.clone() {
        Type::RecordsType(occs, dynamic) => (occs, dynamic),
        Type::VariantType(occs, dynamic) => (occs, dynamic),
        _ => return Err(TypeCheckError::WrongType(row2, unit())),
    };

    // Cvr_Emp
    if row1_occs.is_empty() && row2_occs.is_empty() {return Ok(env)}

    if !row1_occs.is_empty() && !row1_occs.is_empty() {
        let (l1, o1) = &row1_occs[0];
        let (l2, o2) = &row2_occs[0];
        if l1 == l2 {

            // CvR_Ext
            let env = convertible_occurance(env, o1, conv, o2)?;
            row1_occs.remove(0);
            row2_occs.remove(0);
            return convertible_row(env, Type::RecordsType(row1_occs, row1_dyn), conv, Type::RecordsType(row2_occs, row2_dyn))

            // I don't quite get the other CvR rules
        }
    }



    Err(TypeCheckError::NotConvertible(row1.clone(), row2.clone()))
}


pub fn wellformed_context(_env: &TypeEnvironment) -> Result<(), TypeCheckError> {
    Ok(())
    // Todo implement this properly but this should be a decent approxiamtion for the mean time
}

pub fn wellformed_rows(env: &TypeEnvironment, row: &Type) -> Result<(), TypeCheckError> {

    let (occs, _row_dyn) = match row {
        Type::RecordsType(occs, dynamic) => (occs, dynamic),
        Type::VariantType(occs, dynamic) => (occs, dynamic),
        _ => return Err(TypeCheckError::WrongType(row.clone(), unit())),
    };
    for (_ , occ) in occs {
        wellformed_occurances(env, &occ)?;
    }
    Ok(())

}

pub fn wellformed_occurances(env: &TypeEnvironment, wfocc: &FieldOccurrence) -> Result<(), TypeCheckError> {
    match wfocc {
        FieldOccurrence::Present(subtype) => wellformed_types(env, subtype, &Kind::Ty),
        _ => wellformed_context(env)
    }
}

pub fn wellformed_types(env: &TypeEnvironment, wftype: &Type, wfkind: &Kind) -> Result<(), TypeCheckError> {
    match wftype {
        Type::TypeVariable(tv) => if env.typevar_get(tv).is_some_and(|x| x.equal(wfkind)) {wellformed_context(env)} else {Err(TypeCheckError::NotWellformed(wftype.clone(), wfkind.clone()))},
        Type::TypeName(alpha) => match env.get(alpha) {
            Some(subtype) => wellformed_types(env, &subtype, wfkind),
            None => Err(TypeCheckError::TypeOrVarNotFound(alpha.clone())),
        },
        Type::BaseType(_) => if wfkind == &Kind::Ty { wellformed_context(env)} else {Err(TypeCheckError::NotWellformed(wftype.clone(), wfkind.clone()))},
        Type::DynType => wellformed_context(env),
        Type::UniversalType(x, k, a) => {
            let mut my_env = env.flat_independent_clone();
            my_env.typevar_insert(x.clone(), k.clone());
            wellformed_types(&my_env, a, &Kind::Ty)
        },
        // This recordsType definition is wrong
        Type::RecordsType(_label_occs, _dyn_rec) => wellformed_rows(env, &wftype),
        Type::VariantType(_label_occs, _) => wellformed_rows(env, &wftype),
        Type::FunctionType(a, b) => if wfkind == &Kind::Ty {wellformed_types(env, a, wfkind)?; wellformed_types(env, b, wfkind)} else {Err(TypeCheckError::NotWellformed(wftype.clone(), wfkind.clone()))},
        _ => Err(TypeCheckError::NotWellformed(wftype.clone(), wfkind.clone()))
    }

}

fn typecheck_block_rec(env: TypeEnvironment, result: Type, terms: &TermBlock, index: usize) -> TypeCheckResult {
    if terms.len() == index { return Ok((env, result)) }
    let (new_env, new_result) = typecheck_single_term(env, &terms[index])?;
    typecheck_block_rec(new_env, new_result, terms, index+1)
}
#[allow(unreachable_patterns)]
pub fn typecheck_single_term(env: TypeEnvironment, term: &Term) -> TypeCheckResult {
    match term {
        Term::Constant(value) => Ok((env, value.get_type().normalize())),
        Term::Variable(var) => typecheck_variable(env, var),
        Term::LogicGate(lt) => typecheck_logicgate(env, lt),
        Term::Block(_) => typecheck(env, term),
        Term::Let(var, term) => typecheck_let(env, var, term),
        Term::If(decider, consequence, alternative) => typecheck_if(env, decider, consequence, alternative),
        Term::Function(label, parameter, parameter_type, typ, body) => typecheck_function(env, label, typ, parameter, parameter_type, body),
        Term::AnonymousFunction(parameter, parameter_type, _mby_type, body) => typecheck_anonymous_function(env, parameter, parameter_type, body),
        Term::RecursiveAnonymousFunction(label, parameter, parameter_type, typ, body) => typecheck_anonymous_recursive_function(env, label, typ, parameter, parameter_type, body),
        Term::FunctionCall(term, parameters) => typecheck_function_call(env, term, parameters),
        Term::TypeApplication(block, types) => typecheck_type_application(env, block, types),
        Term::RecordConstruction(variant_vec) => typecheck_record_construction(env, variant_vec),
        Term::RecordUpdate(record, target, new_type) => typecheck_record_update(env, record, target, new_type),
        Term::RecordSelection(record, target) => typecheck_record_selection(env, record, target),
        Term::VariantConstruction(variant) => typecheck_variant_construction(env, variant),
        Term::VariantCase(variant, con_label, con_var, con, alt_var, alt) => typecheck_variant_case(env, variant, con_label, con_var, con, alt_var, alt),
        Term::BigLambda(type_var, kind, term) => typecheck_big_lambda(env, type_var, kind, term),
        Term::Promise(types, _block) => Ok((env, types.clone())),
        Term::Print(_) => Ok((env, unit())),
        Term::Blame(blame_label) => Err(TypeCheckError::Blame(blame_label.clone())),
        Term::ConstantType(subtype) => Ok((env, subtype.clone())),
        Term::Cast(subterm, intype, blame, outtype) => typecheck_cast(env, subterm, intype, blame, outtype),
        Term::Convertion(subterm, intype, convertion_label, outtype) => typecheck_convertion(env, subterm, intype, convertion_label, outtype),
        _ => todo!(),
    }
}
#[warn(unreachable_patterns)]

pub fn typecheck_variant_case(env: TypeEnvironment, variant: &Term, con_label: &Label, con_var: &Var, con: &Term, alt_var: &Var, alt: &Term) -> TypeCheckResult {
    let (env, variant_type) = typecheck(env, variant)?; // We force disable type-prediction here, since we need a valid type in the checks in the later part of this function
    // Should the underling variant evaluation need type-prediction, this can still be enabled by an typecheck step which occurs within this typecheck (at least I hope so)
    match &variant_type {
        Type::VariantType(variant_tup, _dynamic) => {
            let variant_map = field_occ_tuple_to_map(variant_tup.clone());
            match variant_map.get(con_label) {
                None => {},
                Some(occ) => match occ {
                    FieldOccurrence::Absent => {},
                    FieldOccurrence::Star => {},
                    FieldOccurrence::Present(typ) => {
                        // Label exists in Variant
                        let (env, consequence_type) = typecheck_anonymous_function(env, &con_var.clone(), &typ.clone(), con)?;
                        let (env, alternative_type) = typecheck_anonymous_function(env, &alt_var.clone(), &variant_type, alt)?;

                        if !consequence_type.is_function() {return Err(TypeCheckError::IncompatibleTypes(Type::FunctionType(Box::new(unit()), Box::new(unit())), consequence_type))}
                        if !alternative_type.is_function() {return Err(TypeCheckError::IncompatibleTypes(Type::FunctionType(Box::new(unit()), Box::new(unit())), alternative_type))}

                        let con_out = consequence_type.get_function_types().expect("typecheck_variant_case: Error while writing con type").1;
                        let alt_out = alternative_type.get_function_types().expect("typecheck_variant_case: Error while writing alt type").1;

                        if con_out.equal(&alt_out) {return Ok((env, alt_out.clone())); } else {return Err(TypeCheckError::IncompatibleTypes(con_out.clone(), alt_out.clone()));}
                        

                    }
                },
            }

            // Label doesn't exist in Variant
            let (env, res_type) = typecheck_anonymous_function(env, &alt_var.clone(), &variant_type, alt)?;
            if !res_type.is_function() {return Err(TypeCheckError::IncompatibleTypes(Type::FunctionType(Box::new(unit()), Box::new(unit())), res_type))}
            Ok((env, res_type.get_function_types().expect("typecheck_variant_case: Error while writing res type").1.clone()))
        },
        _ => return Err(TypeCheckError::WrongType(Type::VariantType(vec![], RecordAndVariantEnd::Closed), variant_type)),
    }
}

pub fn typecheck_big_lambda(mut env: TypeEnvironment, type_var: &Label, kind: &Kind, term: &Term) -> TypeCheckResult {
    env.typevar_insert(type_var.clone(), kind.clone());
    let (env, term_type) = typecheck(env, term)?;
    Ok((env, Type::UniversalType(type_var.clone(), kind.clone(), Box::new(term_type))))
}

pub fn typecheck_type_application(env: TypeEnvironment, block: &Term, types: &Type) -> TypeCheckResult {
    let types = types.normalize();
    let (env, typ) = typecheck(env, block)?;
    match typ.clone() {
        Type::UniversalType(x, k, a) => {
            // if !types.of_kind(&k) {return Err(TypeCheckError::WrongKind(k, types.to_kind()))}
            if !(k == Kind::Ty) {
                let t_kind = types.to_kind();
                if !compatible_type_app_kinds(&t_kind, &k) {return Err(TypeCheckError::WrongKind(k, types.to_kind()))}
            }
            Ok((env, a.replace_target(&Type::TypeVariable(x), &types)))
        },
        _ => if typ == types  {return Ok((env, typ))} else {return Err(TypeCheckError::WrongType(typ, types))}
    }
}

pub fn typecheck_record_selection(env: TypeEnvironment, record: &Term, target: &Label) -> TypeCheckResult {
    let (env, record_type) = typecheck(env, record)?;
    match record_type.clone() {
        Type::RecordsType(variants, _dynamic) => {
            let var_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(variants.clone());
            let occurrence = var_map.get(target);
            match occurrence {
                Some(occurrence) => match occurrence {
                    FieldOccurrence::Absent => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
                    FieldOccurrence::Present(types) => Ok((env, types.clone())),
                    FieldOccurrence::Star => Ok((env, Type::DynType)),
                }
                None => Err(TypeCheckError::LabelNotPresentInType(record_type, target.clone())),
            }
        },
        _ => Err(TypeCheckError::WrongType(record_type, unit()))
    }
}

pub fn typecheck_record_update(env: TypeEnvironment, record: &Term, target: &Label, new_type: &Term) -> TypeCheckResult {
    let (env, record_type) = typecheck(env, record)?;
    let (env, new_type_result) = typecheck(env, new_type)?;
    match record_type.clone() {
        Type::RecordsType(variants, dynamic) => {
            let mut var_map: HashMap<String, FieldOccurrence> = HashMap::from_iter(variants.clone());
            var_map.remove(target);

            var_map.insert(target.clone(), FieldOccurrence::Present(new_type_result));
            let mut record_vec = vec![];
            for (label, types) in var_map {
                record_vec.push((label, types));
            }

            let rec = Type::RecordsType(record_vec, dynamic).normalize();
            Ok((env, rec))
        },
        _ => Err(TypeCheckError::WrongType(record_type, unit()))
    }
}

fn compare_types(first: &Type, second: &Type) -> bool {
    first.is_dyn() || first.equal(second)
}

pub fn typecheck_function_call(env: TypeEnvironment, term: &Term, parameter: &Term) -> TypeCheckResult {
    let (env, function_type) = typecheck(env, term)?;
    let (env, parameter_type) = typecheck(env, parameter)?;
    match function_type {
        Type::FunctionType(input, output) => if compare_types(&input,&parameter_type) {return Ok((env, *output.clone()))} else {return Err(TypeCheckError::WrongType(Type::FunctionType(input, output), Type::FunctionType(Box::new(parameter_type), Box::new(unit()))))},
        _ => Err(TypeCheckError::WrongType(function_type, Type::FunctionType(Box::new(unit()), Box::new(unit()))))
    }

}

pub fn typecheck_anonymous_function(env: TypeEnvironment, parameter: &Var, parameter_type: &Type, body: &Term) -> TypeCheckResult {
    let (_, ret_type) = typecheck(env.new_child_with(HashMap::from([(parameter.clone(), parameter_type.clone())]), HashMap::new()), body)?;
    Ok((env, Type::FunctionType(Box::new(parameter_type.clone()), Box::new(ret_type.normalize()))))
}


pub fn typecheck_anonymous_recursive_function(env: TypeEnvironment, label: &Var, typ: &Type, parameter: &Var, parameter_type: &Type, body: &Term) -> TypeCheckResult {
    let mut param_map = HashMap::from([(parameter.clone(), parameter_type.clone())]);
    param_map.insert( label.clone(), Type::FunctionType(Box::new(parameter_type.clone()), Box::new(typ.clone())));


    let initial_result = typecheck(env.new_child_with(param_map, HashMap::new()), body);

    // Try to typecheck the function
    let final_result = match initial_result {
        Ok((_, Type::TechnicalType(TechnicalType::YetUnknownRecursiveType))) => Err(TypeCheckError::TypeOfFunctionCannotBeDetermined(label.clone())),
        // If the result is okay simply return the result
        Ok((_, ret_type)) => Ok((env, Type::FunctionType(Box::new(parameter_type.clone()), Box::new(ret_type)))),
        // If we get a generic error just return it
        Err(err) => Err(err),
    };
    final_result

}


pub fn typecheck_function(env: TypeEnvironment, label: &Var, typ: &Type, parameter: &Var, parameter_type: &Type, body: &Term) -> TypeCheckResult {
    let (mut fun_env, fun_type) = typecheck_anonymous_recursive_function(env, label, typ, parameter, parameter_type, body)?;
    fun_env.insert(label.clone(), fun_type);
    Ok((fun_env, unit()))
}

pub fn typecheck_record_construction(env: TypeEnvironment, variant_vec: &Vec<RawVariant>) -> TypeCheckResult {
    fn trc_rec(env: TypeEnvironment, vv: &Vec<RawVariant>, index: usize, mut lo: Vec<LabelOccurrence>) -> Result<(TypeEnvironment, Vec<LabelOccurrence>), TypeCheckError> {
        if index == vv.len() {
            return Ok((env, lo));
        }
        let (label, term) = &vv[index];
        let (env, types) = typecheck(env, &term)?;
        lo.push((label.clone(), FieldOccurrence::Present(types)));
        trc_rec(env, vv, index+1, lo)

    }
    let (env, occurrence_vec) = trc_rec(env, variant_vec, 0, vec![])?;
    Ok((env, Type::RecordsType(occurrence_vec, RecordAndVariantEnd::Closed).normalize()))
}

pub fn typecheck_variant_construction(env: TypeEnvironment, variant: &RawVariant) -> TypeCheckResult {
    let (label, term) = variant;
    let (term_env, term_type) = typecheck(env, term)?;
    Ok((term_env, Type::VariantType(vec![(label.to_string(), FieldOccurrence::Present(term_type))], RecordAndVariantEnd::Closed)))
}

pub fn typecheck_logicgate(env: TypeEnvironment, lt: &LogicTerm) -> TypeCheckResult {
    let (fst_env, fst_type) = typecheck(env, lt.fst())?;
    let (snd_env, snd_type) = typecheck(fst_env, lt.snd())?;

    let resulting_type = lt.resulting_type(&fst_type,& snd_type);
    if resulting_type == Type::TechnicalType(TechnicalType::IllegalType) {if lt.is_binary_operation() {return Err(TypeCheckError::IncompatibleBinOperation(fst_type, lt.operation_name(), snd_type))} else 
        {return Err(TypeCheckError::IncompatibleOperation(lt.operation_name(), fst_type))}
    }
    return Ok((snd_env, resulting_type))

}

pub fn typecheck_if(env: TypeEnvironment, decider: &Term, consequence: &Term, alternative: &Term) -> TypeCheckResult {
    let (decider_env, decider_type) = typecheck(env, decider)?;

    // Don't type-check the decider when predicting the type, since that could throw unnecessary errors - Whether the decider is boolean will be checked once we have determined a persumed type
    if !(decider_type.equal(&Type::BaseType(BaseType::BoolType)) || decider_type.is_dyn()) {return Err(TypeCheckError::WrongType(Type::BaseType(BaseType::BoolType), decider_type)); }

    let (consequence_env, consequence_type) = typecheck(decider_env, consequence)?;
    let (alternative_env, alternative_type) = typecheck(consequence_env, alternative)?;

    if consequence_type.equal(&alternative_type) {return Ok((alternative_env, alternative_type)); } else {return Err(TypeCheckError::IncompatibleTypes(consequence_type, alternative_type));}

}

pub fn typecheck_let(env: TypeEnvironment, var: &Var, term: &Term) -> TypeCheckResult {
    let (mut type_env, term_type) = typecheck(env, term)?;
    type_env.insert(var.to_string(), term_type);
    Ok((type_env, unit()))
}

pub fn typecheck_variable(env: TypeEnvironment, var: &Var) -> TypeCheckResult {
    match env.get(var) {
        Some(typ) => match typ {
            // Type::TypeVariable(type_var) => typecheck_typevar(env, &type_var),
            _ => Ok((env, typ))
        },
        None => Err(TypeCheckError::TypeOrVarNotFound(var.clone())),
    }
}
pub fn typecheck_cast(env: TypeEnvironment, term: &Term, type1: &Type, _blame_label: &BlameLabel, type2: &Type) -> TypeCheckResult {
    let type1 = &type1.normalize();
    let type2 = &type2.normalize();

    let (env, term_type) = typecheck(env, term)?;

    if !term_type.equal(type1) {
        return Err(TypeCheckError::IncompatibleTypes(term_type, type1.clone()))
    }

    wellformed_types(&env, type2, &Kind::Ty)?;

    consistent_type(&env, type1, type2)?;


    Ok((env, type2.clone()))
}

pub fn typecheck_convertion(env: TypeEnvironment, term: &Term, type1: &Type, convertion_label: &ConvertionLabel, type2: &Type) -> TypeCheckResult {
    let type1 = &type1.normalize();
    let type2 = &type2.normalize();

    let (env, term_type) = typecheck(env, term)?;

    if !term_type.equal(type1) {
        return Err(TypeCheckError::IncompatibleTypes(term_type, type1.clone()))
    }

    let env = convertible_type(env, type1,  convertion_label, type2)?;

    wellformed_types(&env, type2, &Kind::Ty)?;


    Ok((env, type2.clone()))
}
