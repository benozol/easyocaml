open EzyOcamlmodules
open EzyTypingCoreTypes
open EzyErrors
open EzyConstraints
open EzyEnrichedAst

val unify :
  EzyEnv.t -> AtConstrSet.t ->
  ((Ty.t * ExtLocationSet.t) TyVarMap.t, type_error * ExtLocationSet.t * ExtLocationMap.key) EzyUtils.Result.t


val minimize :
  EzyEnv.t -> AtConstrSet.t -> ExtLocationSet.printable -> ExtLocationMap.key ->
  type_error * ExtLocationSet.t

val enum :
  EzyEnv.t -> float -> AtConstrSet.t ->
  (type_error * ExtLocationSet.t) list

val solve :
  EzyEnv.t -> float -> AtConstrSet.t -> EzyGenerate.PostProcess.t -> 'a ->
  (TyVarSubst.t * ErrorSet.t, ErrorSet.t) EzyUtils.Result.t

val type_expression :
  Env.t -> 'a -> EzyAst.imported_expression -> 'b ->
  generated_expression * TyVarSubst.t

val type_implementation :
  string -> Env.t -> EzyAst.imported_structure ->
  generated_structure * TyVarSubst.t * EzyEnv.t

val type_structure :
  ?program:'a -> Env.t -> EzyAst.imported_structure ->
  generated_structure * TyVarSubst.t * EzyEnv.t

val type_and_compare_implementation :
  string -> string -> string -> Env.t -> Parsetree.structure -> EzyFeatures.program_feats ->
  Typedtree.structure * Typedtree.module_coercion

val type_and_compare_top_phrase :
  EzyFeatures.program_feats -> Env.t ->
  Parsetree.structure -> Typedtree.structure * Types.signature * Env.t
