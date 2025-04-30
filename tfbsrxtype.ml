open Tfbsrxast
open Tfbsrxpp

exception TypecheckerNotImplementedException;;

type env = (ident * fbtype) list;;

let extend_env (x : ident) (t : fbtype) (env : env) : env = (x, t) :: env
let rec lookup_env (x : ident) (env : env) : fbtype option =
  match env with
  |  [] -> None
  |  (y, t) :: tl -> if x = y then Some t else lookup_env x tl

(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = true;;

let rec typecheck_with_env (env : env) (e : expr): fbtype =
  match e with
  |  Int _ -> TInt
  |  Bool _ -> TBool
  |  Var x ->
      (match (lookup_env x env) with
      |  Some t -> t
      |  None -> failwith ("Unbound variable: " ^ show_ident x))
  |  Plus (e1, e2) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      if (equal_fbtype t1 TInt) && (equal_fbtype t2 TInt) then TInt
      else failwith ("Type error in Plus: expected two integer operands")
  |  Minus (e1, e2) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      if (equal_fbtype t1 TInt) && (equal_fbtype t2 TInt) then TInt
      else failwith ("Type error in Minus: expected two integer operands")
  |  Equal (e1, e2) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      if (equal_fbtype t1 TInt) && (equal_fbtype t2 TInt) then TBool
      else failwith ("Type error in Equal: expected two integer operands")
  |  And (e1, e2) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      if (equal_fbtype t1 TBool) && (equal_fbtype t2 TBool) then TBool
      else failwith ("Type error in And: expected two boolean operands")
  |  Or (e1, e2) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      if (equal_fbtype t1 TBool) && (equal_fbtype t2 TBool) then TBool
      else failwith ("Type error in Or: expected two boolean operands")
  |  Not e ->
      if (typecheck_with_env env e  = TBool) then TBool
      else failwith ("Type error in Not: expected boolean operand")
  |  If (e1, e2, e3) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      let t3 = typecheck_with_env env e3 in
      if not (equal_fbtype t1 TBool) then failwith ("Type error in If: expected the first expression to be TBool")
      else if not (equal_fbtype t2 t3) then failwith ("Type error in If: expected the second expression to match the third")
      else t3
  |  Function (x, tx, body) ->
      let extended_env = extend_env x tx env in
      let body_type = typecheck_with_env extended_env body in
      TArrow (tx, body_type)
  |  Appl (e1, e2) ->
      let t1 = typecheck_with_env env e1 in
      let t2 = typecheck_with_env env e2 in
      (match t1 with
      |  TArrow (arg, ret) ->  if (equal_fbtype arg t2) then ret
                               else failwith ("Type error in Appl: expected the function's argument type to match the actual argument's type")
      |  _ -> failwith ("Type error in Appl: expected the first argument to be a function"))
  
  | LetRec (f, x, tx, e1, te1, e2) ->
      let extended_env_fun = extend_env f (TArrow (tx, te1)) env in
      let extended_env_body = extend_env x tx extended_env_fun in
      let t1 = typecheck_with_env extended_env_body e1 in
      if not (equal_fbtype t1 te1) then
        failwith ("Type error in LetRec: e doesn't match te1")
      else
        typecheck_with_env extended_env_fun e2

  |  Record fields ->
      let typed_fields =
        List.map (fun (lab, e) ->
          (lab, typecheck_with_env env e)
        ) fields
      in
      TRec typed_fields
  
  |  Select (field, expr) ->
      match expr with
      |  [] -> failwith ("Field not found in the record")
      |  (lab, selected) :: tl -> if (lab = field)
                                    then selected
                                  else typecheck_with_env env tl

  |  _ -> raise TypecheckerNotImplementedException


let typecheck e = typecheck_with_env [] e;;
