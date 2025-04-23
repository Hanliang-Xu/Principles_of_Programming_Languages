open Fbast

exception Bug;;

let rec subst (e, v2, x) =
  match e with
  |  Int int -> Int int
  |  Bool bool -> Bool bool
  |  Var ident -> if (ident = x) then v2 else Var ident

  |  Plus(e1, e2) -> Plus(subst (e1, v2, x), subst (e2, v2, x))
  |  Minus(e1, e2) -> Minus(subst (e1, v2, x), subst (e2, v2, x))
  |  Equal(e1, e2) -> Equal(subst (e1, v2, x), subst (e2, v2, x))

  |  And(e1, e2) -> And(subst (e1, v2, x), subst (e2, v2, x))
  |  Or(e1, e2) -> Or(subst (e1, v2, x), subst (e2, v2, x))
  |  Not(e) -> Not(subst (e, v2, x))
  |  If(e1, e2, e3) -> If(subst (e1, v2, x), subst (e2, v2, x), subst (e3, v2, x))
  
  |  Function(y, e) -> if (x <> y) then Function(y, subst (e, v2, x)) else Function(y, e)
  |  Appl(e1, e2) -> Appl(subst (e1, v2, x), subst (e2, v2, x))
  |  Let(y, e1, e2) -> if (x <> y) then Let(y, subst (e1, v2, x), subst (e2, v2, x)) else Let(y, e1, e2)
  |  LetRec(f, x, e1, e2) ->
    Int 0
    
let rec check_closed (e, bound_variables) =
  match e with
  |  Int _ | Bool _ -> ()
  |  Var ident -> if List.mem ident bound_variables then () else raise Bug

  |  Plus(e1, e2) | Minus(e1, e2) | Equal(e1, e2) | And(e1, e2) | Or(e1, e2) ->
    check_closed(e1, bound_variables);
    check_closed(e2, bound_variables)
  |  Not(e) ->
    check_closed(e, bound_variables)
  |  If(e1, e2, e3) ->
    check_closed(e1, bound_variables);
    check_closed(e2, bound_variables);
    check_closed(e3, bound_variables)
  |  Function(y, e) ->
    check_closed(e, y :: bound_variables)
  |  Appl(e1, e2) -> check_closed(e1, bound_variables); check_closed(e2, bound_variables)
  |  Let(x, e1, e2) ->
    check_closed(e1, bound_variables);
    check_closed(e2, x :: bound_variables)
  | LetRec(f, x, e1, e2) ->
    ()


(*
 * Replace this with your interpreter code.
 *)
let rec eval e =
  match e with
  |  Int int -> Int int
  |  Bool bool -> Bool bool
  |  Var ident -> raise Bug

  |  Plus(e1, e2) ->
    (let (v1, v2) = (eval(e1), eval(e2)) in
    match (v1, v2) with
    |  (Int int1, Int int2) -> Int (int1 + int2)
    |  _ -> raise Bug)
  |  Minus(e1, e2) ->
    (let (v1, v2) = (eval(e1), eval(e2)) in
    match (v1, v2) with
    |  (Int int1, Int int2) -> Int (int1 - int2)
    |  _ -> raise Bug)
  |  Equal(e1, e2) ->
    (let (v1, v2) = (eval(e1), eval(e2)) in
    match (v1, v2) with
    |  (Int int1, Int int2) -> Bool (int1 = int2)
    |  _ -> raise Bug)
  
  |  And(e1, e2) ->
    (let (v1, v2) = (eval(e1), eval(e2)) in
    match (v1, v2) with
    |  (Bool bool1, Bool bool2) -> Bool(bool1 && bool2)
    |  _ -> raise Bug)
  |  Or(e1, e2) ->
    (let (v1, v2) = (eval(e1), eval(e2)) in
    match (v1, v2) with
    |  (Bool bool1, Bool bool2) -> Bool(bool1 || bool2)
    |  _ -> raise Bug)
  |  Not(e) ->
    (let (v) = (eval(e)) in
    match (v) with
    |  (Bool bool) -> Bool(not bool)
    |  _ -> raise Bug)
  |  If(e1, e2, e3) ->
    (let condition = (eval(e1)) in
    match condition with
    |  (Bool true) -> (eval(e2))
    |  (Bool false) -> (eval(e3))
    |  _ -> raise Bug)
  
  |  Function(x, e) -> check_closed (e, [x]); Function(x, e)
  |  Appl(e1, e2) ->
    (let v1 = eval e1 in
    let v2 = eval e2 in
    match v1 with
    |  Function(x, e3) -> eval(subst (e3, v2, x))
    |  _ -> raise Bug)
  
  |  Let(x, e1, e2) -> eval(subst (e2, eval(e1), x))

  | LetRec(f, x, e1, e2) ->
    Int 0