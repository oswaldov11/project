(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | Fnegate
;;
    
type binop =
  | Plus
  | Fplus
  | Minus
  | Fminus
  | Times
  | Ftimes
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) ->
    SS. union (SS.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Fun (x, e) -> SS.remove x (free_vars exp)
  | Let (x, e1, e2) -> SS.union (free_vars e1) (SS.remove x (free_vars e2))
  | Letrec (x, e1, e2) ->
    SS.union (SS.remove x (free_vars e1)) (SS.remove x (free_vars e2))
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid =
  let ctr = ref 0 in
  fun () ->
    let name = "x" ^ string_of_int !ctr in
    incr ctr;
    name ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Var x -> if x = var_name then repl else exp
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> exp
  | Unop (u, e) -> Unop (u, subst var_name repl e)
  | Binop (b, e1, e2) ->
    Binop (b, subst var_name repl e1, subst var_name repl e2)
  | Conditional (e1, e2, e3) ->
    Conditional (subst var_name repl e1,
                 subst var_name repl e2, subst var_name repl e3)
  | Fun (x, e) ->
    if x = var_name then exp
    else 
      if SS.mem x (free_vars repl) then
        let x' = new_varname () in
        Fun (x', subst var_name repl (subst x (Var x') e))
      else Fun(x, subst var_name repl e)
  | Let (x, e1, e2) ->
    if x = var_name then Let (x, subst var_name repl e1, e2)
    else
      if SS.mem x (free_vars repl) then
        let x' = new_varname () in
        Let (x', subst var_name repl e1,
             subst var_name repl (subst x (Var (x')) e2))
      else Let (x, subst var_name repl e1, subst var_name repl e2)
  | Letrec (x, e1, e2) ->
    if x = var_name then exp
    else
      if SS.mem x (free_vars repl) then
        let x' = new_varname () in
        Letrec (x', subst var_name repl (subst x (Var x') e1),
                subst var_name repl (subst x (Var x') e2))
      else Letrec (x, subst var_name repl e1, subst var_name repl e2)
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)
;;

(*......................................................................
  String representations of expressions
 *)
   
let string_of_unop (u : unop) : string =
  match u with
  | Negate -> "Negate"
  | Fnegate -> "Fnegate" ;;

let string_of_binop (b : binop) : string =
  match b with
  | Plus -> "Plus"
  | Fplus -> "Fplus"
  | Minus -> "Minus"
  | Fminus -> "Fminus"
  | Times -> "Times"
  | Ftimes -> "Ftimes"
  | Equals -> "Equals"
  | LessThan -> "LessThan" ;;
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> x
  | Num x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x
  | Unop (u, e) -> string_of_unop u ^ exp_to_concrete_string e
  | Binop (b, e1, e2) ->
    "(" ^ exp_to_concrete_string e1 ^ " "
    ^ string_of_binop b ^ " " ^ exp_to_concrete_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
    "if " ^ exp_to_concrete_string e1 ^ " then "
    ^ exp_to_concrete_string e2 ^ " else " ^ exp_to_concrete_string e3
  | Fun (x, e) -> "fun " ^ x ^  " -> " ^ exp_to_concrete_string e
  | Let (x, e1, e2) ->
    "let " ^ x ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^
    exp_to_concrete_string e2
  | Letrec (x, e1, e2) ->
    "let rec " ^ x ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^
    exp_to_concrete_string e2
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2
;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Float x -> "Float(" ^ string_of_float x ^ ")"
  | Bool x -> "Bool(" ^ string_of_bool x ^ ")"
  | Unop (u, e) -> 
    "Unop(" ^ string_of_unop u ^ ", "  ^ exp_to_abstract_string e ^ ")"
  | Binop (b, e1, e2) ->
    "Binop(" ^ string_of_binop b ^ ", " ^ exp_to_abstract_string e1 ^ ", "
    ^ exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
    "Conditional(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2
    ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (x, e) -> "Fun(" ^ x ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (x, e1, e2) ->
    "Let(" ^ x ^ ", " ^ exp_to_abstract_string e1 ^ ", "
    ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (x, e1, e2) ->
    "Letrec(" ^ x ^ ", " ^ exp_to_abstract_string e1 ^ ", "
    ^ exp_to_abstract_string e2 ^ ")" 
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) ->
    "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
;;