(*                           Unit Tests for MiniML                           *)


open Expr ;;
open Evaluation ;;
open Miniml ;;

(* Test exp_to_abstract_string and float DT *)

let exprs =
  [ "51 ;;";
    "51. ;;";
    "49 + 1 * 2 ;;";
    "49. +. 1. *. 2. ;;";
    "let f = fun x -> x in f 51 ;;";
    "let intofbool = fun b -> if b then 1 else 0 in intofbool true ;;" ] ;;

let abs_strs =
  [ "Num(51)";
    "Float(51.)";
    "Binop(Plus, Num(49), Binop(Times, Num(1), Num(2)))";
    "Binop(Fplus, Float(49.), Binop(Ftimes, Float(1.), Float(2.)))";
    "Let(f, Fun(x, Var(x)), App(Var(f), Num(51)))";
    "Let(intofbool, Fun(b, Conditional(Var(b), Num(1), Num(0))), App(Var(intofbool), Bool(true)))" ] ;;

let rec test_abs_strs (exprs : string list) (abs_strs : string list) : unit =
  match exprs, abs_strs with
  | (hd1 :: tl1, hd2 :: tl2) ->
    assert (exp_to_abstract_string (str_to_exp hd1) = hd2);
    test_abs_strs tl1 tl2
  | _ -> () ;;
  
(* Test free_vars *)

let free_vars_exprs =
  [ "fun y -> f (x + y) ;;";
    "let x = 51 in let y = x in f x y ;;";
    "let x = fun y -> x in x ;;";
    "fun x -> 3 * (f y) + x ;;"] ;;

let free =
  [ ["f"; "x"];
    ["f"];
    ["x"];
    ["f"; "y"] ] ;;

let rec test_free_vars (fvexprs : string list) (free : string list list) : unit =
  match fvexprs, free with
  | (hd1 :: tl1, hd2 :: tl2) ->
    assert (same_vars (vars_of_list hd2) (free_vars (str_to_exp hd1)));
    test_free_vars tl1 tl2
  | _ -> () ;;

(* Test the substitution model *)

let sub_exprs =
  [ "let f = fun x -> x in f 51 ;;";
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 10 ;;";
    "let x = 2 in let f = fun y -> x * y in let x = 1 in f 21 ;;";
    "let x = 10 in let f = fun y -> fun z -> z * (x + y) in let y = 12 in f 11 2 ;;" ] ;;

let sub_res = ["51 ;;"; "3628800 ;;"; "42 ;;"; "42 ;;"] ;;

let rec test_eval_s (exprs : string list) (res : string list) : unit =
  match exprs, res with
  | (hd1 :: tl1, hd2 :: tl2) ->
    assert (eval_s (str_to_exp hd1) (Env.empty ()) = Env.Val (str_to_exp hd2));
    test_eval_s tl1 tl2
  | _ -> () ;;

(* Test the environment model *)

let env_exprs =
  [ "let f = fun x -> x in f 51 ;;";
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 10 ;;";
    "let x = 2 in let f = fun y -> x * y in let x = 1 in f 21 ;;";
    "let x = 10 in let f = fun y -> fun z -> z * (x + y) in let y = 12 in f 11 2 ;;" ] ;;

let env_res = ["51 ;;"; "3628800 ;;"; "21 ;;"; "44 ;;"] ;;

let rec test_eval_d (exprs : string list) (res : string list) : unit =
  match exprs, res with
  | (hd1 :: tl1, hd2 :: tl2) ->
    assert (eval_d (str_to_exp hd1) (Env.empty ()) = Env.Val (str_to_exp hd2));
    test_eval_d tl1 tl2
  | _ -> () ;;

let _ =
  test_abs_strs exprs abs_strs ;;
  test_free_vars free_vars_exprs free ;;
  test_eval_s sub_exprs sub_res ;;
  test_eval_d env_exprs env_res ;;