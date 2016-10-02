(* ************************************************************************* *
 * L-System types
 *)

(* Some elements of L-Systems can be replaced (Variables from alphabet 'a)
   and some cannot (Constants from alphabet 'b) *)
type ('a, 'b) element =
    | Var of 'a
    | Const of 'b ;;

(* Type for current state of L-System. The system's axiom is its IC state *)
type ('a, 'b) state = (('a, 'b) element) list ;;

(* Rules associate elements of the Variable type with states *)
type ('a, 'b) rule = 'a * ('a, 'b) state ;;

(* L-Systems just have to define their axiom and production rules *)
type ('a, 'b) l_system = {
    axiom : ('a, 'b) state ;
    rules : ('a, 'b) rule list ;
} ;;


(* ************************************************************************* *
 * Production
 *)

(* Run the L-System for @n generations *)
let produce (system : ('a, 'b) l_system) (n : int) : ('a, 'b) state =
    (* helper: find and apply @x's rule *)
    let rec do_rule (x : 'a) (rules : (('a, 'b) rule) list) =
        match rules with
        | [] -> 
        raise (Failure "Incomplete rule set.")
        | (y, st)::rest -> 
        if x == y then st else do_rule x rest
    in
    (* helper: find this state's successor *)
    let rec advance state rules acc : ('a, 'b) state = 
        match state with
        | [] -> 
        acc
        | (Var x)::tl ->
        advance tl rules (acc @ (do_rule x rules))
        | (Const x)::tl ->
        advance tl rules (acc @ [Const x])
    in
    (* main recursive helper *)
    let rec iterate state rules n : ('a, 'b) state =
        match n with
        | 0 -> state
        | _ -> iterate (advance state rules []) rules (n - 1)
    in iterate system.axiom system.rules n ;;


(* ************************************************************************* *
 * Examples
 *)

(* Algae *)
type two_alphabet = X | Y ;;
let algae : (two_alphabet, unit) l_system = {
    axiom = [Var X] ;
    rules = [
             (Y, [Var X]);
             (X, [Var X; Var Y])
            ]
} ;;
let bloom = produce algae 10 ;;

(* Dragon *)
type square_draw_constants = DrawForward | Left90 | Right90 ;;
let dragon : (two_alphabet, square_draw_constants) l_system = {
    axiom = [Const DrawForward; Var X] ;
    rules = [
             (X, [Var X; Const Right90; Var Y; Const DrawForward; Const Right90]);
             (Y, [Const Left90; Const DrawForward; Var X; Const Left90; Var Y])
            ]
} ;;
let a_dragon = produce dragon 10 ;;