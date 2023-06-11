(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
  | Concat
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | String of string                     (* strings *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  (*| Concat of expr * expr                string concatenation *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  (* variables *)
  | Var x -> SS.add x SS.empty
  (* integers and other literals *)
  | Num _ | Bool _ | String _ | Raise | Unassigned -> SS.empty
  (* unary operators *)
  | Unop (_, exp) -> free_vars exp
  (* binary operators *)
  | Binop (_, exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  (* Conditionals *)
  | Conditional (exp1, exp2, exp3) ->
    SS.union (SS.union (free_vars exp1) (free_vars exp2)) (free_vars exp3)
  (* functions *)
  | Fun (x, exp) -> SS.diff (free_vars exp) (free_vars (Var x))
  (* binding *)
  | Let (x, exp1, exp2) ->
    SS.union (SS.diff (free_vars exp2) (free_vars (Var x))) (free_vars exp1)
  (* applications *)
  | App (exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  (* recursive binding *)
  | Letrec (x, exp1, exp2) ->
    SS.diff (free_vars (Let (x, exp1, exp2))) (free_vars (Var x))
;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let ctr = ref 0 ;;

let new_varname () : varid =
  let v = "var" ^ string_of_int (!ctr) in
  ctr := !ctr + 1;
  v ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let subst_help = subst var_name repl in
  match exp with
  (* variables *)
  | Var x -> if x = var_name then repl else exp
  (* integers, booleans, exceptions and temporarily unassigned *)
  | Num _ | Bool _ | String _ | Raise | Unassigned -> exp
  (* unary operators *)
  | Unop (op, e) -> Unop (op, subst_help e)
  (* binary operators *)
  | Binop (op, e1, e2) -> Binop (op, subst_help e1, subst_help e2)
  (* if then else *)
  | Conditional (e1, e2, e3) -> Conditional (subst_help e1, subst_help e2, subst_help e3)
  (* function definitions *)
  | Fun (x, e) ->
    if x = var_name then exp
    else if SS.mem x (free_vars repl) then
      let variable = new_varname () in
      Fun (variable, subst_help (subst x (Var variable) e))
    else Fun (x, subst_help e)
  (* local binding *)
  | Let (x, def, body) ->
    if x = var_name then Let (x, subst_help def, body)
    else if SS.mem x (free_vars repl) then
      let variable = new_varname () in
      Let (variable, subst_help def, subst_help (subst x (Var variable) body))
    else Let (x, subst_help def, subst_help body)
  (* function applications *)
  | App (e1, e2) -> App (subst_help e1, subst_help e2)
  (* recursive local naming *)
  | Letrec (x, def, body) ->
    if x = var_name then exp
    else if SS.mem x (free_vars repl) then
      let variable = new_varname () in
      let x_to_new = subst x (Var variable) in
      Letrec (variable, subst_help (x_to_new def), subst_help (x_to_new body))
    else Letrec (x, subst_help def, subst_help body)
;;
     
(*......................................................................
  String representations of expressions
 *)

(* Returns a concrete string representation of a binop  *)
let binop_to_conc_str (op : binop) =
  match op with
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equals -> " = "
  | LessThan -> " < "
  | Concat -> " ^ "

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  (* variables *)
  | Var x -> x
  (* integers *)
  | Num n -> string_of_int n
  (* booleans *)
  | Bool b -> string_of_bool b
  (* strings *)
  | String s -> "\"" ^ s ^ "\"" 
  (* unary operators *)
  | Unop (Negate, uexp) -> "~-(" ^ exp_to_concrete_string uexp ^ ")"
  (* binary operators *)
  | Binop (op, exp1, exp2) -> "(" ^ (exp_to_concrete_string exp1) ^
                              (binop_to_conc_str op) ^
                              (exp_to_concrete_string exp2) ^ ")"
  (* if then else *)
  | Conditional (cexp1, cexp2, cexp3) ->
    "if " ^ exp_to_concrete_string cexp1 ^
    " then " ^ exp_to_concrete_string cexp2 ^
    " else " ^ exp_to_concrete_string cexp3
  (* function definitions *)
  | Fun (x, expr) -> "(fun " ^ x ^ " -> " ^ exp_to_concrete_string expr ^ ")"
  (* local naming *)
  | Let (x, exp1, exp2) ->
    "let " ^ x ^ " = " ^ exp_to_concrete_string exp1 ^
    " in " ^ exp_to_concrete_string exp2
  (* recursive local naming *)
  | Letrec (x, exp1, exp2) ->
    "let rec " ^ x ^ " = " ^ exp_to_concrete_string exp1 ^
    " in " ^ exp_to_concrete_string exp2
  (* exceptions *)
  | Raise -> "exception raised"
  (* (temporarily) unassigned *)
  | Unassigned -> "unassigned"
  (* function applications *)
  | App (e1, e2) ->
    exp_to_concrete_string e1 ^ "(" ^ exp_to_concrete_string e2 ^ ")"
  ;;

(* Returns an abstract string representation of a binop  *)
let binop_to_abstr_str (op : binop) : string = 
  match op with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "LessThan"
  | Concat -> "Concat"

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  (* variables *)
  | Var x -> "Var " ^ x
  (* integers *)
  | Num n -> "Num " ^ string_of_int n
  (* strings *)
  | String s -> "String(" ^ s ^ ")"
  (* booleans *)
  | Bool b -> "Bool " ^ string_of_bool b
  (* unary operators *)
  | Unop (Negate, uexp) -> "Unop(Negate, " ^ exp_to_abstract_string uexp ^ ")"
  (* binary operators *)
  | Binop (op, exp1, exp2) -> "Binop(" ^ (binop_to_abstr_str op) ^ ", " ^
                              (exp_to_abstract_string exp1) ^ ", " ^
                              (exp_to_abstract_string exp2) ^ ")"
  (* if then else *)
  | Conditional (cexp1, cexp2, cexp3) ->
    "Conditional(" ^ exp_to_abstract_string cexp1 ^ ", "
                   ^ exp_to_abstract_string cexp2 ^ ", "
                   ^ exp_to_abstract_string cexp3 ^ ")"
  (* function definitions *)
  | Fun (x, expr) -> "Fun(" ^ x ^ ", " ^ exp_to_abstract_string expr ^ ")"
  (* local naming *)
  | Let (x, exp1, exp2) ->
    "Let(" ^ x ^ ", " ^ exp_to_abstract_string exp1 ^
                  ", " ^ exp_to_abstract_string exp2 ^ ")"
  (* recursive local naming *)
  | Letrec (x, exp1, exp2) ->
    "Letrec(" ^ x ^ ", " ^ exp_to_abstract_string exp1 ^
                     ", " ^ exp_to_abstract_string exp2 ^ ")"
  (* exceptions *)
  | Raise -> "exception raised"
  (* (temporarily) unassigned *)
  | Unassigned -> "unassigned"
  (* function applications *)
  | App (e1, e2) ->
    "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
;;