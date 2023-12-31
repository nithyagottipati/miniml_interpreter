(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env) 

    let lookup (env : env) (varname : varid) : value =
      match List.find_opt (fun n -> (fst n) = varname) env with
      | Some v -> !(snd v)
      | None -> raise (EvalError "lookup: unbound variable")

    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: env
      
    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val x -> "Val (" ^ exp_to_abstract_string x ^ ")"
      | Closure (x, env) ->
        let string = exp_to_abstract_string x in
        "Closure " ^
        if printenvp then " (" ^ string ^ ", " ^ env_to_string env ^ ")"
        else "(" ^ string ^ "); " 

    and env_to_string (env : env) : string =
      match env with
      | [] -> ""
      | hd :: tl ->
        "(" ^ fst hd ^ ", " ^ value_to_string !(snd hd) ^ "); " ^
        env_to_string tl ;; 
  end
;;

open Env ;;

(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* helper function to concatenate strings *)
let concat_eval (e1 : value) (e2 : value) =
  match e1, e2 with 
  | Val (String s1), Val (String s2) -> let new_s1 = String.sub s1 (1) ((String.length s1) - 2) in
                                        let new_s2 = String.sub s2 (1) ((String.length s2) - 2) in
                                        Val(String({|"|} ^ new_s1 ^ new_s2 ^ {|"|}))
  | _, _ -> raise (EvalError "can't concatenate non-strings")
;;


(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* Evaluates basic exps for all models; specific expressions evals are later *)
let rec evaluate (eval) (exp : expr) (env : Env.env) =
  let extract (exp : expr) =
    match eval exp env with
    | Val x -> x
    | _ -> raise (EvalError "evaluate: error occurred")
  in
  match exp with
  | Var x -> lookup env x
  | Num _ | Bool _ | String _ | Raise | Unassigned -> Val exp   
  | Fun _ -> Val exp
  | Conditional (e1, e2, e3) ->
    (match eval e1 env with
     | Val (Bool b) -> if b then eval e2 env else eval e3 env
     | _ -> raise (EvalError "evaluate: conditional error"))
  | Unop (Negate, e) -> evaluate eval (Binop (Times, Num ~-1, extract e)) env
  | Binop (op, exp1, exp2) ->
    let num_op n =
      match extract exp1, extract exp2 with
      | Num n1, Num n2 -> Num (n n1 n2)
      | _ -> raise (EvalError "evaluate: only int acceptable")
    in
    (match op with
     | Plus -> Val (num_op (+))
     | Minus -> Val (num_op (-))
     | Times -> Val (num_op ( * ))
     | Concat -> concat_eval (eval exp1 env) (eval exp2 env)  
     | LessThan ->
       (match extract exp1, extract exp2 with
        | Bool b1, Bool b2 -> Val (Bool (b1 < b2))
        | Num n1, Num n2 -> Val (Bool (n1 < n2))
        | _ -> failwith "evaluate: only int acceptable")
     | Equals ->
       match extract exp1, extract exp2 with
       | Bool b1, Bool b2 -> Val (Bool (b1 = b2))
       | Num n1, Num n2 -> Val (Bool (n1 = n2))
       | String s1, String s2 -> Val (Bool (s1 = s2))
       | _ -> failwith "evaluate: only int acceptable")
  | Let (id, def, body) -> eval body (extend env id (ref (eval def env)))
  | _ -> raise (EvalError "evaluate: function can not handle") ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
   
let rec eval_s (_exp : expr) (_env : Env.env) : Env.value =
  let reval exp = eval_s exp _env in
  let extract_s (exp : expr) =
    match reval exp with
    | Val x -> x
    | _ -> raise (EvalError "extract_s: error occurred")
  in
  match _exp with
  | Var _ -> raise (EvalError "eval_s: unbound variable")
  | Fun _ | Raise | Unassigned | Num _ | Bool _ | String _ | Conditional _ | Binop _
  | Unop _ -> evaluate eval_s _exp _env
  | Let (id, def, body) -> reval (subst id def body)
  | App (e1, e2) ->
    (match extract_s e1 with
     | Fun (id, def) -> reval (subst id (extract_s e2) def)
     | _ -> raise (EvalError "app in eval_s: non-function application"))
  | Letrec (id, def, body) ->
    match reval (subst id (Letrec (id, def, Var id)) def) with
    | Val x -> reval (subst id x body)
    | _ -> raise (EvalError "closures not included")
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   
let rec eval_d (_exp : expr) (_env : Env.env) : Env.value =
  match _exp with
  | Var _ |Fun _ | Num _ | Bool _ | String _ | Raise | Unassigned | Conditional _
  | Binop _ | Unop _ | Let _ -> evaluate eval_d _exp _env
  | App (e1, e2) ->
    (match eval_d e1 _env with
     | Val (Fun (id, e)) -> eval_d e (extend _env id (ref (eval_d e2 _env)))
     | _ -> raise (EvalError "app in eval_d: non-function application"))
  | Letrec (id, def, body) ->
    (match eval_d def (extend _env id (ref (Val Unassigned))) with
     | (Val (Var output)) as v ->
       if output = id then raise (EvalError "definition is varid")
       else eval_d body (extend _env id (ref v))
     | _ as v -> eval_d body (extend _env id (ref v))) ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let rec eval_l (_exp : expr) (_env : Env.env) : Env.value =
 match _exp with
  | Var _ | Num _ | Bool _ | Raise | String _ | Unassigned | Binop _ | Unop _
  | Conditional _ | Let _ -> evaluate eval_l _exp _env
  | Fun _ -> close _exp _env
  | Letrec (id, def, body) ->
    let id_val = ref (Val Unassigned) in
    let env = extend _env id id_val in
    id_val := eval_l def env;
    eval_l body env
  | App (e1, e2) ->
    (match eval_l e1 _env with
     | Closure (Fun (id, e1), envc) ->
       eval_l e1 (extend envc id (ref (eval_l e2 _env)))
     | _ -> raise (EvalError "app in eval_l: non-function application"))

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate = eval_d ;;