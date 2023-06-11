(* Testing for evaluation.ml *)

open Expr ;;
open Evaluation ;;
open Evaluation.Env ;;

let test_env =
  let env1 = empty () in
  assert (close (Num 2) env1 = Closure (Num 2, env1));
  print_string "Passed tests: 1, ";
  let env1 = extend env1 "x" (ref (Val (Num 2))) in
  assert (env_to_string env1 = "(x, Val (Num 2)); ");
  print_string "2, ";
  let env1 = extend env1 "x" (ref (Val (Num 3))) in
  assert (env_to_string env1 = "(x, Val (Num 3)); (x, Val (Num 2)); ");
  print_string "3, ";
  let env1 = extend env1 "y" (ref (Val (Fun ("x", Num 2)))) in
  assert (env_to_string env1
          = "(y, Val (Fun(x, Num 2))); (x, Val (Num 3)); (x, Val (Num 2)); ");
  print_string "4, ";
  assert (value_to_string (lookup env1 "y") = "Val (Fun(x, Num 2))");
  print_string "5, ";
  let env2 = empty () in
    assert (try
              value_to_string (lookup env2 "y") = ""
            with
            | EvalError _ -> true
            | _ -> false);
  print_string "6. ";
  print_endline "module Env passed tests" ;;

let test_eval_s =
  let eval exp = let env = Env.empty () in eval_s exp env in
  assert (eval Raise = Val Raise);
  print_string "Passed tests: 1, ";
  assert (eval Unassigned = Val Unassigned);
  print_string "2, ";
  assert (eval (Num 3) = Val (Num 3));
  print_string "3, ";
  assert (eval (Bool true) = Val (Bool true));
  print_string "4, ";
  assert (try
            eval (Var "x") = Val (Var "x")
          with
          | EvalError "eval_s: unbound variable" -> true
          | _ -> false);
  print_string "5, ";
  assert (eval (Fun ("x", Var "x")) = Val (Fun ("x", Var "x")));
  print_string "6, ";
  assert (eval (Unop (Negate, Num 3)) = Val (Num ~-3));
  print_string "7, ";
  assert (eval (Binop (Plus, Num 3, Num 4)) = Val (Num 7));
  print_string "8, ";
  assert (eval (Binop (Minus, Num 3, Num 4)) = Val (Num ~-1));
  print_string "9, ";
  assert (eval (Binop (Times, Num 0, Num ~-3)) = Val (Num 0));
  print_string "10, ";
  assert (eval (Binop (LessThan, Num 3, Num 4)) = Val (Bool true));
  print_string "11, ";
  assert (eval (Binop (LessThan, Bool true, Bool false)) = Val (Bool false));
  print_string "12, ";
  assert (eval (Binop (Equals, Num 3, Num 4)) = Val (Bool false));
  print_string "13, ";
  assert (eval (Binop (LessThan, Bool false, Bool false)) = Val (Bool false));
  print_string "14, ";
  assert (try
            eval (Let ("x", Num 1, Var "x")) = Val (Num 1)
          with
          | EvalError "Unbound variable." -> true
          | _ -> false);
  print_string "15, ";
  assert (eval (Let ("x", Num 1, Binop (Plus, Var "x", Num 1))) = Val (Num 2));
  print_string "16, ";
  assert (try
            eval (Let ("f", Fun("x", Binop(Plus, Var "x", Num 2)), App(Var "f", Num 2))) = Val (Num 4)
          with
          | EvalError "Unbound variable." -> true
          | _ -> false);
  print_string "17, ";
  assert (eval (Let("x", Num 2, Let("f", Fun("y", Binop(Plus, Var "x", Var "y")), App(Var "f", Num 3)))) = Val (Num 5));
  print_string "18, ";
  assert (eval (Letrec("f", Fun("n", Conditional(Binop(Equals, Var "n", Num 0),
                                               Num 1,
                                               Binop(Times, Var "n", App (Var "f", Binop(Minus, Var "n", Num 1))))),
                       App (Var "f", Num 3))) = Val (Num 6));
  print_string "19, ";
  assert (eval (Letrec ("f",
                        Fun ("n", Conditional (Binop (Equals, Var "n", Num 0),
                                               Num 1,
                                               Binop(Plus, Var "n",
                                                     App (Var "f",
                                                          Binop (Minus,
                                                                 Var "n",
                                                                 Num 1))))),
                       App (Var "f", Num 6))) = Val (Num 22));
  print_string "20, ";
  assert (eval (Letrec ("f",
                        Fun ("n", Conditional (Binop (Equals, Var "n", Num 0),
                                               Num 1,
                                               Binop(Plus, Var "n",
                                                     App (Var "f",
                                                          Binop (Minus,
                                                                 Var "n",
                                                                 Num 1))))),
                        App (Var "f", Num 6)))
          = Val (Num 22));
  print_string "21. ";
  print_endline "eval_s passed tests" ;;

let test_eval_d =
  let eval exp = let env = Env.empty () in eval_d exp env in
  assert (eval Raise = Val Raise);
  print_string "Passed tests: 1, ";
  assert (eval Unassigned = Val Unassigned);
  print_string "2, ";
  assert (eval (Num 3) = Val (Num 3));
  print_string "3, ";
  assert (eval (Bool true) = Val (Bool true));
  print_string "4, ";
  assert (try
            eval (Var "x") = Val (Var "x")
          with
          | EvalError "lookup: unbound variable" -> true
          | _ -> false);
  print_string "5, ";
  assert (eval (Fun ("x", Var "x")) = Val (Fun ("x", Var "x")));
  print_string "6, ";
  assert (eval (Unop (Negate, Num 3)) = Val (Num ~-3));
  print_string "7, ";
  assert (eval (Binop (Plus, Num 3, Num 4)) = Val (Num 7));
  print_string "8, ";
  assert (eval (Binop (Minus, Num 3, Num 4)) = Val (Num ~-1));
  print_string "9, ";
  assert (eval (Binop (Times, Num 0, Num ~-3)) = Val (Num 0));
  print_string "10, ";
  assert (eval (Binop (LessThan, Num 3, Num 4)) = Val (Bool true));
  print_string "11, ";
  assert (eval (Binop (LessThan, Bool true, Bool false)) = Val (Bool false));
  print_string "12, ";
  assert (eval (Binop (Equals, Num 3, Num 4)) = Val (Bool false));
  print_string "13, ";
  assert (eval (Binop (LessThan, Bool false, Bool false)) = Val (Bool false));
  print_string "14, ";
  assert (try
            eval (Let ("x", Num 1, Var "x")) = Val (Num 1)
          with
          | EvalError "lookup: unbound variable" -> true
          | _ -> false);
  print_string "15, ";
  assert (try
            eval (Let ("x", Num 1, Binop (Plus, Var "x", Num 1))) = Val (Num 2)
          with
          | EvalError "lookup: unbound variable" -> true
          | _ -> false);
  print_string "16, ";
  assert (eval (Let ("f",
                     Fun("x", Binop (Plus, Var "x", Num 2)),
                     App (Var "f", Num 2)))
          = Val (Num 4));
  print_string "17, ";
  assert (eval (Let("x", Num 2, Let("f", Fun("y", Binop(Plus, Var "x", Var "y")), App(Var "f", Num 3)))) = Val (Num 5));
  print_string "18, ";
  assert (eval (Letrec("f", Fun("n", Conditional (Binop(Equals, Var "n", Num 0),
                                                  Num 1,
                                                  Binop (Times, Var "n", App (Var "f", Binop (Minus, Var "n", Num 1))))),
                       App (Var "f", Num 3))) = Val (Num 6));
  print_string "19, ";
  assert (eval (Letrec ("f",
                        Fun ("n", Conditional (Binop (Equals, Var "n", Num 0),
                                               Num 1,
                                               Binop(Plus, Var "n",
                                                     App (Var "f",
                                                          Binop (Minus,
                                                                 Var "n",
                                                                 Num 1))))),
                       App (Var "f", Num 6))) = Val (Num 22));
  print_string "20, ";
  assert (eval (Let ("x",
                     Num 1,
                     Let("f",
                         Fun ("y",
                              Binop (Plus,
                                     Var "x",
                                     Var "y")),
                         Let ("x", Num 2, App (Var "f", Num 3)))))
          = Val (Num 5));
  print_string "21. ";
  print_endline "eval_d passed tests" ;;

let test_eval_l =
  let eval exp = let env = Env.empty () in eval_l exp env in
  assert (eval Raise = Val Raise);
  print_string "Passed tests: 1, ";
  assert (eval Unassigned = Val Unassigned);
  print_string "2, ";
  assert (eval (Num 3) = Val (Num 3));
  print_string "3, ";
  assert (eval (Bool true) = Val (Bool true));
  print_string "4, ";
  assert (try
            eval (Var "x") = Val (Var "x")
          with
          | EvalError "lookup: unbound variable" -> true
          | _ -> false);
  print_string "5, ";
  assert (eval (Fun ("x", Var "x")) = Closure (Fun ("x", Var "x"), Env.empty ()));
  print_string "6, ";
  assert (eval (Unop (Negate, Num 3)) = Val (Num ~-3));
  print_string "7, ";
  assert (eval (Binop (Plus, Num 3, Num 4)) = Val (Num 7));
  print_string "8, ";
  assert (eval (Binop (Minus, Num 3, Num 4)) = Val (Num ~-1));
  print_string "9, ";
  assert (eval (Binop (Times, Num 0, Num ~-3)) = Val (Num 0));
  print_string "10, ";
  assert (eval (Binop (LessThan, Num 3, Num 4)) = Val (Bool true));
  print_string "11, ";
  assert (eval (Binop (LessThan, Bool true, Bool false)) = Val (Bool false));
  print_string "12, ";
  assert (eval (Binop (Equals, Num 3, Num 4)) = Val (Bool false));
  print_string "13, ";
  assert (eval (Binop (LessThan, Bool false, Bool false)) = Val (Bool false));
  print_string "14, ";
  assert (eval (Let ("x", Num 1, Var "x")) = Val (Num 1));
  print_string "15, ";
  assert (try
            eval (Let ("x", Num 1, Binop (Plus, Var "x", Num 1))) = Val (Num 2)
          with
          | EvalError "lookup: unbound variable" -> true
          | _ -> false);
  print_string "16, ";
  assert (eval (Let ("f",
                     Fun("x", Binop (Plus, Var "x", Num 2)),
                     App (Var "f", Num 2)))
          = Val (Num 4));
  print_string "17, ";
  assert (eval (Let("x", Num 2, Let("f",
                                    Fun ("y",
                                              Binop (Plus,
                                                     Var "x",
                                                     Var "y")),
                                    App(Var "f", Num 3))))
          = Val (Num 5));
  print_string "18, ";
  assert (eval (Let ("x",
                     Num 1,
                     Let("f",
                         Fun ("y",
                              Binop (Plus,
                                     Var "x",
                                     Var "y")),
                         Let ("x", Num 2, App (Var "f", Num 3)))))
          = Val (Num 4));
  print_string "19, ";
  assert (eval (Letrec("f",
                       Fun("n", Conditional (Binop(Equals, Var "n", Num 0),
                                             Num 1,
                                             Binop (Times,
                                                    Var "n",
                                                    App (Var "f",
                                                         Binop (Minus,
                                                                Var "n",
                                                                Num 1))))),
                       App (Var "f", Num 3)))
          = Val (Num 6));
  print_string "20, ";
  assert (eval (Letrec ("f",
                        Fun ("n", Conditional (Binop (Equals, Var "n", Num 0),
                                               Num 1,
                                               Binop(Plus, Var "n",
                                                     App (Var "f",
                                                          Binop (Minus,
                                                                 Var "n",
                                                                 Num 1))))),
                        App (Var "f", Num 6)))
          = Val (Num 22));
  print_string "21. ";
  print_endline "eval_l passed tests" ;;