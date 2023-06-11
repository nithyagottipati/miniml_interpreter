(* Testing for expr.ml *)

open Expr ;;

let _test_exp_to_abstract_string =
  let test1 = exp_to_abstract_string in
  assert (test1 (Num 3) = "Num 3");
  assert (test1 (Var "variable") = "Var variable");
  assert (test1 (Bool true) = "Bool true");
  assert (test1 (Bool false) = "Bool false");
  assert (test1 (Unop (Negate, Num 3)) = "Unop(Negate, Num 3)");
  assert (test1 (Binop (Times, Num 2, Num 3))
          = "Binop(Times, Num 2, Num 3)");
  assert (test1 (Binop (Plus, Num 2, Num 3))
          = "Binop(Plus, Num 2, Num 3)");
  assert (test1 (Binop (Minus, Num 2, Num 3))
          = "Binop(Minus, Num 2, Num 3)");
  assert (test1 (Binop (Equals, Num 2, Num 3))
          = "Binop(Equals, Num 2, Num 3)");
  assert (test1 (Binop (LessThan, Num 2, Num 3))
          = "Binop(LessThan, Num 2, Num 3)");
  assert (test1 (Conditional (Bool true, Num 2, Num 3))
          = "Conditional(Bool true, Num 2, Num 3)");
  assert (test1 (Fun("x", Binop (Times, Var "x", Num 3)))
          = "Fun(x, Binop(Times, Var x, Num 3))");
  assert (test1 (Let("x", Binop (Times, Num 2, Num 3), Binop (Plus,
                                                              Num 2,
                                                              Num 3)))
          = "Let(x, Binop(Times, Num 2, Num 3), Binop(Plus, Num 2, Num 3))");
  assert (test1 (Letrec("x", Binop (Times, Var "x", Num 3), Binop(Plus,
                                                                  Num 2,
                                                                  Num 3)))
          = "Letrec(x, Binop(Times, Var x, Num 3), Binop(Plus, Num 2, Num 3))");
  assert (test1 (App (Fun("x", Binop (Times, Var "x", Num 3)), Num 3))
          = "App(Fun(x, Binop(Times, Var x, Num 3)), Num 3)");
  assert (test1 Raise = "exception raised");
  assert (test1 Unassigned = "unassigned");
  print_endline "Exp_to_abstract_string passed the tests." ;;

let _test_exp_to_concrete_string =
  let test2 = exp_to_concrete_string in
  assert (test2 (Num 3) = "3");
  assert (test2 (Var "variable") = "variable");
  assert (test2 (Bool true) = "true");
  assert (test2 (Bool false) = "false");
  assert (test2 (Unop  (Negate, Num 3)) = "~-(3)");
  assert (test2 (Binop (Times, Num 2, Num 3)) = "(2 * 3)"); 
  assert (test2 (Binop (Plus, Num 2, Num 3)) = "(2 + 3)");
  assert (test2 (Binop (Minus, Num 2, Num 3)) = "(2 - 3)");
  assert (test2 (Binop (Equals, Num 2, Num 3)) = "(2 = 3)");
  assert (test2 (Binop (LessThan, Num 2, Num 3)) = "(2 < 3)"); 
  assert (test2 (Conditional (Bool true, Num 2, Num 3))
          = "if true then 2 else 3");
  assert (test2 (Fun ("x", Binop (Times, Var "x", Num 3)))
          = "(fun x -> (x * 3))"); 
  assert (test2 (Let("x", Binop (Times, Num 2, Num 3), Binop (Plus,
                                                              Num 2,
                                                              Num 3)))
          = "let x = (2 * 3) in (2 + 3)");
  assert (test2 (Letrec ("x", Binop (Times, Var "x", Num 3), Binop (Plus,
                                                                    Num 2,
                                                                    Num 3)))
          = "let rec x = (x * 3) in (2 + 3)"); 
  assert (test2 Raise = "exception raised");
  assert (test2 Unassigned = "unassigned");
  print_endline "Exp_to_concrete_string passed the tests." ;; 

let _test_free_vars =
  let test3 = free_vars in
  assert (same_vars (test3 (Let ("x", Num(3), Let("y",
                                                  Var "x",
                                                  App (Var "f",
                                                       App (Var "x",
                                                            Var "y"))))))
                    (vars_of_list ["f"]));
  assert (same_vars (test3 (Let ("x", Fun("y", Var "x"), Var "x")))
                    (vars_of_list ["x"]));
  assert (same_vars (test3 (Letrec ("x", Fun("y", Var "x"), Var "x")))
                    (vars_of_list []));
  assert (same_vars (test3 (Num 3)) (vars_of_list []));
  assert (same_vars (test3 (Bool true)) (vars_of_list []));
  assert (same_vars (test3 Raise) (vars_of_list []));
  assert (same_vars (test3 Unassigned) (vars_of_list []));
  assert (same_vars (test3 (Unop (Negate, Let ("x",
                                               Fun ("y", Var "x"),
                                               Var "x"))))
                    (vars_of_list ["x"]));
  assert (same_vars (test3 (Binop (Plus, Var "x", Var "y")))
                    (vars_of_list ["x"; "y"]));
  assert (same_vars (test3 (Conditional (Binop (LessThan, Num 2, Num 3),
                                         Num 2,
                                         Var "x" )))
                    (vars_of_list ["x"]));
  assert (same_vars (test3 (Letrec ("f",
                                    Fun ("n",
                                         Conditional (Binop (Equals,
                                                             Num 0,
                                                             Var "n"),
                                                      Num 1,
                                                      Binop (Times,
                                                             Var "n",
                                                             App (Var "f",
                                                                  Binop (Minus,
                                                                         Var "n",
                                                                         Num 1))))),
                                    App (Var "f", Num 3))))
                      (vars_of_list []));
  print_endline "Free_vars passed the tests." ;;

let _test_subst =
  assert (subst "x" (Num 3) (Let("x", Num 2, Var "x"))
          = (Let("x", Num 2, Var "x")));
  assert (subst "x" (Num 3) (Let("y", Num 2, Var "x"))
          = (Let("y", Num 2, Num 3)));
  assert (subst "x" (Num 3) (Letrec ("x", Fun("y", Var "x"), Var "x"))
          = Letrec ("x", Fun ("y", Var "x"), Var "x"));
  assert (subst "x" (Num 3) (Var "x") = Num 3);
  assert (subst "x" (Num 3) (Var "y") = Var "y");
  assert (subst "x" (Fun ("n", Var "n")) (Num 3) = Num 3);
  assert (subst "x" (Fun ("n", Var "n")) (Bool true) = Bool true);
  assert (subst "x" (Fun ("n", Var "n")) (Raise) = Raise);
  assert (subst "x" (Fun ("n", Var "n")) (Unassigned) = Unassigned);
  assert (subst "x" (Fun ("n", Var "n")) (Unop (Negate, Var "x"))
          = (Unop (Negate, Fun ("n", Var "n"))));
  assert (subst "x" (Num 2) (Binop (Plus, Var "x", Var "x"))
          = Binop (Plus, Num 2, Num 2));
  assert (subst "x" (Bool true) (Conditional (Var "x",
                                              Binop (LessThan,
                                                     Var "x",
                                                     Bool false), Var "x"))
          = Conditional (Bool true,
                         Binop (LessThan, Bool true, Bool false),
                         Bool true));
  assert (subst "x" (Num 2) (Fun ("x", Var "x")) = (Fun ("x", Var "x")));
  assert (subst "x" (Num 2) (Fun ("y", Var "x")) = (Fun ("y", Num 2)));
  assert (subst "x" (Unop (Negate, Var "x")) (Fun ("y", Var "x"))
          = (Fun ("y", Unop (Negate, Var "x"))));
  assert (subst "x" (Num 2) (App (Fun ("y", Var "x"), Num 3))
          = App (Fun ("y", Num 2), Num 3));
  assert (subst "n" (Num 2)
            (Letrec ("f",
                     Fun ("n",
                          Conditional (Binop (Equals,
                                              Num 0,
                                              Var "n"),
                                       Num 1,
                                       Binop (Times,
                                              Var "n",
                                              App (Var "f",
                                                   Binop (Minus,
                                                          Var "n",
                                                          Num 1))))),
                     App (Var "f", Num 3)))
          = (Letrec ("f",
                     Fun ("n",
                          Conditional (Binop (Equals,
                                              Num 0,
                                              Var "n"),
                                       Num 1,
                                       Binop (Times,
                                              Var "n",
                                              App (Var "f",
                                                   Binop (Minus,
                                                          Var "n",
                                                          Num 1))))),
                     App (Var "f", Num 3))));
  print_endline "Subst passed the tests." ;;