open Lambda_lib
open Ast
open Lambda

type 'a status = Done of 'a | WIP of 'a

let fin x = Done x
let wip x = WIP x

let nor_small_step_strat =
  let rec helper = function
    | (Var _ as l) | (Abs (_, _) as l) -> fin l
    | App (f, arg) -> (
      match helper f with
      | WIP f2 -> fin (app f2 arg)
      | Done (Abs (x, e)) -> wip (subst x ~by:arg e)
      | Done f2 -> fin (App (f2, arg)) ) in
  let rec loop t =
    match helper t with
    | Done x -> x
    | WIP x ->
        Format.printf " -- %a\n%!" Pprintast.pp x;
        loop x in
  let on_app _ f arg = loop (app f arg) in
  let on_abs _ f x = loop (abs f x) in
  let on_var _ x = loop (var x) in
  {on_var; on_abs; on_app}

let test strat term =
  Format.printf "Evaluating: %a\n%!" Pprintast.pp term;
  let rez = apply_strat strat term in
  Format.printf "Result:     %a\n%!" Pprintast.pp rez;
  rez

module Std = struct
  let zero = abs "g" @@ abs "y" @@ Var "y"
  let one = abs "f" @@ abs "x" @@ app f (Var "x")
  let two = abs "f" @@ abs "x" @@ app f (app f x)
  let three = abs "f" @@ abs "x" @@ app f (app f (app f x))

  let plus =
    abs "m" @@ abs "n" @@ abs "f" @@ abs "x" @@ app (app m f) (app (app n f) x)

  let mul = abs "x" @@ abs "y" @@ abs "z" @@ app x (app y z)
  let true_ = abs "x" @@ abs "y" @@ Var "x"
  let false_ = abs "x" @@ abs "y" @@ Var "y"
  let isZero = abs "n" @@ app (app n (abs "x" false_)) true_

  (* if-then-else for lazy strategy *)
  let ite cond th el = app (app (app isZero cond) th) el

  let pred =
    let xxx = abs "g" @@ abs "h" @@ app h (app g f) in
    abs "n" @@ abs "f" @@ abs "x"
    @@ app (app (app n xxx) (abs "u" x)) (abs "u" (Var "u"))

  let fact =
    abs "self" @@ abs "N"
    @@ ite (Var "N") one
         (app (app mul (app (var "self") (app pred (var "N")))) (var "N"))

  let ygrek =
    let hack = abs "x" (app f (app x x)) in
    abs "f" (app hack hack)

  (* 5! = 120 *)
  let () =
    test nor_strat @@ app (app ygrek fact) (app (app plus two) three)
    |> Format.printf "%a\n%!" Pprintast.pp
end
