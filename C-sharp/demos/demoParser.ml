open CSharp.Lib.Ast
open CSharp.Parser

(* TODO: Redo this *)

let parse = apply parse_class
  ({|
     public class Program {

            static void Main()
        {
            
        }
                }

  |})


let print_list = (* ??? *)
  Format.pp_print_list Format.pp_print_string Format.std_formatter

(* let run_test = print_list (List.map show_cs_class parse) *)