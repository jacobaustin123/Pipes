open Ast

(* sast.ml: contains the definitions of the algebraic types which make up the semantically checked abstract 
syntax tree. 

The overall semantically checked program is an sprogram, with a list of semantically checked statements
(sstmt list) and a list of global variables and their inferred types (bind list). 

Semantically checked functions are stored as sfunc_decl records, containing their return type (styp), their
name (sfname), a list of formal variables (sformals), a list of local variables (slocals), and the sbody, an
SBlock sstmt. 

sexp is the type that stores semantically checked expressions. SListSlice, SMethod, and SField have not been
implemented. sexpr is merely an sexp with an associated type. In general, that type is the type inferred 
by semant, while any binds found in sexprs or sstmts contain types that need to be checked at runtime. 

sstmt is the type that stores semantically checked statements. SClass has not been implemented. STransform
is an internal type used to handle some of the boxing and unboxing required by the gradual type system. This
is usually inserted when conditional branches are merged or when generic (unknown) functions are called.

lvalues are the types that can occur on the left-hand side of an assignment. SLListSlice has not been
implemented. These can be expanded as more kinds of features are added, including classes.

The various print functions are pretty-printing functions for debugging and viewing the generated SAST.
*)

type sprogram = sstmt list * bind list

and sfunc_decl = {
  styp : typ;
  sfname : string option;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt
}

and sexp =
  | SBinop of sexpr * operator * sexpr (* (left sexpr, op, right sexpr) *)
  | SLit of literal (* literal *)
  | SVar of string (* see above *)
  | SUnop of uop * sexpr (* (uop, sexpr ) *)
  | SCall of sexpr * sexpr list * sstmt (* SVar or SCall, list of args, SFunc) *) (* sstmt is SNop if recursive call within function or weak function *)
  | SMethod of sexpr * string * sexpr list (* not implemented *)
  | SField of sexpr * string (* not implemented *)
  | SList of sexpr list * typ (* (list of expressions, inferred type) *)
  | SNoexpr (* no expression *)
  | SListAccess of sexpr * sexpr (* not implemented *)
  | SListSlice of sexpr * sexpr * sexpr (* not implemented *)
  | SCast of typ * typ * sexpr (* from type, to type, expr *)
  | SPartial of sexpr * sexpr list 
  | SLambda of bind list * sexpr
  (* | SPipe of sexpr * sexpr  *)

and sexpr = sexp * typ

and sstmt = (* this can be refactored using Blocks, but I haven't quite figured it out yet *)
  | SFunc of sfunc_decl (* (name, return type), list of formals, list of locals, body) *)
  | SBlock of sstmt list (* block found in function body or for/else/if/while loop *)
  | SExpr of sexpr (* see above *)
  | SIf of sexpr * sstmt * sstmt (* condition, if, else *)
  | SFor of bind * sexpr * sstmt (* (variable, list, body (block)) *)
  | SWhile of sexpr * sstmt (* (condition, body (block)) *)
  | SRange of bind * sexpr * sstmt
  | SReturn of sexpr (* return statement *)
  | SClass of string * sstmt (* not implemented *)
  | SAsn of lvalue list * sexpr (* x : int = sexpr, (Bind(x, int), sexpr) *)
  | STransform of string * typ * typ 
  | SStage of sstmt * sstmt * sstmt (* entry, body, exit *)
  | SPrint of sexpr
  | SType of sexpr
  | SContinue
  | SBreak
  | SNop
  | SPass

and lvalue = 
  | SLVar of bind
  | SLListAccess of sexpr * sexpr
  | SLListSlice of sexpr * sexpr * sexpr

let concat_end delim = List.fold_left (fun a c -> a ^ delim ^ c) ""
let append_list v = List.map (fun c -> c ^ v)

let rec string_of_sexpr p (e, t) = string_of_sexp p e

and string_of_sbind = function
  | Bind(s, t) -> s

and string_of_sexp p = function
  | SBinop(e1, o, e2) -> string_of_sexpr p e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr p e2
  | SLit(l) -> string_of_lit l
  | SVar(str) -> str
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr p e
  | SCall(e, el, s) -> 
      (match e with 
        | (SPartial(_, _), _) -> string_of_sexpr false e ^ "(" ^ String.concat ", " (List.map (string_of_sexpr p) el) ^ ")"
        | _ -> string_of_sexpr p e ^ "(" ^ String.concat ", " (List.map (string_of_sexpr p) el) ^ ")")
  | SMethod(obj, m, el) -> string_of_sexpr p obj ^ "." ^ m ^ "(" ^ String.concat ", " (List.map (string_of_sexpr p) el) ^ ")"
  | SField(obj, s) -> string_of_sexpr p obj ^ "." ^ s
  | SList(el, t) -> "[" ^ String.concat ", " (List.map (string_of_sexpr p) el) ^ "]"
  | SListAccess(e1, e2) -> string_of_sexpr p e1 ^ "[" ^ string_of_sexpr p e2 ^ "]"
  | SListSlice(e1, e2, e3) -> string_of_sexpr p e1 ^ "[" ^ string_of_sexpr p e2 ^ ":" ^ string_of_sexpr p e3 ^ "]"
  | SCast(t1, t2, e) -> string_of_typ t2 ^ "(" ^ string_of_sexpr p e ^ ") -> " ^ string_of_typ t1
  | SNoexpr -> ""
  | SPartial(a, b) -> 
    if p then "functools.partial(" ^ string_of_sexpr p a ^ ", " ^ String.concat ", " (List.map (string_of_sexpr p) b) ^ ")"
    else string_of_sexpr false a
  | SLambda(a, b) -> "lambda " ^ String.concat " " (List.map (fun (Bind(x, _)) -> x) a) ^ ": " ^ string_of_sexpr p b

and string_of_sstmt depth = function
  | SFunc({ styp; sfname; sformals; slocals; sbody }) -> (String.make depth '\t') ^ "def " ^ (match sfname with | Some x -> x | None -> "") ^ "(" ^ String.concat ", " (List.map string_of_sbind sformals) ^ "):\n" ^ string_of_sstmt (depth + 1) sbody
  | SBlock(sl) -> String.concat "\n" (List.map (string_of_sstmt depth) sl) ^ "\n"
  | SExpr(e) -> (String.make depth '\t') ^ string_of_sexpr true e
  | SIf(e, s1, s2) ->  (String.make depth '\t') ^ "if " ^ string_of_sexpr true e ^ ":\n" ^ string_of_sstmt (depth + 1) s1 ^ "else:\n" ^ string_of_sstmt (depth + 1) s2
  | SFor(b, e, s) -> (String.make depth '\t') ^ "for " ^ string_of_sbind b ^ " in " ^ string_of_sexpr true e ^ ":\n" ^ string_of_sstmt (depth + 1) s
  | SRange(b, e, s) -> (String.make depth '\t') ^ "for " ^ string_of_sbind b ^ " in range (" ^ string_of_sexpr true e ^ ") :\n" ^ string_of_sstmt (depth + 1) s
  | SWhile(e, s) -> (String.make depth '\t') ^ "while " ^ string_of_sexpr true e ^ ":\n" ^ string_of_sstmt (depth + 1) s
  | SReturn(e) -> (String.make depth '\t') ^ "return " ^ string_of_sexpr true e
  | SClass(b, s) -> (String.make depth '\t') ^ "class " ^ b ^ ":\n" ^ string_of_sstmt (depth + 1) s
  | SAsn(lvalues, e) -> (String.make depth '\t') ^ String.concat ", " (List.map string_of_lvalue lvalues) ^ " = "  ^ string_of_sexpr true e
  | SStage(s1, s2, s3) -> (String.make depth '\t') ^ string_of_sstmt depth s2
  | SPrint(e) -> (String.make depth '\t') ^ "print(" ^ string_of_sexpr true e ^ ")"
  | SBreak -> (String.make depth '\t') ^ "break"
  | SContinue -> (String.make depth '\t') ^ "continue"
  | SNop -> ""
  | SPass -> (String.make depth '\t') ^ "pass"
  | _ -> ""

and string_of_lvalue = function
  | SLVar(sbind) -> string_of_sbind sbind
  | SLListAccess(e1, e2) ->  string_of_sexpr true e1 ^ "[" ^ string_of_sexpr true e2 ^ "]"
  | SLListSlice(e1, e2, e3) -> string_of_sexpr true e1 ^ "[" ^ string_of_sexpr true e2 ^ ":" ^ string_of_sexpr true e3 ^ "]"

and string_of_sprogram (sl, bl) = String.concat "\n" (List.map (string_of_sstmt 0) sl)