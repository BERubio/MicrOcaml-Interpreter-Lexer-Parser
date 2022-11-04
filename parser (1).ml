open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  match lookahead(toks) with
  | Some Tok_Let -> parse_let(toks)
  | Some Tok_If -> parse_if(toks)
  | Some Tok_Fun -> parse_function(toks)
  | _ -> parse_or(toks)
and parse_let toks =
  let l1 = match_token toks Tok_Let in
  let var = match lookahead (l1) with | Some Tok_Rec -> true | _ ->  false in
  let l2 = if var then match_token l1 Tok_Rec else l1 in
  let a1 = match lookahead (l2) with | Some Tok_ID a -> a | _ -> raise (InvalidInputException "wrong bruh") in
  let l3 = match_token l2 (Tok_ID a1) in
  let l4 = match_token l3 Tok_Equal in
  let (x1,a2) = parse_expr (l4) in
  let l5 = match_token x1 Tok_In in
  let (x2,a3) = parse_expr (l5) in
  (x2, Let (a1, var, a2, a3))
and parse_if toks =
  let l1 = match_token toks Tok_If in
  let (x1,a1) = parse_expr (l1) in
  let l2 = match_token x1 Tok_Then in
  let (x2,a2) = parse_expr (l2) in
  let l3 = match_token x2 Tok_Else in
  let (x3,a3) = parse_expr (l3) in
  (x3, If (a1, a2, a3))
and parse_function toks =
  let l1 = match_token toks Tok_Fun in
  let a1 = match lookahead (l1) with | Some Tok_ID a -> a | _ -> raise (InvalidInputException "wrong bruh") in
  let l2 = match_token l1 (Tok_ID a1) in
  let l3 = match_token l2 Tok_Arrow in
  let (x1,a2) = parse_expr (l3) in
  (x1, Fun (a1,a2))
and parse_or toks =
 let (x1,a1) = parse_and(toks) in
 match lookahead (x1) with
 | Some Tok_Or -> let l1 = match_token x1 Tok_Or in 
   let (x2, a2) = parse_or l1 in 
   (x2, Binop(Or, a1, a2))
 | _ -> (x1, a1)
and parse_and toks =
  let (x1,a1) = parse_equality(toks) in
  match lookahead (x1) with
  | Some Tok_And -> let l1 = match_token x1 Tok_And in 
    let (x2, a2) = parse_and l1 in 
    (x2, Binop (And, a1, a2))
  | _ -> (x1, a1)
and parse_equality toks =
  let (x1,a1) = parse_relational(toks) in
  match lookahead (x1) with
  | Some Tok_Equal -> let l1 = match_token x1 Tok_Equal in 
    let (x2, a2) = parse_equality l1 in 
    (x2, Binop (Equal, a1, a2))
  | Some Tok_NotEqual -> let l1 = match_token x1 Tok_NotEqual in
    let (x2, a2) = parse_equality l1 in
    (x2, Binop (NotEqual, a1, a2))
  | _ -> (x1, a1)
and parse_relational toks =
  let (x1,a1) = parse_additive(toks) in
  match lookahead (x1) with 
  | Some Tok_Less -> let l1 = match_token x1 Tok_Less in
    let (x2, a2) = parse_relational l1 in
    (x2, Binop (Less, a1, a2))
  | Some Tok_LessEqual-> let l1 = match_token x1 Tok_LessEqual in 
    let (x2, a2) = parse_relational l1 in 
    (x2, Binop (LessEqual, a1, a2))
  | Some Tok_Greater -> let l1 = match_token x1 Tok_Greater in
    let (x2, a2) = parse_relational l1 in
    (x2, Binop (Greater, a1, a2))
  | Some Tok_GreaterEqual-> let l1 = match_token x1 Tok_GreaterEqual in 
    let (x2, a2) = parse_relational l1 in 
    (x2, Binop (GreaterEqual, a1, a2))
  | _ -> (x1, a1)
and parse_additive toks =
  let (x1,a1) = parse_multiplicative(toks) in 
  match lookahead (x1) with
  | Some Tok_Add -> let l1 = match_token x1 Tok_Add in 
    let (x2, a2) = parse_additive l1 in 
    (x2, Binop (Add, a1, a2))
  | Some Tok_Sub -> let l1 = match_token x1 Tok_Sub in
    let (x2, a2) = parse_additive l1 in
    (x2, Binop (Sub, a1, a2))
  | _ -> (x1, a1)
and parse_multiplicative toks = 
  let (x1,a1) = parse_concat(toks) in
  match lookahead (x1) with
  | Some Tok_Mult -> let l1 = match_token x1 Tok_Mult in
    let (x2, a2) = parse_multiplicative l1 in
    (x2, Binop (Mult, a1, a2))
  | Some Tok_Div -> let l1 = match_token x1 Tok_Div in 
    let (x2, a2) = parse_multiplicative l1 in 
    (x2, Binop (Div, a1, a2))
  | _ -> (x1, a1)
and parse_concat toks =
  let (x1,a1) = parse_unary(toks) in
  match lookahead (x1) with
  | Some Tok_Concat -> let l1 = match_token x1 Tok_Concat in 
    let (x2, a2) = parse_concat l1 in 
    (x2, Binop (Concat, a1, a2))
  | _ -> (x1, a1)
and parse_unary toks =
  match lookahead (toks) with
  | Some Tok_Not -> let l1 = match_token toks Tok_Not in
    let (x1, a1) = parse_unary l1 in 
    (x1, Not (a1))
  | _ -> parse_functioncall toks
and parse_functioncall toks =
  let (x1,a1) = parse_primaryexpr toks in
  match lookahead (x1) with
  | Some Tok_Int a -> let (x2,a2) = parse_primaryexpr x1 in
    (x2, FunctionCall (a1, a2))
  | Some Tok_Bool a -> let (x2,a2) = parse_primaryexpr x1 in
    (x2, FunctionCall (a1, a2))
  | Some Tok_String a -> let (x2,a2) = parse_primaryexpr x1 in
    (x2, FunctionCall (a1, a2))
  | Some Tok_ID a -> let (x2,a2) = parse_primaryexpr x1 in
    (x2, FunctionCall (a1, a2))
  | Some Tok_LParen -> let (x2,a2) = parse_primaryexpr x1 in
    (x2, FunctionCall (a1, a2))
  | _ -> (x1,a1)
and parse_primaryexpr toks =
  match lookahead (toks) with
  | Some Tok_Int a -> let l1 = match_token toks (Tok_Int a) in
    (l1, Value (Int a))
  | Some Tok_Bool a -> let l1 = match_token toks (Tok_Bool a) in
    (l1, Value (Bool a))
  | Some Tok_String a -> let l1 = match_token toks (Tok_String a) in 
    (l1, Value (String a))
  | Some Tok_ID a -> let l1 = match_token toks (Tok_ID a) in
    (l1, ID (a))
  | Some Tok_LParen -> let l1 = match_token toks (Tok_LParen) in
    let (x1,a1) = parse_expr l1 in
    let l2 = match_token x1 (Tok_RParen) in 
    (l2, a1)
  | _ -> raise (InvalidInputException "wrong")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
  match lookahead (toks) with
  | Some Tok_Def -> let l1 = match_token toks Tok_Def in
    let a1 = match lookahead (l1) with | Some Tok_ID a -> a | _ -> raise (InvalidInputException "wrong bruh") in
    let l2 = match_token l1 (Tok_ID a1) in
    let l3 = match_token l2 (Tok_Equal) in 
    let (x1,a2) = parse_expr l3 in
    let l4 = match_token x1 Tok_DoubleSemi in
    (l4, Def (a1, a2))
  | Some Tok_DoubleSemi ->  ([], NoOp)
  | _ -> let (x1, a1) = parse_expr toks in
    let l1 = match_token x1 Tok_DoubleSemi in
    (l1, Expr (a1))
