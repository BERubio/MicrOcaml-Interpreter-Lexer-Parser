open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let rParen = Str.regexp ")"  
let lParen = Str.regexp "(" 
let equal = Str.regexp "=" 
let notEqual = Str.regexp "<>"
let greater = Str.regexp ">" 
let less = Str.regexp "<" 
let greater_equal = Str.regexp ">=" 
let less_equal = Str.regexp "<=" 
let re_or = Str.regexp "||" 
let re_and = Str.regexp "&&"
let not = Str.regexp "not[ \t\n\r(]" 
let if_tok = Str.regexp "if[ \t\n\r(]"
let then_tok = Str.regexp "then[ \t\n\r(]" 
let else_tok = Str.regexp "else[ \t\n\r(]" 
let add = Str.regexp "+"  
let sub = Str.regexp "-" 
let mult = Str.regexp "*" 
let div = Str.regexp "/"
let concat = Str.regexp "^" 
let let_ = Str.regexp "let "  
let def_ = Str.regexp "def " 
let in_ = Str.regexp "in " 
let recur = Str.regexp "rec " 
let func = Str.regexp "fun " 
let arrow = Str.regexp "->" 
let semis = Str.regexp ";;" 

(*end of Single Form tokens*)

let number = Str.regexp "([-]?[0-9]+)"
let pos_number = Str.regexp "([0-9]+)"
let neg_number = Str.regexp "(-[0-9]+)"
let boolean = Str.regexp "\\(\\(true\\)\\|\\(false\\)\\)" 
let strings = Str.regexp "\"[^\"]*\"" 
let id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"  

(*Personal regexs for processing*)
let check = Str.regexp "[a-zA-z0-9]" 
let newline = Str.regexp "[ \t\n]" 

let tokenize input =
  let rec tok pos s =  
   if pos >= (String.length s) then
     []
   else if (Str.string_match (Str.regexp "let ") s pos) then
      let token = Str.matched_string s in
      (Tok_Let)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "def ") s pos) then
      let token = Str.matched_string s in
      (Tok_Def)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "in ") s pos) then
      let token = Str.matched_string s in
      (Tok_In)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "rec ") s pos) then
      let token = Str.matched_string s in
      (Tok_Rec)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "fun ") s pos) then
      let token = Str.matched_string s in
      (Tok_Fun)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "not[ \t\n\r(]") s pos) then
      let token = Str.matched_string s in
      (Tok_Not)::(tok (pos + String.length token - 1) s)
   else if (Str.string_match (Str.regexp "if[ \t\n\r(]") s pos) then
      let token = Str.matched_string s in
      (Tok_If)::(tok (pos + String.length token - 1) s)
   else if (Str.string_match (Str.regexp "then[ \t\n\r(]") s pos) then
      let token = Str.matched_string s in
      (Tok_Then)::(tok (pos + String.length token - 1) s)
   else if (Str.string_match (Str.regexp "else[ \t\n\r(]") s pos) then
      let token = Str.matched_string s in
      (Tok_Else)::(tok (pos + String.length token - 1) s)
   else if (Str.string_match (Str.regexp "\\(\\(true\\)\\|\\(false\\)\\)") s pos) then
      let token = Str.matched_string s in
      (Tok_Bool (bool_of_string token))::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "\"[^\"]*\"") s pos) then
     let token = Str.matched_string s in
     (Tok_String (String.sub token 1 ((String.length token) - 2)))::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") s pos) then
     let token = Str.matched_string s in
     (Tok_ID token)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "[0-9]+") s pos) then
     let token = Str.matched_string s in
     (Tok_Int (int_of_string token))::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "(-[0-9]+)") s pos) then
     let token = Str.matched_string s in
     (Tok_Int (int_of_string (String.sub token 1 ((String.length token) - 2))))::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "(") s pos) then
     let token = Str.matched_string s in
     (Tok_LParen)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp ")") s pos) then
     let token = Str.matched_string s in
     (Tok_RParen)::(tok (pos + String.length token) s) 
   else if (Str.string_match (Str.regexp "+") s pos) then
    let token = Str.matched_string s in
    (Tok_Add)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "->") s pos) then
     let token = Str.matched_string s in
     (Tok_Arrow)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "-") s pos) then
     let token = Str.matched_string s in
    (Tok_Sub)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "*") s pos) then
     let token = Str.matched_string s in
     (Tok_Mult)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "\\^") s pos) then
     let token = Str.matched_string s in
    (Tok_Concat)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "/") s pos) then
     let token = Str.matched_string s in
     (Tok_Div)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "=") s pos) then
     let token = Str.matched_string s in
     (Tok_Equal)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "<>") s pos) then 
    let token = Str.matched_string s in
    (Tok_NotEqual)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp ">") s pos) then
     let token = Str.matched_string s in
     (Tok_Greater)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "<") s pos) then
    let token = Str.matched_string s in
    (Tok_Less)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp ">=") s pos) then
    let token = Str.matched_string s in
    (Tok_GreaterEqual)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "<=") s pos) then
     let token = Str.matched_string s in
     (Tok_LessEqual)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "||") s pos) then
     let token = Str.matched_string s in
     (Tok_Or)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "&&") s pos) then
    let token = Str.matched_string s in
    (Tok_And)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp ";;") s pos) then
    let token = Str.matched_string s in
    (Tok_DoubleSemi)::(tok (pos + String.length token) s)
   else if (Str.string_match (Str.regexp "[ \t\n\r]") s pos) then
    let token = Str.matched_string s in
    (tok (pos + String.length token) s)
   else
    raise (InvalidInputException "tokenize")
in
  tok 0 input ;;
    

    
    
    

    
    
    

    
