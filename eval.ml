open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
match e with
| Value x -> x
| ID x -> lookup env x

| Not x -> 
   let value =
     match (eval_expr env x) with 
     | Bool false -> Bool true
     | Bool true -> Bool false 
     | _ -> raise(TypeError "Expected type bool") in
    value

| Binop (Add, value1, value2) ->
 let num1 =
  match (eval_expr env value1) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
    let num2 =
        match (eval_expr env value2) with
        | Int x -> x
        | _ -> raise (TypeError "Expected type int") in
      let total = num1 + num2 in
      Int total     

| Binop (Sub, value1, value2) ->
  let num1 =
   match (eval_expr env value1) with
   | Int x -> x
   | _ -> raise (TypeError "Expected type int") in

  let num2 =
   match (eval_expr env value2) with
   | Int x -> x
   | _ -> raise (TypeError "Expected type int") in
  let diff = num1 - num2 in
  Int diff

| Binop (Mult, value1, value2) -> 
 let num1 =
  match (eval_expr env value1) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
 let num2 =
  match (eval_expr env value2) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
 let result = num1 * num2 in
 Int result
        
| Binop (Div, value1, value2) ->
  let num1 =
   match (eval_expr env value1) with
   | Int x -> x
   | _ -> raise (TypeError "Expected type int") in
  let num2 =
   match (eval_expr env value2) with
   | Int x -> x
   | _ -> failwith "Expected type int" in
  let result = if (num2 = 0) then
   raise (DivByZeroError)
  else
  num1/num2 in
  Int result

| Binop (Greater, value1, value2) -> 
 let num1 =
  match (eval_expr env value1) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
 let num2 =
  match (eval_expr env value2) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
 if num1 > num2 then 
    Bool true 
 else 
    Bool false
          
| Binop (GreaterEqual, value1, value2) ->
 let num1 =
  match (eval_expr env value1) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
 let num2 =
  match (eval_expr env value2) with
  | Int x -> x
  | _ -> raise (TypeError "Expected type int") in
 if num1 >= num2 then
  Bool true
 else
  Bool false

| Binop (Less, value1, value2) -> 
  let num1 =
   match (eval_expr env value1) with
   | Int x -> x
   | _ -> raise (TypeError "Expected type int") in
  let num2 =
   match (eval_expr env value2) with
   | Int x -> x
   | _ -> raise (TypeError "Expected type int") in
  if num1 < num2 then 
     Bool true 
  else 
     Bool false
  

| Binop (LessEqual, value1, value2) ->
let num1 =
 match (eval_expr env value1) with
 | Int x -> x
 | _ -> raise (TypeError "Expected type int") in
let num2 =
 match (eval_expr env value2) with
 | Int x -> x
 | _ -> raise (TypeError "Expected type int") in
if num1 >= num2 then
 Bool true
else
 Bool false

| Binop (Concat, value1, value2) -> 
 let word1 =
  match (eval_expr env value1) with
  | String x -> x
  | _ -> raise (TypeError "Expected type string") in
 let word2 =
  match (eval_expr env value2) with
  | String x -> x
  | _ -> raise (TypeError "Expected type string") in
 let result_string = word1 ^ word2 in
 String result_string
        
| Binop (Equal, value1, value2) ->
 let res =
  match (eval_expr env value1, eval_expr env value2) with
  | (Int int1, Int int2) -> if int1 = int2 then Bool true
                           else  Bool false
  | (String str1, String str2) -> if str1 = str2 then Bool true
                                 else Bool false
  | (Bool bool1, Bool bool2) -> if bool1 = bool2 then Bool true
                               else  Bool false
  | _ -> raise (TypeError "Cannot compare types") in

 res

| Binop (NotEqual, value1, value2) -> 
 let res = 
  match (eval_expr env value1, eval_expr env value2) with
  | (Int int1, Int int2) -> if int1 != int2 then Bool true
                           else Bool false
  | (String str1, String str2) -> if str1 != str2 then Bool true
                                 else Bool false
  | (Bool bool1, Bool bool2) -> if bool1 != bool2 then Bool true
                               else Bool false
  | _ -> raise (TypeError "Cannot compare types") in
 res
        
| Binop (Or, value1, value2) ->
   let res =
    match (eval_expr env value1, eval_expr env value2) with
    | (Bool true, Bool true) -> Bool true
    | (Bool false, Bool true) -> Bool true
    | (Bool true, Bool false) -> Bool true 
    | (Bool false, Bool false) -> Bool false
    | _ -> raise(TypeError "Expected type bool") in 
   res
        
| Binop (And, value1, value2) ->
  let res =
   match (eval_expr env value1, eval_expr env value2) with
   | (Bool true, Bool true) -> Bool true
   | (Bool false, Bool true) -> Bool false
   | (Bool true, Bool false) -> Bool false 
   | (Bool false, Bool false) -> Bool false
   | _ -> raise(TypeError "Expected type bool") in 
  res
  

| If (exp1, exp2, exp3) ->
 let res =
  match (eval_expr env exp1) with
  | Bool true -> (eval_expr env exp2)
  | Bool false -> (eval_expr env exp3)
  | _ -> raise(TypeError "Expected type bool") in
 res

| Let (name, true, init, body) ->
   let env2 = extend_tmp env name in
   let val1 = eval_expr env2 init in
   let _ = (update env2 name val1) in
   let val2 = eval_expr env2 body in
   val2

| Let (name, false, init, body) -> 
   let val1 = eval_expr env init in
   let env2 = extend env name val1 in
   let val2 = eval_expr env2 body in 
   val2
 
  
| Fun (parameter, name) ->
     Closure (env, parameter, name)


| FunctionCall (exp1, exp2) -> 
  let res =
   match (eval_expr env exp1) with
   | Closure (a, b, c) -> let env2 = extend a b (eval_expr env exp2) in
     eval_expr env2 c
   | _ -> raise(TypeError "Not a function") in 
  res

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
 match m with 
 | Def (x, exp) ->
   let env2 = extend_tmp env x in
   let value = eval_expr env2 exp in
   let _ = update env2 x value in
   (env2, Some value)
 | Expr exp -> 
  let value = eval_expr env exp in
  (env, Some value)

 | NoOp -> (env, None)
