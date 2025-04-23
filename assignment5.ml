(*

PoPL Assignment 5 part I
Your Name : Hanliang Xu
List of Collaborators :

   For this part, you will write some programs in Fb.  Your answers for
   this section must be in the form of OCaml strings which parse
   to Fb ASTs.  If you want a macro for repeated code, you are welcome to use OCaml
   to put strings together as we did in the `fb_examples.ml` file.
   You are also welcome to copy-paste any code from that file into here.

   Remember to test your Fb code below against the reference Fb binaries (not just
   your own implementation of Fb which could in theory be buggy) to ensure that your
   functions work correctly.

 *)

(*
   Do realize this is a VERY primitive macro system, you will want to put () around
   any definition you make or when appended the parse order could change.

   For questions in this section you are not allowed to use the Let Rec syntax even
   if you have implemented it in your interpreter. Any recursion that you use must
   entirely be in terms of Functions. Feel free to implement an Fb Y-combinator
   here.  For examples and hints, see the file "fbdk/debugscript/fb_examples.ml".

   Remember to test your code against the standard Fb binaries (and not just your own
   implementation of Fb) to ensure that your functions work correctly.

*)

(* Part 2 question 1.

   Fb fails to provide any operations over integers more complex than addition 
   and subtraction.  Below, define the following 2-argument Fb functions: 
   less-than and multiplication.  
   
   (Hint: if you get stuck, try getting them working for positive numbers first 
    and then dealing with negatives.)  *)

let ycomb =
  "(Fun code -> 
      Let repl = Fun self -> Fun lft -> Fun rgt -> code (self self) lft rgt
      In repl repl)";;

let apply_y code =
   "( " ^ ycomb ^ " ) " ^ code;;

let both_pos_smaller_than =
   "(Fun rec -> Fun lft -> Fun rgt ->
      If (lft = 0) Then True Else (If (rgt = 0) Then False Else rec (lft - 1) (rgt - 1)))";;

let both_neg_smaller_than =
   "(Fun rec -> Fun lft -> Fun rgt ->
      If (lft = 0) Then False Else (If (rgt = 0) Then True Else rec (lft + 1) (rgt + 1)))";;

let same_sign_smaller_than =
   "(Fun lft -> Fun rgt -> Fun is_pos ->
      If (is_pos) Then " ^ apply_y both_pos_smaller_than ^ " lft rgt
      Else " ^ apply_y both_neg_smaller_than ^ " lft rgt)";;

let is_pos =
   "(Fun rec -> Fun pos -> Fun neg ->
      If (pos = 0) Then True Else
      If (neg = 0) Then False Else
      rec (pos - 1) (neg + 1))";;

let is_pos_wrapper =
   "(Fun param ->
      (( " ^ apply_y is_pos ^ " param) param))";;

let pos_neg_judge =
  "(Fun lft -> Fun rgt ->
      Let is_pos = " ^ is_pos_wrapper ^ " In
      Let same_sign_smaller_than = " ^ same_sign_smaller_than ^ " In
      Let lft_is_pos = is_pos lft In
      Let rgt_is_pos = is_pos rgt In
      If (lft_is_pos And rgt_is_pos) Then (same_sign_smaller_than lft rgt True)
      Else If (lft_is_pos And (Not rgt_is_pos)) Then False
      Else If ((Not lft_is_pos) And rgt_is_pos) Then True
      Else (same_sign_smaller_than lft rgt False))";;

let fb_lt =
   "Fun lft -> Fun rgt ->
      (If ( lft = rgt ) Then False
      Else ( " ^ pos_neg_judge ^ " lft ) rgt)";;

let mul_by_add =
   "(Fun rec -> Fun lft -> Fun rgt ->
      If (rgt = 0) Then 0
      Else If ( " ^ is_pos_wrapper ^ " rgt) Then (lft + (rec lft (rgt - 1)))
      Else ((rec lft (rgt + 1)) - lft))";;

let fb_mult =
   "Fun lft -> Fun rgt -> (( " ^ apply_y mul_by_add ^ " ) lft ) rgt";;
   
(*    
   assert(peu ("("^fb_lt^") 33 3") = "False");;
      --> 33 < 3 => False
   assert(peu ("("^fb_lt^") (-1) 3") = "True");;
      --> -1 < 3 => True
   assert(peu ("("^fb_mult^") 5 3") = "15");;
      --> 5 * 3 => 15
   assert(peu ("("^fb_mult^") (-3) 5") = "-15");;
      --> (-3) * 5 => (-15)
*)
      
(* Part 2 question 2.

   Fb is a simple language. But even it contains more constructs than strictly 
   necessary.
   For example, you don't even need integers! They can be encoded 
   by functions using what is called Church's encoding.
   (For more information, see http://en.wikipedia.org/wiki/Church_encoding)

   Essentially this encoding allows us to represent integers as 
   functions. 
   
      Here's some examples of the integer representation:

         0 --> Function f -> Function x -> x
         1 --> Function f -> Function x -> f x
         2 --> Function f -> Function x -> f (f x)

   Remember that all your answers should generate Fb programs as 
   strings.     

*)

(* In this first part, you can assume that we are dealing with only 
*non-negative* integers. *)

(* Write a Fb function to convert a church encoded int to an Fb native int.*)
let fb_unchurch = "Fun church_int -> (church_int (Fun x -> x + 1) 0)";;

let church_num_constructor =
   "(Fun rec -> Fun count -> Fun f -> Fun x ->
      If (count = 0) Then x
      Else (f (rec (count - 1) f x)))";;

(* Write a Fb function to convert an Fb native int to a church encoded int *)
let fb_church = "(Fun nat_int ->
   (Fun f -> (Fun x -> ( ( ( " ^ apply_y church_num_constructor ^ " ) nat_int ) f ) x) ) )";;

(*
let church2 = "Function f -> Function x -> f (f x)";;
assert ( peu ("("^fb_unchurch^")("^church2^")") = "2" );;
assert ( peu ("("^fb_church^" 4) (Function n -> n + n) 3") = "48" );;
*)

(* NOTE: It's important to keep in mind that for the following functions, you
   must implement these functions using CHURCH NUMERALS only--that is, if you 
   simply unchurch the numerals, perform operations on normal integers and 
   convert them back to their church forms, you will NOT receive full points!!
*)

let fb_church_is_zero = "Fun church_num -> church_num (Fun _ -> False) True";;

   
(* Write a function to add two church encoded values *)
let fb_church_add = "Fun lft -> Fun rgt -> Fun f -> Fun x -> (lft f (rgt f x))";;

(* Write a function to multiply two church encoded values *)
let fb_church_mult = "Fun lft -> Fun rgt -> Fun f -> Fun x -> (lft (rgt f) x)";;

(*
let church2 = "(Function f -> Function x -> f (f x))";;
let church3 =  "(Function f -> Function x -> f (f (f x)))" ;;
assert ( peu ("("^fb_unchurch^") (("^fb_church_add^")"^church3^church2^")") = "5" );;
assert ( peu ("("^fb_unchurch^") (("^fb_church_mult^")"^church3^church2^")") = "6" );;
*)