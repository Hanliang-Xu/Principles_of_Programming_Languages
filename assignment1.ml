(*

PL Assignment 1
 
Name                  : 
List of Collaborators :

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you will find it helpful to define
auxillary functions, feel free to.

The question may involve implementation of well-known search algorithms;
if you are not familiar with search algorithms in general, your trusty search 
engine should be able to give you an answer, and feel free to ask on Courselore.

You must not use any mutation operations of OCaml for any of these
questions (which we have not taught yet in any case): no arrays,
for- or while-loops, references, etc.

Note for this first assignment you can only use things in standard library Stdlib; 
you **cannot** use list library functions such as `List.hd` or `List.nth` 
(you can code your own versions of them, of course).

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27-39"]

(* Here is a simple function which gets passed unit, (), as argument
   and raises an exception.  It is the initial implementation below. *)

let unimplemented () = failwith "unimplemented"

(* ************** Section One: List operations ************** *)

(*
  1a. Write a function that will return the value at a given index (location) 
      in a list. Raise an exception if the provided index is negative or out of 
      bounds.  The first element is indexed 0.
*)

let rec nth_exn (n : int) (lst : 'a list) : 'a =
  if n < 0 then raise (Invalid_argument "Negative index")
  else
    let rec iter (i : int) (lst : 'a list) : 'a =
      match lst with
      |  [] -> raise (Invalid_argument "Index out of bounds")
      |  hd :: tl -> if (n = i) then hd  else iter (i + 1) tl
    in
    iter 0 lst

(*
# nth_exn 1 [1;2;3;4] ;;
- : int = 2
# nth_exn 2 ["TOS";"TNG";"DS9"] ;;
- : string = "DS9"
*)

(*
  1b. Write a function that, when given a predicate function and a list, will 
  return true if all elements in the list satisfy this predicate function, and
  false otherwise. If the list is empty, then the function returns true.
*)

let rec for_all (predicate : 'a -> bool) (lst : 'a list) : bool =
  match lst with
  |  [] -> true
  |  hd :: tl -> (predicate hd) && for_all predicate tl

(*
# for_all (fun n -> n > 0) [0; 1; 2; 3] ;;
- : bool = false
# for_all (fun b -> not b) [false; false] ;;
- : bool = true
*)

(* 1c. List map is one of the most commonly used list combinators in functional
       programming. Essentially, it takes in a function and a list, and returns
       the result of applying this function to every element in the list.
*)

let rec list_map (f : 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  |  [] -> []
  |  hd :: tl -> f hd :: list_map f tl

(*
# list_map (fun n -> n * 2 - 1) [2; 4; 6; 8] ;;
- : int list = [3; 7; 11; 15]
# list_map (fun l -> for_all (fun n -> n > 0) l) [[0;1;2]; [3;4;5]; [6;7;8]] ;;
- : bool list = [false; true; true]
*)

(*
   1d. A transpose function takes in an n x m matrix and returns an m x n result,
       where the rows become columns and vice versa. For example, transposing
       the 2 x 4 matrix 

       [[1; 2; 3; 4]; 
        [4; 3; 2; 1]] 

       gives us the resulting 4 x 2 matrix

       [[1; 4]; 
        [2; 3]; 
        [3; 2]; 
        [4; 1]].

       Implement this transpose function. Note that matrix is represented by a 
       list of list (i.e: 'a list list).
*)

let rec transpose (og_vector : 'a list list) : 'a list list =
  match og_vector with
  |  [] -> []
  |  [] :: _ -> []
  |  _ ->
    let rec get_first_orig_col m =
      match m with
      |  [] -> []
      |  first_row :: rest ->
        match first_row with
        |  hd :: tl -> hd :: get_first_orig_col rest
        |  [] -> failwith "Encountered an empty row"
    in

    let rec get_rest_cols m =
      match m with
      |  [] -> []
      |  first_row :: rest ->
        match first_row with
        |  hd :: tl -> tl :: get_rest_cols rest
        |  [] -> failwith "Encountered an empty row"
    in

    (get_first_orig_col og_vector) :: transpose (get_rest_cols og_vector)

(*
# transpose [[1;2;3;4]; [4;3;2;1]];;
- : int list list = [[1; 4]; [2; 3]; [3; 2]; [4; 1]]
*)

(* ************** Section Two: Futoshiki verifier ************** *)

(* Futoshiki is a board-based puzzle game, also known under the name Unequal.
   It is playable on a square board having a given fixed size (4x4 for example).
   The rules are similar to sudoku, where each cell contains a number between 1
   and the board's size, and on each row and column, each number appears exactly
   once. The twist to Futoshiki is that apart from this sudoku conditoin, there
   are certain inequalities between neighboring cells that the solution must
   satisfy.
   In case you're not familiar with the game, the rules and some examples can
   be found here:
   https://www.futoshiki.com/

   An example of a correctly solved Futoshiki puzzle is shown below.

    1<2 4 3

    4 3 2 1
          ^
    3<4 1 2
    v
    2 1 3<4

   As you can see, for this particular puzzle, each row and column contain numbers
   ranging from 1 to 4, with each number only appearing once. Besides, the
   inequalities are also respected. Therefore, this is a valid solution.

   A simple representation of an n x n Futoshiki grid is as a list of length n,
   where each of the n elements is itself a length-n list, and the inequalities
   are represented by a list of pairs of coordinates (which is in turn represented
   by an int tuple)

   e.g.: The above example grid is represented as the OCaml list

   [[1; 2; 4; 3];
    [4; 3; 2; 1];
    [3; 4; 1; 2];
    [2; 1; 3; 4]]

   And the inequalities are encoded by the following list:

   [((0,0), (0, 1)); ((1, 3), (2, 3)); ((2, 0), (2, 1));
    ((3, 0), (2, 0)); ((3, 2), (3, 3))]

   For simplicity's sake, we will implement a naive checking algorithm, which can
   largely be divided into two steps.

   1. Checking for inequalities correctness. This can be achieved by simply
      querying the coordinates provided in the inequality list, and check whether
      the less-than relation holds for all pairs.

   2. Checking for row/column correctness, i.e. the sudoku property. This can be
      achieved by sorting each row and make sure that it contains all the right
      numbers, and then do the same for each column.

   Note that many of the functions you implemented in Part 1 will come in handy
   here, so don't hesitate to use them as necessary!

   Overall, the verification function itself will take in three arguments:
   an integer representing the dimension of the game, a list that represents the
   inequalities, and the grid in the above representation. You can assume that
   the grid provided will be of the correct dimension.
*)

(* 2a. To perform the verification, we often need to query a value by its
       coordinate. Write a function that will return a cell's value given
       its coordinate. Use `failwith` to throw a runtime error if the coordinate
       provided is out of bound.
*)

let fetch_val (grid : int list list) (coord : int * int) : int =
  
  match coord with
  | (row, col) -> nth_exn col (nth_exn row grid)

(*
# fetch_val [[1;2;3];[4;5;6];[7;8;9]] (0,0) ;;
- : int = 1
# fetch_val [[1;2;3];[4;5;6];[7;8;9]] (2,1) ;;
- : int = 8
*)

(* 2b. Since the verification involves sorting the list, you can implement your
   favorite sorting algorithm for an integer list. It doesn't have to be super
   efficient; anything with at least an O(n^2) efficiency is acceptable.
*)

let rec sort_int_list (lst : int list) : int list =
  let rec insert x l =
    match l with
    |  [] -> [x]
    |  hd :: tl -> if x <= hd then (x :: l) else (hd :: (insert x tl))
  in
  match lst with
  |  [] -> []
  |  hd :: tl -> insert hd (sort_int_list tl)

(*
# sort_int_list [1;3;5;4;2;6] ;;
- : int list = [1; 2; 3; 4; 5; 6]
*)

(*
  2c. We'll also need to check if a list of coordinate pairs satisfy the less-than 
      relation, which is the second step of verification.
*)

let rec verify_coord_pairs (coord_list : ((int * int) * (int * int)) list)
    (grid : int list list) : bool =
  match coord_list with
  |  [] -> true
  |  (smaller, larger) :: tl ->
    (fetch_val grid smaller) < (fetch_val grid larger) && verify_coord_pairs tl grid

(*
# let grid = [[1; 2; 4; 3];[4; 3; 2; 1];[3; 4; 1; 2];[2; 1; 3; 4]];;
# let neq_pairs = [((0,0), (0, 1)); ((1, 3), (2, 3)); ((2, 0), (2, 1));
   ((3, 0), (2, 0)); ((3, 2), (3, 3))];;
# verify_coord_pairs neq_pairs grid;;
- : bool = true 
*)

(* 2d. Now we can put it all together and verify the entire grid! *)

let verify_solution n neq_pairs grid =
  let rec aux expected lst =
    match lst with
    | [] -> (expected = n + 1)
    | hd :: tl -> if hd <> expected then false else aux (expected + 1) tl
  in
  for_all (aux 1) (list_map sort_int_list grid) &&
  for_all (aux 1) (list_map sort_int_list (transpose grid)) &&
  verify_coord_pairs neq_pairs grid


(*
# verify_solution 4 neq_pairs grid ;;
- : bool = true

# let neq_pairs_bad = [((0,0), (0, 1)); ((1, 3), (2, 3)); ((2, 0), (2, 1));
  ((2, 0), (3, 0)); ((3, 2), (3, 3))];;
# verify_solution 4 neq_pairs_bad grid ;;
- : bool = false
*)