(************************************************************************
 Name   : hw1.ml
 Author : Omar Abdelmotaleb
 Date   : February 21, 2021
 Pledge : I pledge my honor that I have abided by the Stevens Honor System.
*************************************************************************)

(*****************************
 Encoding          Instruction
 -----------------------------
    0              Pen Down
    1              Pen Up
    2              Move North
    3              Move East
    4              Move South
    5              Move West
******************************)


type program = int list;;

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1];;
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1];;

(****************** 
  Helper Functions 
 ******************)

(*0=0, 1=1, 2=4, 3=5, 4=2, 5=3*)
let mirror n = 
  if n < 2 then n
  else if n < 4 then n + 2
  else n - 2

(*0=0, 1=1, 2=3, 3=4, 4=5, 5=2*)
let letter_90 n = 
  if n < 2 then n
  else if n > 4 then 2
  else n + 1

(*Takes a list of lists and concatenates all the elements together to make one big list*)
let rec cleanup l = 
  match l with
  | [] -> []
  | h::t -> h @ cleanup t

(*Increments or decrements the first or second value of a tuple*)
let inc_x (a : int * int) = ((fst a)+1,snd a);;
let dec_x (a : int * int) = ((fst a)-1,snd a);;
let inc_y (a : int * int) = (fst a,(snd a)+1);;
let dec_y (a : int * int) = (fst a,(snd a)-1);;

(*Moves a coordinate point by 1 in the direction of instruction n*)
let move n (a : int * int) =
  if n = 2 then (inc_y a)
  else if n = 3 then (inc_x a)
  else if n = 4 then (dec_y a)
  else if n = 5 then (dec_x a)
  else a

(*Turns an int list into a list of its values but as a tuple with a second value of 1*)
let list_tuples l = List.map (fun a -> (a,1)) l;;
(*Increments the second value of the tuple*)
let inc_tuple (a : int * int) (b : int * int) = (fst a, (snd a) + (snd b));;
(*Function for folding*)
let compress_helper a b = 
  if (fst (List.hd (List.rev a))) = fst b then (List.rev (List.tl (List.rev a))) @ [(inc_tuple (List.hd (List.rev a)) b)]
  else a @ [b]

(*Creates a list of elements for a '(x,y)' expanding out x, y times*)
let rec expand a =
  if snd a = 0 then []
  else [fst a] @ expand (fst a, (snd a)-1)

(* Main functions *)

let mirror_image l = List.map mirror l;;

let rotate_90_letter l = List.map letter_90 l;;

let rotate_90_word l = List.map rotate_90_letter l;;

let rec repeat x s = 
  if x = 0 then []
  else [s] @ (repeat (x-1) s)

(*Helper repeat for pantograph. Same as repeat but checks for 0s or 1s.*)
let repeat_but_check x n = 
  if x < 2 || n < 2 then [n]
  else repeat x n

let pantograph x l = cleanup (List.map (repeat_but_check x) l);;
let rec pantograph_nm x l =
  if x < 2 then l
  else match l with 
  | [] -> []
  | h::t -> (repeat_but_check x h) @ (pantograph_nm x t)
let pantograph_f x l = List.fold_left (fun a b -> a @ (repeat_but_check x b)) [] l;;

let rec coverage (a : int * int) l = 
  match l with 
  | [] -> [a]
  | h::t -> [a] @ (coverage (move h a) t)

let compress l = List.fold_left compress_helper [(0,0)] (list_tuples l);;

let rec uncompress l = 
  match l with
  | [] -> []
  | h::t -> (expand h) @ (uncompress t)

let uncompress_m l = cleanup (List.map expand l);;
let uncompress_f l = List.fold_left (fun a b -> a @ (expand b)) [] l;;

let rec optimize_helper n l =
  match l with
  | [] -> []
  | h::t -> 
    if h > 1 then [h] @ (optimize_helper n t)
    else if h = n then optimize_helper n t
    else [h] @ (optimize_helper (abs (n-1)) t)
let optimize (l : program) : program = optimize_helper 1 l;;