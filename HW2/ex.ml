(* 

8 Feb 2021 - Third class on OCaml

*)

(* Examples of recursion over natural numbers *)

(** [fact n] returns the factorial of n 
   [n] must be positive *)
let rec fact n =
  match n with
  | 0 -> 1
  | m when m>0 -> m * fact (m-1)
  | _ -> failwith "fact: negative argument"

(** [f e n] returns a list with [n] copies of [e] *) 
let rec f (e:'a) (n:int) : 'a list =
  match n with
  | 0 -> []
  | m -> e :: f e (m-1)


(* Examples of recursion on lists *)

let rec length l =
  match l with
  | [] -> 0
  | h::t -> 1 + length t

let rec sum (l:int list) : int =
  match l with
  | [] -> 0
  | h::t -> h + sum t

let rec sum' : int list -> int =
  function
  | [] -> 0
  | h::t -> h + sum' t

let rec sum : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + sum t


let rec stutter (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> h::h::stutter t
    
(* 
Examples:

stutter [1;2;3] ==> [1;1;2;2;3;3]
stutter [] ==> []

*)

let at_least_two_elements l =
  match l with
  | e1::e2::t -> true
  | _ -> false
    
let rec rev l =
  match l with
  | [] -> []
  | h::t -> rev t @ [h]

let rev' l = 
  let rec rev_helper l a =
    match l with
    | [] -> a
    | h::t -> rev_helper t (h::a)
  in rev_helper l []

let rec rem_adj_duplicates l =
  failwith "implement me"

(* 
rem_adj_duplicates [] ==> []

rem_adj_duplicates [1] ==> [1]

rem_adj_duplicates [1;7;7;7;3;3;4] ==> [1;7;3;4]

*)


(* Well-known higher-order function schemes *)

let inc i = i+1
let upper i = Char.uppercase_ascii i
let is_zero i = i=0
                
(** [succl l] adds one to each number in [l] *)
let rec succl (l:int list) : int list =
  match l with
  | [] -> []
  | h::t -> inc h :: succl t

let rec upperl (l:char list) : char list =
  match l with
  | [] -> []
  | h::t -> upper h :: upperl t

let rec chk_zero (l:int list) : bool list =
  match l with
  | [] -> []
  | h::t -> is_zero h :: chk_zero t

let rec map (f:'a->'b) (l:'a list) : 'b list =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let succl' = map inc
let upperl' = map upper 
let chk_zero' = map is_zero 


let is_positive i = i>0
let is_upper i = i=Char.uppercase_ascii i
let is_ne i = i<>[]
                 
(** [greater_than_zero l] filters (retains) those numbers in [l]
    that are >0 *)
let rec greater_than_zero (l:int list) : int list =
  match l with
  | [] -> []
  | h::t ->
    if is_positive h
    then h:: greater_than_zero t
    else greater_than_zero t

let rec get_upper (l:char list) : char list =
  match l with
  | [] -> []
  | h::t ->
    if is_upper h
    then h :: get_upper t
    else get_upper t

let rec get_ne (l:'a list list) : 'a list list =
  match l with
  | [] -> []
  | h::t ->
    if is_ne h
    then h:: get_ne t
    else get_ne t 

let rec filter (p:'a -> bool) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h :: filter p t
    else filter p t
        
let greater_than_zero' = filter is_positive
let get_upper' = filter is_upper
let get_ne' = filter is_ne
    
let rec andl l =
  match l with
  | [] -> true
  | h::t -> h && andl t

let rec appendl l =
  match l with
  | [] -> []
  | h::t -> h @ appendl t
  
let rec suml l =
  match l with
  | [] -> 0
  | h::t -> h + suml t

(* Exercise 

Represent sets as functions.
Implement set operations using that representation

*)


let seta : int -> bool = fun i-> i=1 || i=3 || i=7
let setb : int -> bool = fun i -> i=12 || i=9 || i=5 || i=7

let setc : char -> bool = fun i -> i='a' || i='c'
let setd : char -> bool = fun i -> i='a' || i='e' || i='r'

let union : ('a -> bool) -> ('a -> bool) -> ('a -> bool) =
  fun sa sb ->
  fun i -> sa i || sb i

(* 
union seta setb 14 ==> false
union seta setb 7 ==> true
*)

let intersection : ('a -> bool) -> ('a -> bool) -> ('a -> bool) =
  fun sa sb ->
  fun i -> sa i && sb i

let complement : ('a -> bool) -> ('a -> bool) =
  fun sa ->
  fun i -> not (sa i)




let rec andl l =
  match l with
  | [] -> true
  | h::t -> h && andl t

let rec appendl l =
  match l with
  | [] -> []
  | h::t -> h @ appendl t
  
let rec suml l =
  match l with
  | [] -> 0
  | h::t -> h + suml t

let rec foldr (f:'a->'b->'b) (a:'b) (l:'a list) : 'b =
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let rec foldl (f:'a->'b->'b) (a:'b) (l:'a list) : 'b =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t

let andl' = foldl (fun h r -> h && r) true 
let appendl' = foldl (fun h r -> h @ r) [] 
let suml' = foldl (fun h r -> h+r) 0


(*
Summary three well-known higher-order function schemes we have
 *presented:

1. Map
2. Filter
3. Fold

Last intro topic on OCaml

Algebraic Data Types


*)


type dot = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let is_weekend d =
  match d with
  | Sat | Sun  -> true
  | _ -> false

let next_day d =
  match d with
  | Mon -> Tue
  | Tue -> Wed
  | Wed -> Thu
  | Thu -> Fri
  | Fri -> Sat
  | Sat -> Sun
  | Sun  -> Mon


type flavor = Choc | Straw | Vanilla
type icecream = Cup of flavor | Cone of flavor*flavor

let ic1 = Cup Choc
let ic2 = Cone(Choc,Vanilla)

let boring_cone f = Cone(f,f)

type ('a,'b) either = Left of 'a | Right of 'b

type 'a option = None | Some of 'a 
  
let rec lookup l n =
  match l with
  | [] -> None
  | (key,value)::t ->
    if key = n
    then Some value
    else lookup t n

type 'a btree = Empty | Node of 'a*'a btree*'a btree

let t : int btree =
  Node(24,
       Node(7,Empty,Empty),
       Node(33,
            Node(28,Empty,Empty),
            Empty))

let rec sizet t =
  match t with
  | Empty -> 0
  | Node(d,lt,rt) -> 1 + sizet lt + sizet rt

let rec m t =
  match t with
  | Empty -> Empty
  | Node(d,lt,rt) -> Node(d, m rt, m lt)

let rec f t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> d :: (f lt @ f rt)

let rec f' t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> f' lt @ [d] @ f' rt

let rec f'' t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> f'' lt @ f'' rt @ [d]


let rec mapt f t =
  failwith "implement"

let rec foldt f a t =
  failwith "implement"
  
(* Quiz 2 *)
(* 
   One submission per group
   Name of both partners in source code
   Name of other member (the one not submitting) as a canvas comment
*)

(* [prune n t] prunes the tree [t] at level [n].
   If [n] is larger than the height of [t], then it returns [t].
   Also, [prune 0 t] ==> Empty 
*)

let rec prune n t =
  match (n,t) with
  | (0,_) -> Empty
  | (_,Empty) -> Empty
  | (m,Node(d,lt,rt)) ->
       Node(d,prune (m-1) lt, prune (m-1) rt)

let rec prune' n t =
  match n with
  | 0 -> Empty
  | m ->
    (match t with
     | Empty -> Empty
     | Node(d,lt,rt) ->
       Node(d,prune' (m-1) lt, prune' (m-1) rt)
    )

(* [perfect t] determines whether [t] is a perfect binary tree
   [t] is perfect if the length of all the
 * paths to all the leaves is the same *)


let rec perfect_helper l =
  match l with
  | [ ] | [_] -> true
  | n::m::t -> n=m && perfect_helper (m::t)
       
let perfect t =
    perfect_helper (List.map List.length (paths_to_leaves t))