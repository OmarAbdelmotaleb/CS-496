(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:

    Name1: Omar Abdelmotaleb
    Name2:

  I pledge my honor that I have abided by the Stevens Honor System.

*)



type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

let rec height t =
  match t with
  | Node(n,[]) -> 1
  | Node(n,h::t) -> 1 + List.fold_left (fun a b -> max a b) (height h) (List.map height t)
    
let rec size t =
  match t with
  | Node(n, []) -> 1
  | Node(n,h::t) -> 1 + List.fold_left (fun a b -> a + b) (size h) (List.map size t)


let rec elem_helper n e l =
  match l with
  | [] -> n
  | h::t -> 
    if e = h then n
    else elem_helper (n+1) e t

let elem e l = elem_helper 0 e l;;

let rec paths_to_leaves t =
  match t with
  | Node(n, []) -> [ [] ]
  | Node(n, l) -> List.fold_left (fun a b -> a @ b) [[n]] (List.map paths_to_leaves l)

let rec is_perfect t =
  failwith "implement"


let rec preorder (Node(d,ch)) =
  match (Node(d,ch)) with
  | Node(n, []) -> [n]
  | Node(n, l) -> List.fold_left (fun a b -> a @ b) [n] (List.map preorder l)
           
let rec mirror (Node(d,ch)) =
  match (Node(d,ch)) with
  | Node(n, []) -> Node(n, [])
  | Node(n, l) -> Node(n, List.map mirror (List.rev l))

  
let rec mapt f (Node(d,ch)) =
  match (Node(d,ch)) with 
  | Node(n, []) -> Node(f n, [])
  | Node(n, l) -> Node(f n, List.map (mapt f) l)
  
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
    f d (List.map (foldt f) ch)

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
  foldt (fun a b -> Node(a, List.rev b)) t
