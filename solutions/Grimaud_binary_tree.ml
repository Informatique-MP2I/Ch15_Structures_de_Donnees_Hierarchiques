type 'a binary_tree =
  | EmptyBT
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* Number of nodes of a binary tree *)
(*let rec nodes = function
  | EmptyBT -> 0
  | Node (_, left, right) -> 1 + nodes left + nodes right*)
    
let nodes tree =
  let rec aux tree acc =
    match tree with
    | EmptyBT -> acc
    | Node (_, t_g, t_d) -> let acc_left = aux t_g (acc+1) in
      aux t_d acc_left in
  aux tree 0

(* Height of a binary tree *)     
let rec height = function
  | EmptyBT -> -1
  | Node (_, left, right) -> 1 + max (height left) (height right)

(* Verification of a perfect binary tree *)
let rec is_perfect = function
  | EmptyBT -> true
  | Node (_, left, right) -> is_perfect left && is_perfect right && (height left) = (height right)
                   
   
(* Verification of a complet binary tree - quadratic complexity *)
let rec is_complete = function
  | EmptyBT -> true
  | Node (_, left, right) ->
    (height left = height right && is_perfect(left) && is_complete(right)) ||
    (height left = (height right) + 1 && is_complete(left) && is_perfect(right))

let is_complete2 bin_tree =
  let rec aux = function
  | EmptyBT -> Some (-1)
  | Node (_, left, right) -> let lh = aux left and rh = aux right in
    match (lh, rh) with
    | (None, _) | (_, None) -> None
    | (Some x, Some y) -> if (x <> y) && (x <> y+1) then None else Some (x+1) in
  match aux bin_tree with
  | None -> false
  | Some _ -> true

(* Breath-First Search *)
let breath_first_search bin_tree =
  let rec aux queue l =
    match queue with
    | [] -> l
    | EmptyBT :: tail -> aux tail l
    | Node (v, left, right) :: tail -> aux (tail @ [left; right]) (l @ [v])in
  aux [bin_tree] []
                       
(* Array of a complet tree *)
let bin_tree_to_array bin_tree =
  assert (is_complete bin_tree);
  let n = nodes bin_tree in
  match bin_tree with
  | EmptyBT -> [||]
  | Node (v, _, _) ->
    let t = Array.make n v in
    let rec aux queue i =
      match queue with
      | [] -> ()
      | EmptyBT :: tail -> aux tail i 
      | Node (v, left, right) :: tail ->
        t.(i) <- v;
        aux (tail @ [left; right]) (i+1) in
    aux [bin_tree] 0;
    t
    
(* Inorder tree search *)  
let inorder bin_tree =
  let rec aux bin_tree acc = 
    match bin_tree with
    | EmptyBT -> acc
    | Node (v, left, right) -> aux left (v :: (aux right acc)) in
  aux bin_tree []

(* Preorder tree serach *) 
let preorder bin_tree =
  let rec aux bin_tree acc = 
    match bin_tree with
    | EmptyBT -> acc
    | Node (v, left, right) -> v :: aux left (aux right acc) in
  aux bin_tree []

(* Postorder tree search *) 
let postorder bin_tree =
  let rec aux bin_tree acc = 
    match bin_tree with
    | EmptyBT -> acc
    | Node (v, left, right) -> (aux right (aux left acc)) @ [v] in
  aux bin_tree []

(* Main expression *)
let () =
  let tree1 = EmptyBT in
  let tree2 =
  Node('0',
       Node('1',
            Node('3', EmptyBT, EmptyBT),
            Node('4', Node('6', EmptyBT, EmptyBT), Node('7', EmptyBT, EmptyBT))),
       Node('2',
            EmptyBT,
            Node('5', Node('8', EmptyBT, EmptyBT), EmptyBT))) in
  let tree3 =
    Node('0', Node('1', Node ('3', EmptyBT, EmptyBT), EmptyBT), Node('2', EmptyBT, EmptyBT)) in
  let tree4 =
    Node('0', Node('1', EmptyBT, EmptyBT), Node('2', Node ('3', EmptyBT, EmptyBT), EmptyBT)) in
  let tree5 =
    Node('+', Node('*',Node('5', EmptyBT,EmptyBT),Node('*',Node('x',EmptyBT,EmptyBT),Node('x',EmptyBT,EmptyBT))), Node('-',Node('*',Node('2',EmptyBT,EmptyBT),Node('x',EmptyBT,EmptyBT)),Node('1',EmptyBT,EmptyBT))) in
  let tree6 = Node('1',Node('2',Node('4',EmptyBT,EmptyBT),EmptyBT),Node('3',Node('5',EmptyBT,EmptyBT),EmptyBT)) in
  Printf.printf "Height : %d - Nodes : %d - Complete : %B\n" (height tree1) (nodes tree1) (is_complete tree1);
  Printf.printf "Height : %d - Nodes : %d - Complete : %B\n" (height tree2) (nodes tree2) (is_complete tree2);
  Printf.printf "Height : %d - Nodes : %d - Complete : %B\n" (height tree3) (nodes tree3) (is_complete tree3);
  Printf.printf "Height : %d - Nodes : %d - Complete : %B\n" (height tree4) (nodes tree4) (is_complete tree4);
  Printf.printf "Height : %d - Nodes : %d - Complete : %B\n" (height tree5) (nodes tree5) (is_complete tree5);
  Printf.printf "Height : %d - Nodes : %d - Complete : %B\n" (height tree6) (nodes tree6) (is_complete tree6);
  List.iter (fun x-> Printf.printf "%c " x) (breath_first_search tree2); Printf.printf "\n";
  Array.iter (fun x-> Printf.printf "%c " x) (bin_tree_to_array tree3); Printf.printf "\n";
  List.iter (fun x-> Printf.printf "%c " x) (inorder tree2); Printf.printf "\n";
  List.iter (fun x-> Printf.printf "%c " x) (inorder tree5); Printf.printf "\n";
  List.iter (fun x-> Printf.printf "%c " x) (preorder tree5); Printf.printf "\n";
  List.iter (fun x-> Printf.printf "%c " x) (postorder tree5); Printf.printf "\n"

