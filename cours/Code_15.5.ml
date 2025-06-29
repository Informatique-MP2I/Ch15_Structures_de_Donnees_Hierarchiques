let rec inorder bin_tree =
  match bin_tree with
  | EmptyBT -> []
  | Node (v, left, right) -> (inorder left)@(v::(inorder right))
