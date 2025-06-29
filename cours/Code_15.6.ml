let inorder bin_tree =
  let rec aux bin_tree acc = 
    match bin_tree with
    | EmptyBT -> acc
    | Node (v, left, right) -> aux left (v :: (aux right acc)) in
  aux bin_tree []
