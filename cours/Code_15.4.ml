let breath_first_search bin_tree =
  let rec aux queue l =
    match queue with
    | [] -> l
    | EmptyBT :: tail -> aux tail l
    | Node (v,left,right)::tail -> aux (tail@[left;right]) (l@[v])
  in
  aux [bin_tree] []
