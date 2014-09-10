{-| Cut away a value from a sparse collection of ranges in sub-linear time.

Range with holes is represented as a tree, with a non-leaf node
at every split and leaf nodes representing contiguous subranges.
Each split node carries the widths of left and right subtrees.
On a change, only the path including the changed subrange is replaced.
-}

data MultiRangeNode
  = EmptySubrange
  | Subrange Int Int
  | Split Int Int MultiRangeNode MultiRangeNode
  deriving (Eq, Show)

rangeLength :: MultiRangeNode -> Int
rangeLength EmptySubrange = 0
rangeLength (Subrange a b) = b - a + 1
rangeLength (Split len_l len_r _ _) = len_l + len_r

cutAt :: Int -> MultiRangeNode -> MultiRangeNode
cutAt pos mrange
  | otherwise = cut_tree
  where (_, cut_tree) = cutAt_ pos mrange

cutAt_ :: Int -> MultiRangeNode -> (Int, MultiRangeNode)
cutAt_ _ EmptySubrange = error "Extracting from empty subrange"
cutAt_ pos (Subrange l r)
  | pos < 0 = error ("Negative position " ++ (show pos))
  | r == l = if pos == 0 then (0, EmptySubrange)
             else error ("Index " ++ (show pos) ++ " in a 1-wide subrange of " ++ (show l))
  | pos > (r - l) = error ("Cutting at " ++ (show pos) ++ " in " ++ (show (Subrange l r)))
  | pos == 0 = ((r - l), Subrange (l + 1) r)
  | pos == (r - l) = ((r - l), Subrange l (r - 1))
  | otherwise = ((len_l + len_r), Split len_l len_r subrange_l subrange_r)
  where subrange_l = Subrange l l_split
        subrange_r = Subrange r_split r
        len_l = l_split - l + 1
        len_r = r - r_split + 1
        l_split = pos - 1
        r_split = pos + 1
cutAt_ pos (Split len_l  len_r  node_l  node_r)
  | pos > (len_l + len_r) = error (
    "Cutting at " ++ (show pos) ++ " in [" ++ (show len_l) ++ ", " ++ (show len_r) ++ "]")
  | pos < len_l = ((new_len_l +  len_r), Split new_len_l  len_r  new_node_l  node_r)
  | otherwise = ((len_l + new_len_r), Split len_l  new_len_r  node_l  new_node_r)
  where (new_len_l, new_node_l) = cutAt_ pos node_l
        (new_len_r, new_node_r) = cutAt_ (pos - len_l) node_r

