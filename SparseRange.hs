{-| Cut away a value from a sparse collection of ranges in sub-linear time.

Range with holes is represented as a tree, with a non-leaf node
at every split and leaf nodes representing contiguous subranges.
Each split node carries the widths of left and right subtrees.
On a change, only the path including the changed subrange is replaced.
-}

data SparseRange
  = EmptySubrange
  | Subrange Int Int -- [start::Int, end::Int)
  | SplitRange Int Int SparseRange SparseRange
    -- left_width right_width left right
  deriving (Eq, Show)

rangeLength :: SparseRange -> Int
rangeLength EmptySubrange = 0
rangeLength (Subrange a b) = b - a + 1
rangeLength (SplitRange len_l len_r _ _) = len_l + len_r

{-| Cut an element from a node.
  * pos: position of the element
  * mrange: where to cut from
  * return (the value at position, updated range node)
-}
cutAt :: Int -> SparseRange -> (Int, SparseRange)
cutAt pos mrange = (value, new_mrange)
  where (value, _, new_mrange) = cutAt_ pos mrange

{-| index -> multi-range -> (extracted_number, new_length, new_subrange)
-}
cutAt_ :: Int -> SparseRange -> (Int, Int, SparseRange)
cutAt_ pos _
  | pos < 0 = error ("Cutting at negative position " ++ (show pos))
cutAt_ pos mrange
  | pos >= rangeLength mrange = error (
    "Cutting at " ++ (show pos) ++ " in " ++ (show mrange))
              
cutAt_ pos (Subrange l r)
  | pos == 0 = (l, (r - l), flatSubrange (l + 1) r)
  | pos == (r - l) = (r, (r - l), flatSubrange l (r - 1))
  | otherwise = (l + pos, (len_l + len_r),
                 SplitRange len_l len_r subrange_l subrange_r)
  where subrange_l = flatSubrange l l_split
        subrange_r = flatSubrange r_split r
        l_split = l + pos - 1
        r_split = l + pos + 1
        len_l = l_split - l + 1
        len_r = r - r_split + 1

cutAt_ pos (SplitRange len_l  len_r  node_l  node_r)
  | pos < len_l = (value_l, (new_len_l +  len_r),
                   SplitRange new_len_l  len_r  new_node_l  node_r)
  | otherwise = (value_r, (len_l + new_len_r),
                 SplitRange len_l  new_len_r  node_l  new_node_r)
  where (value_l, new_len_l, new_node_l) = cutAt_ pos node_l
        (value_r, new_len_r, new_node_r) = cutAt_ (pos - len_l) node_r

flatSubrange :: Int -> Int -> SparseRange
flatSubrange l r
  | l > r = EmptySubrange
  | otherwise = Subrange l r


