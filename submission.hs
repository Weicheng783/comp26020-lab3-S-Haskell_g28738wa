data Quadtree a = Empty | Node (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a) | Leaf a deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)

allBlack :: Int -> Quadtree Color
allBlack n = Leaf Black

allWhite :: Int -> Quadtree Color
allWhite n = Leaf White

clockwise a b c d = Node (a) (b) (c) (d)
anticlockwise a b c d = Node (a) (d) (c) (b)

-- input: tree pos no., whole four sub-trees in order [give up]
-- return finished subtree
-- helper a b c d e = 
--     if a == 1
--         then Leaf Black
--     else if a == 2
--         then Leaf White
--     else if a == 3
--         then Leaf Black
--     else if a == 4
--         then Leaf White
--     else Leaf Black

-- Logic of ndiff
ndiff (Node a b c d) = 
    -- First check if the colors of all 2 x 2 squares are the same, return all Leaf White according to rules.
    if (a == Leaf Black || a == Leaf White) && (b == Leaf Black || b == Leaf White) && (c == Leaf Black || c == Leaf White) && (d == Leaf Black || d == Leaf White) && a == b && a == c && a == d
        then Node (Leaf White) (Leaf White) (Leaf White) (Leaf White)
    -- Check if not all the 2 x 2 colors the same, return all Leaf Black according to rules.
    else if (a == Leaf Black || a == Leaf White) && (b == Leaf Black || b == Leaf White) && (c == Leaf Black || c == Leaf White) && (d == Leaf Black || d == Leaf White)
        then Node (Leaf Black) (Leaf Black) (Leaf Black) (Leaf Black)
    -- This case below represents we meet 4 x 4 or above dimensions (have square not being white or black solely).
    else if (a /= Leaf Black && a /= Leaf White) && (b /= Leaf Black && b /= Leaf White) && (c /= Leaf Black && c /= Leaf White) && (d /= Leaf Black && d /= Leaf White) 
    -- [TODO] Four branches all composites.
    -- Sorry I attempted hardly over nights and never find an appropriate solution or may found a solution but very hard to implement in Haskell.
    -- Some thoughts: for each square we need first search neighbours internally, the position numbers inside brackets are assuming if they can be subdivided:
        -- Internal Search: 1 -> 2 (1,4),3 (1),4 (1,2) | 2 -> 3 (1,2), 4 (2), 1(2,3) | 3 -> 4 (2,3), 1(3), 2(3,4) | 4 -> 1 (3,4), 2(4), 3(1,4)
        -- Besides we need to do neighbours search outside this square: 1: diagonal 3, left 2&3, up 3&4.
        -- 2: diagonal 4, up 3&4, right 1&4.
        -- 3: diagonal 1, right 1&4, down 1&2.
        -- 4: diagonal 2, left 2&3, down 1&2.
    -- then Node (helper 1 a b c d) (helper 2 a b c d) (helper 3 a b c d) (helper 4 a b c d) --[code give up here, will fail the tests]
        then Node (a) (b) (c) (d)
    -- Default case
    else Node (a) (b) (c) (d)
-- For 1 x 1 only one square involved, return Leaf White as no neighbors.
ndiff (Leaf a) = Leaf White
