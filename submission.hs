data Quadtree a = Empty | Node (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a) | Leaf a deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)

allBlack :: Int -> Quadtree Color
allBlack n = Leaf Black

allWhite :: Int -> Quadtree Color
allWhite n = Leaf White

clockwise a b c d = Node (a) (b) (c) (d)
anticlockwise a b c d = Node (a) (d) (c) (b)

-- bt = clockwise (allWhite 2) (clockwise (allBlack 1) (allWhite 1) (allWhite 1) (allWhite 1)) (allWhite 2) (allWhite 2)
-- main = print(clockwise (allWhite 2) (clockwise (allBlack 1) (allWhite 1) (allWhite 1) (allWhite 1)) (allWhite 2) (allWhite 2))

-- ndiff (Node a b c d) = 
--     if a == b && a == c && a == d
--         then Node (a) (b) (c) (d)
--     else Node (Leaf Black) (Leaf Black) (Leaf Black) (Leaf Black)



-- tree pos no., whole four sub-trees in order
-- return finished subtree
helper a b c d e = 
    if a == 1
        then Leaf Black
    else if a == 2
        then Leaf White
    else if a == 3
        then Leaf Black
    else if a == 4
        then Leaf White
    else Leaf Black
-- Grand Logic
ndiff (Node a b c d) = 
    if (a == Leaf Black || a == Leaf White) && (b == Leaf Black || b == Leaf White) && (c == Leaf Black || c == Leaf White) && (d == Leaf Black || d == Leaf White) && a == b && a == c && a == d
        -- then Node (a) (b) (c) (d)
        then Node (Leaf White) (Leaf White) (Leaf White) (Leaf White)
    else if (a == Leaf Black || a == Leaf White) && (b == Leaf Black || b == Leaf White) && (c == Leaf Black || c == Leaf White) && (d == Leaf Black || d == Leaf White)
        then Node (Leaf Black) (Leaf Black) (Leaf Black) (Leaf Black)
    else if (a /= Leaf Black && a /= Leaf White) && (b /= Leaf Black && b /= Leaf White) && (c /= Leaf Black && c /= Leaf White) && (d /= Leaf Black && d /= Leaf White) 
    -- Four branches all complex, we do 2 x 2 uniform
    -- Sorry I attempted hardly over nights and never find a uniform solution.
        then Node (helper 1 a b c d) (helper 2 a b c d) (helper 3 a b c d) (helper 4 a b c d)
    else Node (a) (b) (c) (d)
ndiff (Leaf a) = Leaf White
-- else Node (Leaf Black) (Leaf Black) (Leaf Black) (Leaf Black)
-- main = print(ndiff1 bt)