import Data.List
import Control.Monad

data Point a = P a a deriving (Show, Eq, Ord) 
data Tour a = T [Point a] deriving (Show)
data Turn = TStraight | TLeft | TRight deriving (Show, Eq, Ord, Enum)

-- Graham scann (ConvexHull)
cmpPoints :: (Num a, Ord a) => Point a -> Point a -> Ordering
cmpPoints (P x1 y1) (P x2 y2)
	| compare x1 x2 == EQ = compare y1 y2
    | otherwise = compare x1 x2

findMinX :: (Num a, Ord a) => [Point a] -> [Point a]
findMinX xs = sortBy cmpPoints xs

cmpAngle :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
cmpAngle (P x1 y1) (P x2 y2) (P x3 y3) = 
	compare ((y1 - y3)*(x2 - x3)) ((y2-y3)*(x1-x3))

sortByAngle :: (Num a, Ord a) => [Point a] -> [Point a]
sortByAngle (x:xs) = x:sortBy (\x1 x2 -> cmpAngle x1 x2 x) xs

findTurn :: (Num a, Ord a, Eq a) => Point a -> Point a -> Point a -> Turn
findTurn (P x1 y1) (P x2 y2) (P x3 y3)
	| (y2 - y1)*(x3-x1) < (y3-y1)*(x2-x1) = TLeft
	| (y2 - y1)*(x3-x1) > (y3-y1)*(x2-x1) = TRight
	| otherwise = TStraight

findHull :: (Num a, Ord a) => [Point a] -> [Point a] -> [Point a]
findHull [x] (z:ys) = findHull [z,x] ys
findHull xs [] = xs
findHull (y:x:xs) (z:ys)
	| findTurn x y z == TRight = findHull (x:xs) (z:ys)
	| findTurn x y z == TStraight = findHull (x:xs) (z:ys)
	| otherwise = findHull (z:y:x:xs) ys

convexHull :: (Num a, Ord a) => [Point a] -> [Point a]
convexHull xs = reverse . findHull [y,x] $ ys where 
    (x:y:ys) = sortByAngle.findMinX $ xs

-- Every rotation of list (first is original list)
rotate :: [a] -> [[a]]
rotate xs = init $ zipWith (++) (tails xs) (inits xs)

-- Two point distance
distance :: Floating a => Point a -> Point a -> a
distance (P x1 y1) (P x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

-- Length of tour
length :: Floating a => Tour a -> a
length (T ts) = sum $ zipWith distance ts $ rotate ts !! 1

-- Compare length of two tours
cmpTourLength :: (Ord a, Floating a) => Tour a -> Tour a -> Ordering
cmpTourLength t1 t2 = compare (Main.length t1) (Main.length t2)

-- Point to line distance
p2l :: Floating a => Point a -> Point a -> Point a -> a
p2l (P x0 y0) p1@(P x1 y1) p2@(P x2 y2) = numerator / denominator
    where numerator = abs $ (x2-x1)*(y1-y0)-(x1-x0)*(y2-y1)
          denominator = distance p1 p2

-- Point to tour minimal distance
p2t :: (Ord a, Floating a) => Tour a -> Point a -> a
p2t (T ts) p = minimum $ zipWith (p2l p) ts (rotate ts !! 1)

-- Compare minimal distance to tour of two points
cmpToTour :: (Ord a, Floating a) => Tour a -> Point a -> Point a -> Ordering
cmpToTour t p1 p2 = let part = p2t t in compare (part p1) (part p2)



-- Point witch minimal distance to tour is maximal
maxMinDistanceToTour :: (Ord a, Floating a) => Tour a -> [Point a] -> Point a
maxMinDistanceToTour t ps = maximumBy (cmpToTour t) ps

-- Insert point to tour in farthest insertion fashion
farthestInsert :: (Eq a, Ord a, Floating a) => Tour a -> Point a -> Tour a
farthestInsert tour@(T (t:ts)) p = farthestInsertHelper [t] ts
    where distToTour = p2t tour p
          farthestInsertHelper xs [] = T $ xs ++ [p]
          farthestInsertHelper allx@(x:xs) ally@(y:ys) = if (p2l p x y) == distToTour then
                                               T $ (reverse allx) ++ (p:ally)
                                           else
                                               farthestInsertHelper (y:allx) ys
									
-- Estimate TSP using fasthest insert "heuristic"	   
tspFarthest :: (Eq a, Ord a, Floating a) => [Point a] -> Tour a
tspFarthest ps = tspFarthestHelper (T $ hull) (ps \\ hull)
    where hull = convexHull ps
          tspFarthestHelper tour [] = tour
          tspFarthestHelper tour ps = tspFarthestHelper (farthestInsert tour p) rest
              where p = maxMinDistanceToTour tour ps
                    rest = filter (/= p) ps

extract :: Tour a -> [Point a]
extract (T ps) = ps

processIt :: [String] -> [Point Float]
processIt [] = []
processIt (l:lines) = do
    (P a b):(processIt lines)
    where w = words l
          a = read $ w !! 0
          b = read $ w !! 1

main = do
    content <- getContents
    mapM (putStrLn.out) $ solved content
    where out (P a b) = unwords [show a,show b]
          solved = extract.tspFarthest.processIt.lines
    