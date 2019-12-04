import Data.List
import Data.List.Split
import Data.Maybe

data Pos = Pos { x :: Int, y :: Int } deriving (Show)
data Wire = Wire { fstPos :: Pos, sndPos :: Pos }

addPos :: Pos -> Pos -> Pos
addPos p1 p2 = Pos (x p1 + x p2) (y p1 + y p2)

manhattan :: Pos -> Int
manhattan (Pos a b) = (abs a) + (abs b)

insideOnAxis :: Bool -> Pos -> Wire -> Bool
insideOnAxis axisBool a (Wire b c) = f (axis a) (axis b) (axis c) where
	axis = if axisBool then y else x
	f g h i = (g >= h && g <= i) || (g <= h && g >= i)

takeFromAxes :: Pos -> Pos -> Pos
takeFromAxes p1 p2 = Pos (x p1) (y p2)

intersecting :: Wire -> Wire -> (Maybe Pos)
intersecting w1 w2
	| testAxis False = Just $ takeFromAxes (fstPos w1) (fstPos w2)
	| testAxis True  = Just $ takeFromAxes (fstPos w2) (fstPos w1)
	| otherwise      = Nothing
	where
	testAxis ax = testWire ax w1 w2 && testWire (not ax) w2 w1
	testWire ax (Wire a b) w = insideOnAxis ax a w && insideOnAxis ax b w

intersections :: [Wire] -> [Wire] -> [Pos]
intersections l1 l2 = catMaybes $ do
	w1 <- l1
	w2 <- l2
	return $ intersecting w1 w2

dropZeroIfHead :: [Int] -> [Int]
dropZeroIfHead (0:rest) = rest
dropZeroIfHead x = x

bestIntersection :: [Wire] -> [Wire] -> Int
bestIntersection l1 l2 = head $ dropZeroIfHead $ sort $ map manhattan $ intersections l1 l2

makeWires :: Pos -> [String] -> [Wire]
makeWires _ [] = []
makeWires p (s:rest) = (Wire p p2):(makeWires p2 rest) where
	offset ('R':n) = Pos (read n) 0
	offset ('L':n) = Pos (-read n) 0
	offset ('U':n) = Pos 0 (read n)
	offset ('D':n) = Pos 0 (-read n)
	p2 = p `addPos` offset s

main1 :: String -> String
main1 s = show $ bestIntersection (l 0) (l 1) where
	l :: Int -> [Wire]
	l = (makeWires (Pos 0 0)) . (splitOn ",") . (lines s !!)

main = interact main1
