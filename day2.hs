assignElem :: Int -> a -> [a] -> [a]
assignElem _ _ [] = []
assignElem 0 x (_:rest) = x:rest
assignElem n x (a:rest) = a:(assignElem (n-1) x rest)

runThingy :: Int -> [Int] -> [Int]
runThingy x l
	| opcode == 99 = l
	| otherwise = runThingy newX newL
	where
		at = (l !!) . (+ x)
		opcode = at 0
		op = if opcode == 1 then (+) else (*)
		newX = x + 4
		argRef = (l !!) . at
		newL = assignElem (at 3) ((argRef 1) `op` (argRef 2)) l

main1 :: String -> String
main1 = show . head . (runThingy 0) . (assignElem 1 12) . (assignElem 2 2) . read . ('[':) . (++"]")

isValid :: [Int] -> (Int, Int) -> Bool
isValid l (v1,v2) = head (runThingy 0 (assignElem 1 v1 (assignElem 2 v2 l))) == 19690720

runOnList :: [Int] -> Int
runOnList l = (\a -> 100 * (fst a) + (snd a)) $ head $ filter (isValid l) [(a,b) | a <- [0..99], b <- [0..99]]

main2 = show . runOnList . read . ('[':) . (++"]")

main = interact main2
