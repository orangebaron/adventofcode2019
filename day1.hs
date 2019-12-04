solve :: Integer -> Integer
solve = (subtract 2) . (`div` 3)

solve2 :: Integer -> Integer
solve2 n
	| sn <= 0 = 0
	| otherwise = sn + (solve2 sn) where
		sn = solve n

main1 = interact $ show . sum . (fmap (solve . read)) . words

main2 = interact $ show . sum . (fmap (solve2 . read)) . words

main = main2
