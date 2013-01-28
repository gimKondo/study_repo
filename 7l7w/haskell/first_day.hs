module Main where
	----------------------------------------
	-- UTILITY
	----------------------------------------
	putAny a = putStrLn $ show a
	
	size :: [a] -> Integer
	size [] = 0
	size (h:t) = 1 + size t
	
	----------------------------------------
	-- create list
	----------------------------------------
	--
	allEven :: [Integer] -> [Integer]
	allEven [] = []
	allEven (h:t) = if even h then h:allEven t else allEven t
	
	-- ======================================
	--            SELF STUDY
	-- ======================================
	-- variation of allEven 
	-- using comprehension
	allEven1 :: [Integer] -> [Integer]
	allEven1 lst = [a | a <- lst, (mod a 2) == 0]
	-- using 'filter' with unnamed function
	allEven2 :: [Integer] -> [Integer]
	allEven2 lst = filter (\a -> (mod a 2) == 0) lst
	
	-- make reversed list
	reverseList :: [a] -> [a]
	reverseList [] = []
	reverseList (h:t) = (reverseList t) ++ [h]
	-- take last element
	reverseList2 :: [a] -> [a]
	reverseList2 [] = []
	reverseList2 lst = (last lst):(reverseList2 $ init lst)
	-- use foldr
	reverseList3 :: [a] -> [a]
	reverseList3 lst = foldr (\fst sec -> sec ++ [fst]) [] lst

	-- make all pair from list [black, white, blue, yellow, red]
	-- !ATTENTION! don't include pair that has only different order
	-- 	   EX. (blue, red) and (red, blue)
	makeColorPair :: [String] -> [(String, String)]
	makeColorPair [] = []
	makeColorPair lst = [(a,b) | a <- lst, b <- lst, a < b]

	-- make multiplication table
	makeMultiTable :: Integer -> [(Integer, Integer, Integer)]
	makeMultiTable mx = do 
		let arr = [1..mx]
		[(a, b, a * b) | a <- arr, b <- arr]
	
	main = do
		let lst = take 10 [1,4..]
		putStrLn "--- original list ---"
		putAny $ lst
		putStrLn "--- list extracted just even numbers ---"
		putAny $ allEven1 lst
		putAny $ allEven2 lst
		putStrLn "--- reversed list ---"
		putAny $ reverseList lst
		putAny $ reverseList2 lst
		putAny $ reverseList3 lst
		putStrLn "--- create unique pair of color ---"
		let colLst = ["black", "white", "blue", "yellow", "red"]
		putAny $ makeColorPair colLst
		putAny $ size $ makeColorPair colLst
		putStrLn "--- create multiplication table from 1 to 12 ---"
		putAny $ makeMultiTable 12
		putAny $ size $ makeMultiTable 12
		putStrLn "--- color-code map ---"
		let colorCodedMap = do
			let triColor = ["red", "green", "blue"]
			[("Alabama " ++ a, "Mississippi " ++ m, "Georgia " ++ g,
		  	  "Tennessee " ++ t, "Florida " ++ f) | 
				a <- triColor, 
				m <- triColor,
				g <- triColor,
				t <- triColor,
				f <- triColor,
			  	and [a /= t, a /= m, a /= g, a /= f, m /= t, g /= t, g /= f]]
		putAny $ colorCodedMap
		putAny $ size colorCodedMap


