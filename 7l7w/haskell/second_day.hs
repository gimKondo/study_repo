module Main where
    ----------------------------------------
    -- UTILITY
    ----------------------------------------
    putAny a = putStrLn $ show a
    
    -- remove one element of list
    remove :: (Eq a) => a -> [a] -> [a]
    remove _ [] = []
    remove e (h:t)
        | e == h = t
        | otherwise = h:remove e t
    
    -- ======================================
    --            SELF STUDY
    -- ======================================
    -- sort list
    -- pseudo-select sort
    sortList1 :: (Ord a) => [a] -> [a]
    sortList1 [] = []
    sortList1 lst = min:(sortList1 $ remove min lst)
        where min = minimum lst

    -- sort by given comparer

    -- exchange string to number
    -- e.g. $2, 345, 034, 678.99
    
    -- on given x as parameter,
    -- make lazy sequence starting x and stepping 2
    makeSeqStep2 :: Integer -> [Integer]
    makeSeqStep2 x = [x, x + 2..]

    -- on given y as parameter,
    -- make lazy sequence starting x and stepping 4
    makeSeqStep4 :: Integer -> [Integer]
    makeSeqStep4 y = [y, y + 4..]

    -- on given x and y as parameter,
    -- make lazy sequence starting x + y and stepping 8
    makeSeqStep8 :: Integer -> Integer -> [Integer]
    makeSeqStep8 x y = map ((+ x) . (+ y)) [0, 8..]

    -- get half number using partially applied
    halfNum :: (Fractional a) => a -> a
    halfNum n = (/ 2) n

    -- get string added "\n" using partially applied

    -- get the greatest common divisor
    
    -- make lazy sequence of prime numbers

    -- separate sentence at well-suited verge of word,
    -- and get multiline string

    -- append line number to text noted above

    -- form text noted above to left, right or center justification

    
    main = do
        let orgLst = take 10 [1,4..]
        let invLst = take 12 [30,27..0]
        let rndLst = [4,2,7,3,2,1,6,4,2,9,2,1,9]
        putStrLn "--- original list ---"
        putAny orgLst
        putAny invLst
        putAny rndLst
        putStrLn "--- remove one element from list ---"
        putAny $ remove 7 orgLst
        putStrLn "--- sort list ---"
        putAny $ sortList1 invLst
        putAny $ sortList1 rndLst

        putStrLn "--- number sequence stepping 2 ---"
        putAny $ take 8 $ makeSeqStep2 3
        putStrLn "--- number sequence stepping 4 ---"
        putAny $ take 8 $ makeSeqStep4 9
        putStrLn "--- number sequence stepping 8 ---"
        putAny $ take 8 $ makeSeqStep8 3 9
        putStrLn "--- half number ---"
        putAny $ halfNum 8
        putAny $ halfNum 3

