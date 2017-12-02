-- checks if the given set is empty
empty :: [a] -> Bool
empty []  = True
empty set = False

-- finds the union of two sets
union :: Eq a => [a] -> [a] -> [a]
union [] set2 = set2
union (head1:set1) set2
    | elem head1 set2 = union set1 set2
    | otherwise = union set1 (head1:set2)

-- finds the intersection of two sets
intersection :: Eq a => [a] -> [a] -> [a]
intersection [] set2 = []
intersection (head1:set1) set2
    | elem head1 set2 = head1:(intersection set1 set2)
    | otherwise = intersection set1 set2

-- subtracts the second set from the first set
subtraction :: Eq a => [a] -> [a] -> [a]
subtraction [] set2 = []
subtraction (head1:set1) set2
    | elem head1 set2 = subtraction set1 set2
    | otherwise = head1:(subtraction set1 set2)

-- performs the operation on the elements of two sets
operation :: a -> [a] -> (a -> a -> a) -> [a]
operation element [] function = []
operation element (head1:set1) function = (function element head1):(operation element set1 function)

-- minkowski sum
addition :: (Eq a, Num a) => [a] -> [a] -> [a]
addition [] [] = []
addition [] set2 = addition set2 []
addition (head1:set1) set2 = union (operation head1 set2 (\ element1 element2 -> element1 + element2)) (addition set1 set2)

-- takes input and shows output
main :: IO ()
main = do
    print (empty [])
    print (union [1, 2, 3] [3, 4, 5])
    print (intersection [1, 2, 3] [3, 4, 5])
    print (subtraction [1, 2, 3] [3, 4, 5])
    print (addition [1, 2] [1, 2, 4])
