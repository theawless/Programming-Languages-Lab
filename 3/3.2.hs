import           Data.List
import           Data.Maybe
import           System.IO

-- item data type
data Item = Item {iCode :: String, iName :: String, iPrice :: Int, iQuantity :: Int} deriving Show

-- initial items in store
items = [Item {iCode = "0", iName = "Chicken", iPrice = 50, iQuantity = 5},
    Item {iCode = "1111", iName = "Fish",    iPrice = 30, iQuantity = 5},
    Item {iCode = "2222", iName = "Paneer",  iPrice = 30, iQuantity = 5},
    Item {iCode = "3333", iName = "Tea",     iPrice = 10, iQuantity = 10},
    Item {iCode = "4444", iName = "Water",   iPrice = 10, iQuantity = 20},
    Item {iCode = "5555", iName = "Coffee",  iPrice = 10, iQuantity = 10},
    Item {iCode = "6666", iName = "Samosa",  iPrice = 10, iQuantity = 10},
    Item {iCode = "7777", iName = "Burger",  iPrice = 20, iQuantity = 10},
    Item {iCode = "8888", iName = "Pizza",   iPrice = 40, iQuantity = 5},
    Item {iCode = "9999", iName = "Chips",   iPrice = 10, iQuantity = 20}]

-- get item with code if found
getItem :: String -> [Item] -> Maybe Item
getItem code items = find (\ item -> iCode item == code) items

-- buy item if quantity available
buyItem :: Int -> Item -> Maybe Item
buyItem quantity item
    | quantity > iQuantity item = Nothing
    | otherwise = Just (item {iQuantity = quantity})

-- takes the item from store
takeItem :: Item -> [Item] -> [Item]
takeItem item1 (item2:items)
    | iCode item1 == iCode item2 = item2 {iQuantity = (iQuantity item2 - iQuantity item1)}:items
    | otherwise = item2:(takeItem item1 items)

-- print invoice function
getBill :: Int -> Int -> [Item] -> String
getBill 0 amount orderedItems
    = header ++ (getBill 1 amount orderedItems)
    where
        header = "---------------------------------------------------\n" ++ "ALCHERINGA 2018, STALL 14: TANGO FAST FOOD CENTER\n"
getBill count amount (item:orderedItems)
    = line ++ (getBill (count + 1) (amount + cost) orderedItems)
    where
        code = iCode item
        name = iName item
        quantity = iQuantity item
        price = iPrice item
        cost = quantity * price
        line = show count ++ ". " ++ code ++ " " ++ name ++ " " ++ show price ++ " " ++ show quantity ++ " " ++ show cost ++ " \n"
getBill count amount []
    = "---------------------------------------------------\n" ++ "Total ********************** Rs. " ++ show amount ++ "\n"

-- gets order from remainingItems and stores them in orderedItems
getOrder :: [Item] -> [Item] -> IO ()
getOrder orderedItems remainingItems = do
    putStr "More Items? (Y/N): "
    ordering <- getLine
    case ordering of
        "Y" -> do
            putStr "Enter Code: "
            code <- getLine
            case (getItem code remainingItems) of
                Just gotItem -> do
                    putStrLn ("(" ++ (iName gotItem) ++ ")")
                    putStrLn "Enter Quantity: "
                    quantity <- readLn :: IO Int
                    case (buyItem quantity gotItem) of
                        Just boughtItem -> do
                            putStrLn "AVAILABLE"
                            getOrder (orderedItems ++ [boughtItem]) (takeItem boughtItem remainingItems)
                        Nothing -> do
                            putStrLn "NOT AVAILABLE"
                            getOrder orderedItems remainingItems
                Nothing -> do
                    putStrLn "**WRONG CODE, NO ITEM FOUND**"
                    getOrder orderedItems remainingItems
        "N" -> do
            putStrLn (getBill 0 0 orderedItems)
        _   -> do
            putStrLn "Invalid input."
            getOrder orderedItems remainingItems

-- takes input and shows output
main :: IO ()
main = do
    putStrLn "Welcome to our stall!"
    getOrder [] items
    putStrLn "Thank you for visiting our stall!"
