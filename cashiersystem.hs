--Cashier System

module Main where
import Data.List
import Text.CSV
import Text.Read

main = do 
  menu >>= process

-- data types
data Item = Item {itemName :: String, itemPrice :: Double, quantity :: Int} deriving (Show)
data Day = Monday | Tuesday | Wednesday| Thursday | Friday | Saturday | Sunday deriving (Show)

-- types
type Cart = [Item]
type CSVFormatCart = [String]

-- instance for Day data type
instance Eq Day where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False

-- function to convert CSV into Cart
getCart:: CSV -> Cart 
getCart csv = fmap (\record -> Item {itemName = record !! 0, itemPrice = read $ record !! 1, quantity = read $ record !! 2}) (tail csv)

-- function for main menu's flow
process :: String -> IO () 
process "1" = do
  cartCSV <- parseCSVFromFile "cart.csv"
  case cartCSV of 
    Left err -> print err
    Right csv -> do
      let cart = getCart csv
      displayAddItemsToCart >>= (\item -> addItemsToCart item cart) >>= updateCSV >> menu >>= process
process "2" = do 
  cartCSV <- parseCSVFromFile "cart.csv"
  case cartCSV of 
    Left err -> print err
    Right csv -> do
      let cart = getCart csv
      choice <- displayViewCart cart 
      processViewCart choice cart
process "3" =  do
  cartCSV <- parseCSVFromFile "cart.csv"
  case cartCSV of 
    Left err -> print err
    Right csv -> do
      let cart = getCart csv
      enterCurrentDay >>= displayTotal cart
      choice <- getLine
      processCheckout choice
process "4" = do
        putStrLn "         " 
        putStrLn ".............................................."
        putStrLn "Thank you for using Functional Cashier System"
        putStrLn ".............................................."
process _ = do
  putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
  putStrLn "              Invalid input."
  putStrLn "         Please enter between 1-4"
  putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  menu >>= process 

-- function to print menu and get the user's menu input
menu:: IO String 
menu = do 
  putStrLn "         " 
  putStrLn "         " 
  putStrLn "=============================================="
  putStrLn "Welcome to the Functional Cashier System!"
  putStrLn "=============================================="
  putStrLn "1. Add item to cart"
  putStrLn "2. View cart"
  putStrLn "3. Checkout"
  putStrLn "4. Quit Program "
  putStrLn "----------------------------------------------"
  putStrLn "Enter your choice: (1-4)"
  putStrLn "=============================================="
  putStrLn "         " 
  getLine 

-- function to display the UI for adding items and returns the items data type that was created
displayAddItemsToCart:: IO Item
displayAddItemsToCart = do
  putStrLn "=============================================="
  putStrLn "Enter item Name (e.g sprite)"
  putStrLn "=============================================="
  name <- getLine 
  putStrLn "=============================================="
  putStrLn "Enter price per quantity (e.g 3.0)"
  putStrLn "=============================================="
  price <- readPrice 
  putStrLn "=============================================="
  putStrLn "How many quantity? (e.g 4)"
  putStrLn "=============================================="
  quantity <- readQuantity
  putStrLn "=============================================="
  let item = Item name price quantity
  return item

-- function to read the input for price and check whether it is a Double
readPrice :: IO Double
readPrice = do
  priceString <- getLine
  let price = readMaybe priceString 
  case price of
    Just price -> return price
    Nothing -> do
      putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
      putStrLn "              Invalid input."
      putStrLn "       Please enter a number (e.g 3.0)"
      putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      putStrLn "         " 
      putStrLn "=============================================="
      putStrLn "Enter price per quantity (e.g 3.0)"
      putStrLn "=============================================="
      readPrice


-- function to read the input for quantity and check whether it is an Integer
readQuantity :: IO Int
readQuantity = do
  quantityString <- getLine
  let quantity = readMaybe quantityString
  case quantity of
    Just quantity -> return quantity
    Nothing -> do
      putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
      putStrLn "              Invalid input."
      putStrLn "       Please enter a number (e.g 4)"
      putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      putStrLn "         " 
      putStrLn "=============================================="
      putStrLn "How many quantity? (e.g 4)"
      putStrLn "=============================================="
      readQuantity

-- function that add items to cart and returns the updated cart
addItemsToCart :: Item -> Cart -> IO Cart
addItemsToCart item cart = do
                let updatedCart = cart ++ [item]
                putStrLn ""
                putStrLn ""
                putStrLn "----------------------------------------------"
                putStrLn $ itemName item ++ " added to Cart"
                putStrLn "----------------------------------------------"
                return updatedCart

-- function to check whether the cart is empty
checkEmpty :: Cart -> Bool
checkEmpty cart
  | length cart == 0 = True
  | otherwise = False

-- function to update the CSV file 
-- if it is empty, it will only add the header which is name,price,quantity
-- if it is not empty, it will add the header together with the current items in the cart
-- the check for empty ensures that the error where a blank space in the csv file (which causes "index too large") does not happen.
updateCSV :: Cart -> IO()
updateCSV cart = do
  let isEmpty = checkEmpty cart
  if isEmpty == False
    then do
    let updatecsv = "itemName,itemPrice,quantity\n" ++ (intercalate "\n" (listToCSVFormat cart))
    writeFile "cart.csv" updatecsv
  else do
    let emptyCSV = "itemName,itemPrice,quantity"
    writeFile "cart.csv" emptyCSV

-- function to convert the items into csv format
csvFormat :: Item -> String 
csvFormat item = itemName item ++ "," ++  show(itemPrice item) ++ "," ++ show(quantity item) 

-- function to convert items to display in view cart
viewCartFormat :: Item -> String 
viewCartFormat item = itemName item ++ "|RM " ++  show(itemPrice item) ++ "|" ++ show(quantity item) 

-- converts every item in cart into csv format
listToCSVFormat :: Cart -> CSVFormatCart
listToCSVFormat cart = csvFormat <$> (cart)

-- function for viewing cart menu's flow
processViewCart:: String -> Cart -> IO()
processViewCart "1" cart = do
  cartCSV <- parseCSVFromFile "cart.csv"
  case cartCSV of
    Left err -> print err
    Right csv -> do
      itemToRemove <- displayRemoveItem  
      removeItem cart itemToRemove  >>= updateCSV 
      process "2"
processViewCart "2" cart = do
  menu >>= process
processViewCart _ cart = do
  putStrLn ""
  putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
  putStrLn "                Invalid input."
  putStrLn "          Please enter between 1 or 2"
  putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  displayViewCart cart >>= (\x -> processViewCart x cart)

-- function that displays the current cart and the menu for view cart
displayViewCart :: Cart -> IO String
displayViewCart cart = do
  putStrLn ""
  putStrLn ""
  putStrLn "=============================================="
  putStrLn "                 View Cart"
  putStrLn "=============================================="
  viewCart cart
  putStrLn "=============================================="
  putStrLn "Enter 1 to remove item"
  putStrLn "Enter 2 to leave"
  putStrLn "=============================================="
  getLine 

-- function to display the cart 
viewCart :: Cart -> IO ()
viewCart cart = do
  let displayCartFormat = intercalate "\n" (viewCartFormat <$> cart)
  putStrLn "item|price|quantity"
  putStrLn "----------------------------------------------"
  putStrLn displayCartFormat 

-- function that gets the input from the user on which item to remove
displayRemoveItem :: IO String
displayRemoveItem = do
  putStrLn ""
  putStrLn ""
  putStrLn "=============================================="
  putStrLn "Input name of item to remove"
  putStrLn "=============================================="
  getLine

-- function that removes the items from the cart if it is found in the cart
removeItem :: Cart -> String -> IO Cart 
removeItem cart name = do
  let removedItemCart = filter (\x -> itemName x == name) cart
  let newItemCart = filter (\x -> itemName x /= name) cart
  let checkResult = checkEmpty removedItemCart
  if checkResult == False
    then do
    putStrLn "         " 
    putStrLn "         " 
    putStrLn "         " 
    putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
    putStrLn $ name ++ " has been removed from the cart"
    putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
    return newItemCart
  else do
    putStrLn "         " 
    putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
    putStrLn "Item not found."
    putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
    putStrLn "         " 
    return cart

-- function for user to enter the current day 
enterCurrentDay :: IO Day
enterCurrentDay = do
  putStrLn "         " 
  putStrLn "         "
  putStrLn "=============================================="
  putStrLn "Enter current day (Monday,Tuesday,Wednesday"
  putStrLn ",Thursday,Friday,Saturday,Sunday)"
  putStrLn "=============================================="
  day <- getLine
  case day of 
    "Monday" -> return Monday
    "Tuesday" -> return Tuesday
    "Wednesday" -> return Wednesday
    "Thursday" -> return Thursday
    "Friday" -> return Friday
    "Saturday" -> return Saturday
    "Sunday" -> return Sunday
    _ -> do 
      putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
      putStrLn "                Invalid input."
      putStrLn "    Please enter a day (e.g: Monday, Tuesday)"
      putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      enterCurrentDay
    
-- function to get user choice for after checking out
getCheckoutChoice :: IO String
getCheckoutChoice = do 
  putStrLn "Enter 1 to continue using this cashier system"
  putStrLn "Enter 2 to exit"
  putStrLn "=============================================="
  getLine

-- function for the flow after user enter their choice after checking out
processCheckout :: String -> IO()
processCheckout "1" = do 
  putStrLn "=============================================="
  putStrLn "Cart has been cleared." 
  putStrLn "----------------------------------------------" 
  putStrLn "Going back to Main Menu."
  putStrLn "=============================================="
  menu >>= process
processCheckout "2" = do
  process "4"
processCheckout _ = do
  putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 
  putStrLn "                 Invalid input."
  putStrLn "               Please enter 1-2"
  putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  putStrLn "=============================================="
  getCheckoutChoice >>= processCheckout


-- function that combines the calculation functions together and displays the total price 
-- it will also empty the cart at the end
displayTotal :: Cart -> Day -> IO()
displayTotal items currentDay = do
  let itemsTotal = calculateTotal items
  let tax = calculateTax (calculateTotal items)
  let isDiscountDay = checkDiscountDay currentDay
  let itemsTotalFinal = discountDayOffer isDiscountDay itemsTotal
  let finalTotal = itemsTotalFinal + tax
  displayDiscount (checkDiscountDay currentDay)
  putStrLn "=============================================="
  putStrLn "                   Checkout"
  putStrLn "=============================================="
  putStrLn $ "Total price of items: RM" ++ show(itemsTotalFinal)
  putStrLn $ "Tax: RM" ++ show(tax)
  putStrLn "----------------------------------------------"
  putStrLn $ "Final Total: RM" ++ show(finalTotal)
  putStrLn "=============================================="
  putStrLn "Enter 1 to continue using this cashier system"
  putStrLn "Enter 2 to exit"
  putStrLn "=============================================="
  let newCart = []
  updateCSV newCart

-- calculate total price of the whole cart
calculateTotal :: Cart -> Double
calculateTotal cart = sum $ map(\item -> itemPrice item * fromIntegral(quantity item)) cart

-- calculate the tax
calculateTax :: Fractional a => a -> a
calculateTax total = total * 6/100

-- check if the current day is discount day
-- If it is a tuesday, it will return True as it is the discount day
checkDiscountDay :: Day -> Bool
checkDiscountDay  currentDay 
  | currentDay == Tuesday = True
  | currentDay == Sunday = True
  | otherwise = False

-- function that give discount to the total if the current day is discount day
discountDayOffer :: Fractional a => Bool -> a -> a 
discountDayOffer discountDay total =
  if discountDay 
    then (total - (total * 10/100))
  else total  

-- function to display that the current day is discount day
displayDiscount :: Bool -> IO()
displayDiscount discountDay = 
  if discountDay
    then do
      putStrLn ""
      putStrLn ""
      putStrLn "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
      putStrLn "              It is discount day!"
      putStrLn "     10% discount for total price of items"
      putStrLn "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
  else
    putStrLn ""