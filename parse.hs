--Will Browne
--09389822
module Project where

import IO
import System.Environment
import System.IO
import System.Exit
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Calendar
import Locale
import Data.Time.Format
import Data.Char
import Data.List
import Data.Maybe
import Text.Regex

liftUp :: (String -> Program -> (Program,String)) ->
          String ->
          Program ->
          IO (Program,String)
liftUp f s p = return (f s p)

--QUIT
quit :: [Char] -> Program -> IO (Program, [Char])
quit _ p | (state p) == True = exitWith ExitSuccess
         | otherwise = do
                        putStrLn "Data has not been saved. Do you want to save before quitting? (y/n)"
                        s <- getLine
                        if s == "n" then
                            exitWith ExitSuccess
                        else if s == "y" then
                            do
                            putStrLn "What do you want to name your file?"
                            file <- getLine
                            (prog, mess) <- save file p
                            exitWith ExitSuccess
                            else
                                return (p, "Bad input. Aborting quit for your own safety.")

--END QUIT

--LOAD
load :: [Char] -> IO (Program, [Char])
load f = catch (
            do
            inh <- openFile f ReadMode
            inpStr <- hGetContents inh
            let x = parse inpStr 1 0 []
            let p = Program x [] True f
            return (p, "CSV successfully loaded: " ++ f ++ "\nPlease enter command or type help\n")
            ) $ \_ -> do
                      hPutStrLn stderr ("Error opening file. Aborting...")
                      return ((Program [] [] True []), "Please try again!\n")
--END LOAD

--SAVE
save :: [Char] -> Program -> IO (Program, [Char])   
save "" p = return (p, "No filename specified. Save failed.")
save filename p = do writeFile filename (output (csv p) 1 0)
                     return ((Program (csv p) (cur p) True filename), filename ++ " saved!")
--END SAVE

curTime :: IO String
curTime = do now <- getCurrentTime
             return $ formatTime defaultTimeLocale "%Y-%m-%d" now

dispatch :: [([Char], [Char] -> Program -> IO (Program, [Char]))]
dispatch =  [ ("insert", liftUp insert'),
              ("select", liftUp select),
              ("delete", liftUp delete'),
              ("reformat", liftUp reformat),
              ("datefix", liftUp (datefix monthsList)),
              ("gridfix", liftUp gridfix),
              ("distinct", liftUp distinct),
              ("count", liftUp count),
              ("list", liftUp list),
              ("update", liftUp (update columns)),
              ("report", report),
              ("help", liftUp help),
              ("save", save),
              ("?", liftUp help),
              ("q", quit),
              ("quit", quit),
              ("show", liftUp show'),
              ("print", liftUp print')
            ]

monthsList = [ ("01", "january", "jan"),
           ("02", "february", "feb"),
           ("03", "march", "mar"),
           ("04", "april", "apr"),
           ("05", "may", "may"),
           ("06", "june", "jun"),
           ("07", "july", "jul"),
           ("08", "august", "aug"),
           ("09", "september", "sep"),
           ("10", "october", "oct"),
           ("11", "november", "nov"),
           ("12", "december", "dec")
        ]

columns = [(1, "Club"),
           (2, "Map Name"),
           (3, "Nearest Town"),
           (4, "Terrain"),
           (5, "Map Grade"),
           (6, "Grid ref of SW corner"),
           (7, "Grid ref of NE corner"),
           (8, "Expected Completion Date"),
           (9, "Size, sq km")
          ]

output :: [DataVal] -> Int -> Int -> String
output [] _ _ = []
output full@(DataVal x y col val : xs) expectedx expectedy
        | x > expectedx = "," ++ (output full (expectedx+1) expectedy) -- add empty
        | y > expectedy = "\n" ++ (output full 1 (expectedy+1))
        | x == expectedx && y == expectedy = (show val) ++ "," ++ (output xs (expectedx+1) expectedy)

output' :: [DataVal] -> Int -> Int -> String
output' [] _ _ = []
output' full@(DataVal x y col val : xs) expectedx expectedy 
        | x > expectedx = "," ++ (output' full (expectedx+1) expectedy) -- add empty
        | y > expectedy = "\n" ++ (output' full 1 (expectedy+1))
        | x == 1 && x == expectedx && y == expectedy =  (show y) ++ ": " ++ val ++ "," ++ (output' xs (expectedx+1) expectedy)
        | x == expectedx && y == expectedy = val ++ "," ++ (output' xs (expectedx+1) expectedy)


show' :: [Char] -> Program -> (Program, [Char])
show' _ p@(Program _ [] _ _) = (p, "Sorry you have not made a valid selection! Try run select first.")
show' _ p = (p, "Currently selected rows:\n" ++ (output' (cur p) 1 0))

--Represents a single cell in the DataVal
data DataVal = DataVal {
        x :: Int,
        y :: Int,
        col :: [Char],
        val :: [Char]
    } deriving (Eq)

instance Show DataVal where
 show (DataVal x y col val) | x == 9 = val ++ "\n"
                            | (y == 0) = printHeader val
                            | x == 1 = (show y) ++ ": " ++ val
                            | otherwise = val ++ " "

printHeader cur = "| " ++ cur ++ " |"

--Represents the program state (stores the CSV file, any currently selected rows, whether the file has been saved)
data Program = Program {
                csv :: [DataVal],
                cur :: [DataVal],
                state :: Bool,
                file :: String   
    } deriving (Eq)

instance Show Program where
  show p@(Program csv cur s f) = (output csv 1 0)

main :: IO ()
main = do
                putStrLn "Please enter CSV filename to load"
                filename <- getLine
                if not (null filename) then
                    do
                    (p, m) <- load filename
                    if not (null (file p)) then
                        do
                        putStr m
                        main' (p, m)
                    else
                        main
                else
                    main

{-
main :: IO ()
main = do
                    (p, m) <- load "map-register.csv"
                    --print (csv p)
                    putStr m
                    main' (p, m)
-}
--Main loop
main' :: (Program, [Char]) -> IO ()
main' (p, m) = do
                s <- getLine
                if null s then
                    main' (p, "")
                else
                    do
                let (cmd : args) = words s
                let arg = concat args
                let maybeAction = lookup cmd dispatch
                if isJust maybeAction && not (null arg) then
                    execute (fromJust maybeAction) arg p
                else --further check here for quit and save
                    if isJust maybeAction && null arg then
                        execute (fromJust maybeAction) [] p
                    else
                    do
                putStrLn "Sorry I did not understand."
                putStrLn "Please try again!"
                main' (p, "")

--Return CSV as list of rows
getListRows :: [DataVal] -> Int -> [[DataVal]]
getListRows [] _ = [[]]
getListRows csv 10 = [csv]
getListRows csv row = [getRow row csv] ++ getListRows csv (row+1)

--Function used to execute function found in dispatch
execute :: ([Char] -> Program -> IO (Program, [Char])) -> [Char] -> Program -> IO ()
execute action args p = do
                    (prog, message) <- action args p
                    putStrLn message
                    main' (prog, "") -- recursive call to main loop

print' :: [Char] -> Program -> (Program, [Char])
print' _ p@(Program [] cur s f) = (p, "Sorry. Nothing to print!")
print' _ p = (p, output' (csv p) 1 0)

--GRIDFIX
gridfix :: [Char] -> Program -> (Program, [Char])
gridfix [] p = (p, "Empty grid-fix query!")
gridfix (x:y:xs) p
   | x == '$' && (y == '6' || y == '7') = (gridFormat xs p (digitToInt y), "Grid-fix successful!")
   | isSpace x = gridfix (y:xs) p --Call again if whitespace is met
   | otherwise = (p, "Grid-fix returned nothing. Invalid query. Please try again.")

gridFormat :: [Char] -> Program -> Int -> Program
gridFormat [] p _ = p
gridFormat str p col | stripWhite str == "4" = Program (ensure (digitToInt '4') col (csv p)) (cur p) False (file p)
                     | stripWhite str == "6" = Program (ensure (digitToInt '6') col (csv p)) (cur p) False (file p)
                     | otherwise = p
                    
ensure :: Int -> Int -> [DataVal] -> [DataVal]
ensure _ 0 csv = csv
ensure _ _ [] = []
ensure len col csv@(cur:rest)
    | (x cur) == col && checkGrid cur == True && check4 cur && len == 4 = cur : ensure len col rest
    | (x cur) == col && checkGrid cur == True && check4 cur && len == 6 = (DataVal (x cur) (y cur) "" ((val cur)++"00")) : ensure len col rest
    | (x cur) == col && checkGrid cur == True && check6 cur && len == 4 = (DataVal (x cur) (y cur) "" (init(init(val cur)))) : ensure len col rest
    | (x cur) == col && checkGrid cur == True && check6 cur && len == 6 = cur : ensure len col rest
    | (x cur) == col && checkBgrid cur == True = rearrangeGrid cur len : ensure len col rest
    | otherwise = cur : ensure len col rest

rearrangeGrid :: DataVal -> Int -> DataVal
rearrangeGrid (DataVal x y col val) len = DataVal x y col (head ts : (hs ++ tail ts))
                                            where (hs, ts) = splitAt len val

checkBgrid :: DataVal -> Bool
checkBgrid cur | checkNum (init (val cur))
                    && checkAlpha (tail (val cur)) = True
               | otherwise = False

checkGrid :: DataVal -> Bool
checkGrid csv | isAlpha (head (val csv))
                    && checkNum (tail(val csv)) = True
              | otherwise = False

check4 :: DataVal -> Bool
check4 csv | length (tail(val csv)) == 4 = True
           | otherwise = False

check6 :: DataVal -> Bool
check6 csv | length (tail(val csv)) == 6 = True
           | otherwise = False

replace :: [DataVal] -> Int -> [DataVal] -> [DataVal]
replace [] _ csv = csv
replace _ 0 [] = []
replace (cur:next) col (csv:rest)
    | col == (y csv) = (assign csv (val cur)) : replace next col rest
    | otherwise = replace [cur] col rest

--Given a dataVal and a string, assign string to new dataVal
assign :: DataVal -> [Char] -> DataVal
assign (DataVal x y col val) new = DataVal x y col new

--END GRIDREF

--SORT
sort' :: [DataVal] -> [String]
sort' [] = []
sort' csv@(cur:rest) = sort (toString (getCol 2 csv))

--END SORT

--quicksort :: [String] -> [DataVal] -> (Program, [Char])
quicksort [] = []
quicksort (x:xs) = quicksort small ++ mid ++ (x : quicksort large)
    where small = [y | y <- xs, y < x]
          mid = [y | y <- xs, y == x]
          large = [y | y <- xs, y > x]


--DATEFIX
datefix :: [(String, String, String)] -> [Char] -> Program -> (Program, [Char])
datefix _ [] p = (p, "Empty date-fix query!")
datefix [] _ p = (p, "Missing months list. Technical Error.")
datefix months conv p
    | stripWhite conv == "%Y-%m-%d" = (Program (splitDate (csv p) months) (cur p) False (file p), "Date-fix was successful")
    | otherwise = (p, "Date-fix returned nothing. Invalid format string.")

--Splits a string at the supplied character
split :: Char -> [Char] -> [Char] -> [String]
split _ [] partial = [partial]
split s str@(x:xs) partial | x == s = [partial] ++ split s xs []
                           | null [s] = split ' ' str partial
                           | otherwise = split s xs (partial ++ [x])

--Searches string for a certain character
searchSeperator :: Char -> [Char] -> [Char]
searchSeperator _ [] = []
searchSeperator sep (x:xs) | x == sep = [sep]
                           | otherwise = searchSeperator sep xs

--Searches string for a certain character
searchSeperator' :: Char -> [Char] -> Bool
searchSeperator' _ [] = False
searchSeperator' sep (x:xs) | x == sep = True
                            | otherwise = searchSeperator' sep xs

splitDate :: [DataVal] -> [(String, String, String)]  -> [DataVal] 
splitDate [] _ = []
splitDate _ [] = [] -- Empty months list provided
splitDate (cur:rest) months
    | searchSeperator' '-' (val cur) == True && (x cur) == 8 = [(DataVal (x cur) (y cur) (col cur) (dateF (split '-' (val cur) []) months))] ++ splitDate rest months
    | searchSeperator' '/' (val cur) == True && (x cur) == 8 = [(DataVal (x cur) (y cur) (col cur) (dateF (split '/' (val cur) []) months))] ++ splitDate rest months
    | searchSeperator' ' ' (val cur) == True && (x cur) == 8 = [(DataVal (x cur) (y cur) (col cur) (dateF (split ' ' (val cur) []) months))] ++ splitDate rest months
    | checkNum (head (words (val cur))) && length (stripWhite (val cur)) == 4 && (x cur) == 8 = [(DataVal (x cur) (y cur) (col cur) ((val cur) ++ "-01-01"))] ++ splitDate rest months
    | otherwise = [cur] ++ splitDate rest months

checkSeason :: [String] -> [Char]
checkSeason [] = []
checkSeason (x:xs) | isSeason x = checkSeason xs ++ getSeason (stripWhite (map toLower x))
                   | checkNum x = dateF' [x] ++ "-"
                   | otherwise = "????" -- default year

dateF :: [String] -> [(String, String, String)] -> [Char]
dateF [] _ = []
dateF _ [] = []
dateF str@(x:xs) ((monNum,monStr,monStr3):rest) 
    | map toLower x == monStr || map toLower x == monStr3 = dateF' xs ++ "-01-" ++ monNum 
    | isSeason x = checkSeason str
    | checkNum x = dateF' str ++ "-31-12"
    | otherwise = dateF str rest

dateF' :: [String] -> [Char]
dateF' [] = []
dateF' (x:xs) | checkNum x && length (stripWhite x) == 4 = x
              | checkNum x && length (stripWhite x) == 2 = "20" ++ x -- ??????????
              | otherwise = dateF' xs 

getSeason :: [Char] -> [Char]
getSeason "spring" = "30-04"
getSeason "summer" = "31-07"
getSeason "autumn" = "31-10"
getSeason "winter" = "31-01"
getSeason _        = "01-01" -- or blank?

isSeason :: [Char] -> Bool
isSeason [] = False
isSeason season =
  case (map toLower season) of
     "autumn" -> True
     "spring" -> True
     "summer" -> True
     "winter" -> True
     _        -> False
--END DATEFIX

--PARSE
parse :: [Char] -> Int -> Int -> [Char] -> [DataVal]                        
parse [] _ _ _ = [] --Last pattern to be match (EOF)
parse (',':zs) x y [] = parse zs (x+1) y []
parse str@(z:zs) x y partial | z == ','  = DataVal x y "" partial : (parse zs (x+1) y "")
                             | z == '\r' = parse zs 1 (y+1) ""
                             | z == '"'  = parseQuotes zs x y partial
                             | otherwise = parse zs x y (partial++[z])
                                                                   
parseQuotes str@(z:zs) x y partial | z == '"' = (parse zs x y partial)
                                   | otherwise = parseQuotes zs x y (partial++[z])
--END PARSE

--function that retrieves that last row number of csv file
lastRow :: Program -> Int
lastRow (Program ( [] ) _ _ _) = 0
lastRow (Program (DataVal x y col val : []) _ _ _) = y
lastRow (Program (DataVal x y col val : xs) cur s f) = lastRow (Program (xs) cur s f)

--function that strips whitespace from a supplied string
stripWhite :: [Char] -> [Char]
stripWhite [] = ""
stripWhite (x:xs) | isSpace x = stripWhite xs
                  | otherwise = [x] ++ stripWhite xs

--INSERT
insert' :: [Char] -> Program -> (Program, [Char])
insert' [] p = (p, "Empty insert statement!")
insert' ins@(x:xs) p@(Program csv cur s f)
   | isJust newData = (Program (csv ++ (fromJust newData)) cur False f, "Insert was successful.")
   | otherwise = (Program csv cur s f, "Insert changed nothing!")
    where newData = parseInsert ins p 

parseInsert :: [Char] -> Program -> Maybe [DataVal]
parseInsert [] _ = Just [] --Reached end of command
parseInsert (x:xs) p | isSpace x = parseInsert xs p
                     | x == '$' = getColNum xs p
                     | otherwise = Nothing

getColNum :: [Char] -> Program -> Maybe [DataVal]
getColNum [] _ = Nothing -- No col number specified
getColNum (x:xs) p | isDigit x = getVal xs (digitToInt x) p
                   | x <= '0' = Nothing --Cannot not insert at row <= 0
                   | otherwise = Nothing

getVal :: [Char] -> Int -> Program -> Maybe [DataVal]
getVal [] row p = Nothing
getVal (x:xs) newCol p | isSpace x = getVal xs newCol p
                       | x == '=' = ignoreQuotes xs newCol p
                       | otherwise = Nothing

ignoreQuotes :: [Char] -> Int -> Program -> Maybe [DataVal]
ignoreQuotes [] _ _ = Nothing -- No value specified
ignoreQuotes (x:xs) newCol p | x == '"' = insertDataVal xs [] newCol p
                             | otherwise = Nothing

insertDataVal :: [Char] -> [Char] -> Int -> Program -> Maybe [DataVal]
insertDataVal [] _ _ _ = Nothing -- No matching '"' was found
insertDataVal (x:xs) partial newCol p
       | x == '"' && isJust rest = Just (first : fromJust rest)
       | x == '"' && isNothing rest = Nothing
       | otherwise = insertDataVal xs (partial ++ [x]) newCol p
        where first = DataVal newCol ((lastRow p)+1) "" partial
              rest =  parseInsert xs p
--END INSERT

--HELP FUNCTION
help :: [Char] -> Program -> (Program, [Char])
--help [] p = (p, "Please supply a function name in order to receive help!")
help "save" p = (p, "This saves the current CSV file. The function requires argument - the filename to be written.")
help "insert" p = (p, "The insert function is a function that will insert a new row into the CSV. You can enter as few as one row. Insert takes the format\ninsert $1=\"This is \" $2=\" a test.\" ")
help "load" p = (p, "This function is used to load in CSV file. The function takes one argument - the filename of the CSV to be read.")
help "help" p = (p, "This function will provide help to the user. It takes one argument - a function name. If you type help or ? by itself you will get a list of all the supported functions.")
help "quit" p = (p, "This function will the quit program. If the CSV file has been edited and not yet saved, the user will be prompted. The option is also available to just type 'q' instead of 'quit'.")
help "distinct" p = (p, "This function will report the distinct values for a column having supplied a column number. An example would be\ndistinct $3 - which will give all the distinct values in column 3.")
help "count" p = (p, "This function takes a condition as an argument and counts the number of values that satisfy the condition(s) for a given column.\nExample - count $1=\"BOC\"")
help "select" p = (p, "This function will make a selection of a certain row/number of rows depending on the condition supplied as an argument. Select is one of the few functions that will work with regular expressions!\nSelect takes the form - select all ~or~ select $1=\"AJAX\"")
help "delete" p = (p, "This function will delete a row in the CSV file. The function takes the form\ndelete $4")
help "list" p = (p, "This function will list a number of rows that satisfy the supplied condition(s). List aswell as select will suport queries with regular expressions.")
help "report" p = (p, "This function, followed by either the string 'completions' or 'registrations' will make useful reports on the CSV file and return them to the user.")
help "reformat" p = (p, "This function takes one of the following arguments ('capitalize', 'uppercase', 'lowercase', 'trim') to the function and carries out the action on a column./nExample reformat $1 lowecase")
help "datefix" p = (p, "This function will convert all dates to the format YYYY-MM-DD. This is the only format that is currently supported.")
help "gridfix" p = (p, "This function will ensure that all grid references are in the correct format\nIE leading alpha follwed by 4/6 digits.")
help "update" p = (p, "This function is used to update any single data value cell of the CSV that is currently selected. NOTE: Select NEEDS to be used beforehand or else there is no data to work with.\nUpdate takes the form - update 5 \"Club\" \"AJAX\". - where 5 is the row number, Club is the column name and AJAX is the new value to be inserted. Update is the only function that currently recognizes column names and numbers.")
help _ p          = (p, "Please supply a valid function name in order to receive help! Here are the list of supported functions. NOTE: They are case sensitive.\n" ++ " • load — load CSV spreadsheet \n" ++ " • save — save spreadsheet as CSV\n" ++ " • report — run builtin report\n" ++ " • count — count records satisfying a condition\n" ++ " • list — show records satisfying a condition\n" ++ " • distinct — report distinct items in a column\n" ++ " • output — redirect output to file\n" ++ " • date-fix — fix date data\n" ++ " • grid-fix — fix grid-reference data\n" ++ " • reformat — reformat column data\n" ++ " • select — select sheet rows\n" ++ " • show — show selected rows\n" ++ " • update — update field\n" ++ " • delete — delete row\n" ++ " • insert — insert new row\n" ++  " • help — help\n" ++ " • quit — exit the program\n" ++ " • print — show all csv entries")
--END HELP

--Retrieves a row by supplying a row number
getRow :: Int -> [DataVal] -> [DataVal]
getRow _ [] = []
getRow row (cur:rest) | row == (y cur) = cur : getRow row rest
                      | otherwise = getRow row rest

--Retrieves a column by supplying a column number
getCol :: Int -> [DataVal] -> [DataVal]
getCol _ [] = []
getCol col (DataVal x y c csv : rest) | col == x = DataVal x y c csv : getCol col rest
                                      | otherwise = getCol col rest

--Counts duplicates
dups :: [String] -> [(String, Int)]
dups xs = (map (\ys -> (head ys, length ys)) . group . sort) xs

tupleToStr :: [(String, Int)] -> [Char]
tupleToStr [] = []
tupleToStr ((str, num):rest) = str ++ ": Frequency of " ++ (show num) ++ "\n" ++ tupleToStr rest
                         
--REPORT
report :: [Char] -> Program -> IO (Program, [Char])
report [] p = return (p, "Empty report query!") --Empty input string
report str p = do date <- curTime
                  if map toLower str == "completions"
                  then return (p, "Report completions result\n" ++ unlines (toString (repCompletions date p (getCol 8 (csv p)) [])) ++ "Try running datefix for a better result if you haven't already!")
                  else
                    if map toLower str == "registrations"
                    then return (p, "Report registrations result\n" ++  tupleToStr tuples)
                    else return  (p, "Sorry. Can only report on 'registrations' and 'completions'. Try again.")
                      where tuples = dups (toString ((getCol 1 (csv p))))

repCompletions :: [Char] -> Program -> [DataVal] -> [DataVal] -> [DataVal]
repCompletions _ _ [] result = result
repCompletions cur_date p (DataVal x y c date : rest) result
         | checkDates date cur_date = repCompletions cur_date p rest (fromJust (getEntry (csv p) 2 y) : result)
         | otherwise = repCompletions cur_date p rest result

checkDates :: [Char] -> [Char] -> Bool
checkDates [] _ = False
checkDates str@(x:xs) date | checkWhite str = False
                           | stripWhite str <= stripWhite date = True
                           | checkNum str && checkNum date = stripWhite str <= stripWhite date
                           | (stripWhite str) == "completed" = False
                           | otherwise = False

--Checks that supplied string in an integer
checkNum :: [Char] -> Bool
checkNum [] = True
checkNum (x:xs) | isDigit x = checkNum xs
                | otherwise = False

--Checks that supplied string is all alpha characters
checkAlpha :: [Char] -> Bool
checkAlpha [] = True
checkAlpha (x:xs) | isAlpha x = checkAlpha xs
                  | otherwise = False

--Checks that supplied string in whitespace
checkWhite :: [Char] -> Bool
checkWhite [] = True
checkWhite (x:xs) | isSpace x = checkWhite xs
                  | otherwise = False
--Retrieves a single cell in the CSV by supplying row and column numbers
getEntry ::  [DataVal] -> Int -> Int -> Maybe DataVal
getEntry [] _ _ = Nothing
getEntry (cur : rest) col row
    | col == (x cur) && row == (y cur) = Just cur
    | otherwise = getEntry rest col row

--DISTINCT (single column)
distinct :: [Char] -> Program -> (Program, [Char])
distinct [] p = (p, "Empty distinct query!")
distinct ('$':xs) p | isDigit (head xs) = (p, show (countDistinct (toString (getCol (digitToInt (head xs)) (csv p)))) ++ " distinct values found.")
                    | otherwise = (p, "Invalid distinct query. Please try again.")

--Takes a list of dataVals and puts their cell values into a list of strings
toString :: [DataVal] -> [String]
toString [] = []
toString (DataVal x y col val : rest) = [val] ++ toString rest 

--Counts distinct values of a list of strings
countDistinct :: [String] -> Int
countDistinct [] = 0
countDistinct (x:rest) = 1 + (countDistinct (filter (/= x) rest))

listY :: [DataVal] -> [Int]
listY [] = []
listY (DataVal x y col val : rest) = y : listY rest

dist :: [Int] -> Int
dist [] = 0
dist (x:xs) = 1 + dist (filter (/= x) xs)

--COUNT -- Maybe i should just use [char] as the return type here and show the int and ++ it on ?
count :: [Char] -> Program -> (Program, [Char])
count [] p = (p , "No condition for count was specified.")
count query p | isJust result = (p, show (dist (listY (fromJust result))) ++ " values found for " ++ query ++ ".")          
              | otherwise = (p, "0 rows returned for count.")
               where result = parseCond (stripWhite query) (csv p)

--Counts the amount of DataVals
sum' :: [DataVal] -> Int
sum' [] = 0
sum' all@(DataVal x y c val : rest) = 1 + sum' rest

--SELECT
select :: [Char] -> Program -> (Program, [Char])
select [] p = (p, "Empty select query!")
select "all" p = (Program (csv p) (csv p) (state p) (file p), "All CSV file selected.")
select query p | isJust result = (Program (csv p) (fromJust result) (state p) (file p), "Select successful and returned " ++ show (length (group (sort (countRows(fromJust result))))) ++ " rows.")
               | otherwise = (p, "Sorry. Select returned nothing!")
                 where result = parseCond (stripWhite query) (csv p)

--Counts the all the row numbers in a list of dataVals
countRows :: [DataVal] -> [Int]
countRows [] = []
countRows (cur:rest) = (y cur) : countRows rest
                      
--LIST
list :: [Char] -> Program -> (Program, [Char])
list [] p = (p, "Empty list query!")
list query p | isJust result = (p, "List result: \n" ++ unlines (toString (fromJust result)))          
             | otherwise = (p, "Sorry. List returned nothing!")
               where result = parseCond (stripWhite query) (csv p)

parseCond :: [Char] -> [DataVal] -> Maybe [DataVal]
parseCond [] p = Just p
parseCond (x:xs) p | x == '$' = condColNum xs p
                   | otherwise = Nothing

condColNum :: [Char] -> [DataVal] -> Maybe [DataVal]
condColNum [] _ = Nothing
condColNum (x:xs) p | isSpace x = condColNum xs p
                    | isDigit x = getCond xs p (digitToInt x)
                    | otherwise = Nothing

getCond :: [Char] -> [DataVal] -> Int -> Maybe [DataVal]
getCond [] _ _ = Nothing
getCond (x:xs) p col | x == '=' = ignoreQuotes' xs p col
                     | otherwise = Nothing

ignoreQuotes' :: [Char] -> [DataVal] -> Int -> Maybe [DataVal]
ignoreQuotes' [] _ _ = Nothing
ignoreQuotes' (x:xs) p col | x == '"' = extractCondVal xs [] p col
                           | otherwise = Nothing

extractCondVal :: [Char] -> [Char] -> [DataVal] -> Int -> Maybe [DataVal]
extractCondVal [] _ _ _ = Just []
extractCondVal (x:xs) partial p col | x == '"' && isJust result = parseCond xs (fromJust result)
                                    | x == '"' && isNothing result = Nothing
                                    | otherwise = extractCondVal xs (partial ++ [x]) p col
                                      where result = searchOnCond partial p col

searchOnCond :: [Char] -> [DataVal] -> Int -> Maybe [DataVal]
searchOnCond [] _ _ = Nothing
searchOnCond term csv@(DataVal x y c val : rest) col = searchOnCol term (getCol col csv) csv

searchOnCol :: [Char] -> [DataVal] -> [DataVal] -> Maybe [DataVal]
searchOnCol [] _ _ = Nothing
searchOnCol _ [] _ = Just []
searchOnCol term col@(cur : rest) csv
       | term == (val cur) || isJust regex = Just ((getRow (y cur) csv) ++ fromJust (searchOnCol term rest csv))
  | otherwise = searchOnCol term rest csv
        where regex = (matchRegex (mkRegex ("("++ term ++")")) (val cur))
--END SELECT & LIST

--UPDATE
update :: [(Int, [Char])] -> [Char] -> Program -> (Program, [Char])
update _ [] p = (p, "Empty update statement!")
update cl _ p@(Program csv [] state filename) = (p, "Sorry. Nothing is currently selected. Try running select first!") --Nothing currently selected
update cl str@(x:xs) p@(Program csv cur state filename)
    | isDigit x && rowExist (cur) (digitToInt x) = (Program (getColByName xs p cl (digitToInt x)) [] False filename, "Update successful.")
    | otherwise = (p, "Update was unsuccessful due to bad query. Nothing was changed.")

getColByName :: [Char] -> Program -> [(Int, [Char])] -> Int -> [DataVal]
getColByName [] p _ _ = (csv p) --ran out of input
getColByName str@(x:xs) p cl row
    | isSpace x = getColByName xs p cl row
    | x == '$' && isDigit (head xs) = matchCol' xs p cl (digitToInt (head xs)) row
    | x == '"' = extractCol xs p [] cl row
    | isDigit x = matchCol' xs p cl (digitToInt x) row
    | otherwise = (csv p)

extractCol :: [Char] -> Program -> [Char] -> [(Int, [Char])] -> Int -> [DataVal]
extractCol [] p _ _ _ = (csv p) --ran out of input (no column specified)
extractCol str@(x:xs) p partial cl row | x == '"' = matchCol xs p cl partial row
                                       | otherwise = extractCol xs p (partial ++ [x]) cl row

--This function is called if a column name is specified in query
matchCol :: [Char] -> Program -> [(Int, [Char])] -> [Char] -> Int -> [DataVal]
matchCol [] p _ _ _ = (csv p) --No update value specified
matchCol _ p _ [] _ = (csv p) --No column name specified (empty)
matchCol _ p [] _ _ = (csv p)--Column name list empty
matchCol str@(x:xs) p ((colNum,colList):rest) colName row
    | colName == colList = extractVal str p row colNum
    | otherwise = matchCol str p rest colName row

--This function is called if a column number (rather than name) is specified in quer
matchCol' :: [Char] -> Program -> [(Int, [Char])] -> Int -> Int -> [DataVal]
matchCol' [] p _ _ _ = (csv p) --No update value specified
matchCol' _ p [] _ _ = (csv p)--Column name list empty
matchCol' str@(x:xs) p ((colNum,colList):rest) col row
    | col == colNum = extractVal str p row colNum
    | otherwise = matchCol' str p rest col row

extractVal :: [Char] -> Program -> Int -> Int -> [DataVal]
extractVal [] p _ _ =  (csv p)
extractVal str@(x:xs) p row col | isSpace x = extractVal xs p row col
                                | x == '"' = getUpdateVal xs p [] row col
                                | otherwise = extractVal xs p row col

getUpdateVal :: [Char] -> Program -> [Char] -> Int -> Int -> [DataVal]
getUpdateVal str@(x:xs) p partial row col | x == '"' = search partial p row col
                                          | otherwise = getUpdateVal xs p (partial++[x]) row col

search :: [Char] -> Program -> Int -> Int -> [DataVal]
--wont need to catch for empty because user might want to clear value
search str p@(Program csv (curSel:rest) s f) row c
    | (x curSel) == row && row == 0 = [] -- Shouldn't error out here
    | (x curSel) == row = replaceRow  (DataVal c row (col curSel) str) csv     
    | otherwise = search str (Program csv (rest) s f) row c

replaceRow :: DataVal -> [DataVal] -> [DataVal]
replaceRow _ [] = []
replaceRow new (csv:rest)
    | (x new) == (x csv) && (y new) == (y csv) = [new] ++ replaceRow new rest
    | otherwise = [csv] ++ replaceRow new rest

rowExist :: [DataVal] -> Int -> Bool
rowExist [] _ = False
rowExist (cur:rest) row | (y cur) == row = True
                        | otherwise = rowExist rest row
--END UPDATE

--Function for editing column header values
changeHeader :: Int -> [Char] -> [(Int, [Char])] -> [(Int, [Char])]
changeHeader col str [] = []
changeHeader col str cl@((colNum,colList):rest) 
    | col == colNum = [(colNum, str)] ++ changeHeader col str rest
    | otherwise = changeHeader col str cl

--DELETE
delete' :: [Char] -> Program -> (Program, [Char])
delete' [] p = (p, "Empty delete query")
delete' row@('$':x:xs) p
    | isDigit x && null xs = (Program (deleteRow (digitToInt x) (csv p)) (cur p) False (file p), "Delete successful!")
    | otherwise = (p, "Delete was unsuccessful. Invalid query. Format is\ndelete $row")

deleteRow :: Int -> [DataVal] -> [DataVal]
deleteRow 0 csv = csv --Cannot delete row 0
deleteRow _ [] = []
deleteRow row (cur : rest)
     | (y cur) == row = deleteRow row rest
     | otherwise = cur : deleteRow row rest
--END DELETE

--REFORMAT
reformat :: [Char] -> Program -> (Program, [Char])
reformat "" p = (p, "Empty reformat query!")
reformat str@(x:xs) p = (Program (checkCol (stripWhite str) p) (cur p) False (file p), "Reformat successful.")

checkCol :: [Char] -> Program -> [DataVal]
checkCol "" p = (csv p)
checkCol str@('$':x:xs) p | isDigit x = reformatInstr xs (digitToInt x) p
                          | otherwise = csv p

reformatInstr :: [Char] -> Int -> Program -> [DataVal]
reformatInstr "" _ p = (csv p)
reformatInstr action col p =
  case (stripWhite action) of
    "uppercase"  -> upper (csv p) col
    "capitalize" -> capital (csv p) col
    "lowercase"  -> lower (csv p) col
    "trim"       -> trim (csv p) col
    _            -> (csv p)

--Convert cell value to uppercase
upper :: [DataVal] -> Int -> [DataVal]
upper [] _ = []
upper (DataVal x y c val : rest) col
    | x == col = (DataVal x y c (map toUpper val)) : upper rest col
    | otherwise = (DataVal x y c val) : upper rest col

--Convert cell value to lowercase
lower :: [DataVal] -> Int -> [DataVal]
lower [] _ = []
lower (DataVal x y c val : rest) col
    | x == col = (DataVal x y c (map toLower val)) : lower rest col
    | otherwise = (DataVal x y c val) : lower rest col

--Capitalize beginning of each word cell value
capital :: [DataVal] -> Int -> [DataVal]
capital [] _ = []
capital (DataVal x y c val : rest) col
     | x == col && not (null val) = (DataVal x y c (capitalize (words val))) : capital rest col
     | otherwise = (DataVal x y c val) : capital rest col

--Trim whitespace in each cell value
trim :: [DataVal] -> Int -> [DataVal]
trim [] _ = []
trim (DataVal x y c val : rest) col
    | x == col = (DataVal x y c (stripWhite val)) : trim rest col
    | otherwise = (DataVal x y c val) : trim rest col

--Function used to capitalize beginning of each word
capitalize :: [String] -> [Char]
capitalize [] = []
capitalize str@(x:xs) = (toUpper (head x)) : tail x ++ " " ++ capitalize xs
