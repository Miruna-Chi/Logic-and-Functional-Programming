module Query where

import UserInfo
import Rating
import Movie
import Data.List.Split
import Data.List
import Numeric

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table cs ls table = Table (head string_table) (remEmptyLn (tail string_table))
    where string_table = make_matrix cs ls table

-- make the matrix necessary for the table: split on the colSeparator and lnSeparator:
-- head = TableSchema, tail = [Entry] 
make_matrix :: ColSeparator -> LnSeparator -> String -> [[String]]
make_matrix cs ls table = map (splitOn (cs:[])) $ splitOn (ls:[]) table

-- there will be a new line at the end of the file -> last entry will be a [] -> remove it
remEmptyLn :: [Entry] -> [Entry]
remEmptyLn entries = fst $ splitAt ((length entries) - 1) entries

movie = read_table '|' '\n' movie_str
rating = read_table ' ' '\n' rating_str
user_info = read_table '|' '\n' user_info_str

-- TODO 2
instance Show Table where
    show (Table header entries) = show_table (Table header entries)

show_table :: Table-> String
show_table (Table header entries) = prSmth ++ "\n" ++ prHdr ++ prSmth 
      ++ "\n" ++ prEntr ++ prSmth ++ "\n"
            where total_len = (sum all_maxl + (length header) + 1)
                  all_maxl = allMaxLens (Table header entries)
                  prSmth = printSmth '-' total_len
                  prHdr = printHeader header all_maxl
                  prEntr = printEntries entries all_maxl

-- makes a String of n * (whatever character needs to be printed)
printSmth :: Char -> Int -> String
printSmth c 0 = ""
printSmth c n = c : printSmth c (n - 1)


{-    
      printHeader and printEntry:
      - prints the header/line in the given format
      - takes each header column/field of an entry and the length of the specific column
      to know how many spaces to print after each
-}

printHeader :: TableSchema -> [Int] -> String
printHeader [] _ = "|\n"
printHeader (h:header) (len:all_maxl) = "|" ++ h ++ prSmth ++ printHeader header all_maxl
      where prSmth = printSmth ' ' (len - length h)

printEntry :: Entry -> [Int] -> String
printEntry [] _ = "|\n"
printEntry (f:field) (len:all_maxl) = "|" ++ f ++ prSmth ++ printEntry field all_maxl
      where prSmth = printSmth ' ' (len - length f)

printEntries :: [Entry] -> [Int] -> String
printEntries [] _ = ""
printEntries (e:entries) all_maxl = printEntry e all_maxl ++ printEntries entries all_maxl

-- make a list of all the max lengths on each column
entryColMaxLen :: [Entry] -> [Int]
entryColMaxLen ([]:_) = []
entryColMaxLen entry = (maximum $ 
      (map length (map head entry))) : (entryColMaxLen (map tail entry))

-- make a list of all the header column lengths 
tsColMaxLen :: TableSchema -> [Int]
tsColMaxLen ts = map length ts

-- choose the max between the header column length and max length on that column
-- make the final max lengths list
maxTsEntry :: [Int] -> [Int] -> [Int]
maxTsEntry [] [] = []
maxTsEntry (x:xs) (y:ys) = maximum [x, y] : maxTsEntry xs ys

-- call the previous function with the right parameters
allMaxLens :: Table -> [Int]
allMaxLens (Table header entries) = maxTsEntry (tsColMaxLen header) $ entryColMaxLen entries

-- add all the max lengths of each column -> total length of a line
allColLen :: Table -> Int
allColLen table = sum $ allMaxLens table

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- TODO 3

{-
      chooseEntries (function needed for filter):
      - will make a list of booleans with as many elements as there are entries
      to remember which entries will be chosen for filtering
      - take all the cases, call a function for each
-}
chooseEntries :: FilterCondition -> Table -> [Bool]
chooseEntries (Lt field num) table = getTrueLt num (findCol field table)
chooseEntries (Eq field value) table = getTrueEqIn (value:[]) (findCol field table)
chooseEntries (In field val_list) table = getTrueEqIn val_list (findCol field table)
chooseEntries (Not filter_cond) table = inverseBool (chooseEntries filter_cond table)

getTrueLt :: Integer -> [Entry] -> [Bool]
getTrueLt _ [[]] = []
getTrueLt num [e:col]
      | (read e::Integer) < num = True : (getTrueLt num [col])
      | otherwise = False : (getTrueLt num [col])

getTrueEqIn :: [String] -> [Entry] -> [Bool]
getTrueEqIn _ [[]] = []
getTrueEqIn val_list [e:col]
      | findVal e val_list  = True : (getTrueEqIn val_list [col])
      | otherwise = False : (getTrueEqIn val_list [col])

findVal :: String -> [String] -> Bool
findVal _ [] = False
findVal entry (val:val_list)
      | entry == val = True
      | otherwise = findVal entry val_list

-- for Not: inverse the boolean list so the "False" entries will actually be the chosen ones
inverseBool :: [Bool] -> [Bool]
inverseBool list = map (\x -> x /= True) list

-- pick the entries which have been paired with a "True"
filteredEntries :: [Entry] -> [Bool] -> [Entry]
filteredEntries [] [] = []
filteredEntries (e:entries) (b:chosen)
      | b == True = e : filteredEntries entries chosen
      | otherwise = filteredEntries entries chosen

-- TODO 4
data Query = Filter FilterCondition Query |
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom table) = table
eval (Select col_list query) = Table col_list $ select col_list (eval query)

eval (SelectLimit col_list no_of_entries query) = Table col_list $ selected_entries
      where selected_entries = select col_list (cutTable (eval query) no_of_entries)

eval (Filter (Lt field num) query) = filterTableLt (eval query) field num

eval (Filter (Eq field value) query) = filterTableEq (eval query) field value

eval (Filter (In field val_list) query) = filterTableIn (eval query) field val_list

eval (Filter (Not filter_cond) query) = filterTableNot (eval query) filter_cond

eval (query_left :|| query_right) = unite (eval query_left) (eval query_right)

eval (Cosine query) = Table (["user_id1", "user_id2", "sim"]) (makeSimEntries u_list1 u_list2
                                    (goThroughUsers u_list1 u_list2 0 1 mr_lists norm_list))
      where u_list1 = makeSortedIDlist $ gatherIDs ratings
            u_list2 = tail u_list1
            ratings = eval query
            mr_lists = sortByMovieID (makeMRLists u_list1 ratings)
            norm_list = allSqSums mr_lists


-- call all the needed functions for Select, glue the found columns together in the given order
select :: [String] -> Table -> [Entry]
select [] (Table header entries) = listOfEmptyEntries entries
select (col:col_list) (Table header entries) = zipWith (++) nextColToGlue 
                                                      (select col_list (Table header entries))
      where nextColToGlue = sepEntries (findCol col (Table header entries))

-- finds a specific column in a table by its heading, returns the column (without the heading)
findCol :: String -> Table -> [Entry]
findCol col (Table (h:header) entries)
      | h == col = (map head entries) : []
      | otherwise = findCol col (Table header (map tail entries))

{-
      - findCol returns the column as a list within a list: [["entry1_field", "entry2_field" ...]]
      when it should be: [["entry1_field"], ["entry2_field"] ...], so that it can represent
      a column and be glued to other columns. sepEntries transforms the result of findCol this way
-}
sepEntries :: [Entry] -> [Entry]
sepEntries [[]] = []
sepEntries [e:entries] = (e : []) : sepEntries [entries]

-- the equivalent of [] for a column: the void column which is the base for gluing 
-- another colums to it
listOfEmptyEntries :: [Entry] -> [Entry]
listOfEmptyEntries [] = []
listOfEmptyEntries (e:entries) = [] : listOfEmptyEntries entries  

-- select the first no_of_entries entries
newlyCutEntries :: [Entry] -> Integer -> [Entry]
newlyCutEntries _ 0 = []
newlyCutEntries (e:entries) no_of_entries = e : (newlyCutEntries entries (no_of_entries - 1))      

-- make the table with the selected no_of_entries entries
cutTable :: Table -> Integer -> Table
cutTable (Table header entries) no_of_entries = Table header 
                                          $ newlyCutEntries entries no_of_entries

filterTableLt :: Table -> Field -> Integer -> Table
filterTableLt (Table header entries) field num = Table header (filteredEntries entries $
      chooseEntries (Lt field num) (Table header entries))

filterTableEq :: Table -> Field -> String -> Table
filterTableEq (Table header entries) field value = Table header (filteredEntries entries $
      chooseEntries (Eq field value) (Table header entries))

filterTableIn :: Table -> Field -> [String] -> Table
filterTableIn (Table header entries) field val_list = Table header (filteredEntries entries $
      chooseEntries (In field val_list) (Table header entries))

filterTableNot :: Table -> FilterCondition -> Table
filterTableNot (Table header entries) filter_cond = Table header (filteredEntries entries $
      chooseEntries (Not filter_cond) (Table header entries))


-- make one table with the entries for two queries (:||)
unite :: Table -> Table -> Table
unite (Table header1 entries1) (Table header2 entries2) = Table header1 $ combine entries1 entries2

-- combine the entries of two queries (:||)
combine :: [Entry] -> [Entry] -> [Entry]
combine [] [] = []
combine [] entries2 = entries2
combine entires1 [] = entires1
combine entries1 entries2 = entries1 ++ entries2



-- TODO 5
same_zone :: String -> Query
same_zone userid = Select ["user_id", "occupation"]
                  $ Filter (Not (Eq "user_id" userid))
                  $ Filter (Eq "zone" (getEntry (eval $ Select ["zone"] 
                        $ Filter (Eq "user_id" userid) $ Atom user_info)))
                  $ Atom user_info

getEntry :: Table -> String
getEntry (Table header [[entries]]) = entries

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Select ["occupation", "zone"]
                        $ Filter (Not (Eq "age" (show x)))
                        $ Filter (Not (Lt "age" x))
                        $ Filter (Lt "age" y)
                        $ Filter (Eq "sex" "M")
                        $ Atom user_info

mixed :: [String] -> [String] -> Integer -> Query
mixed zone_list occupation_list threshold = Select ["user_id"]
                                                $ Filter (In "zone" zone_list)
                                                $ Filter (In "occupation" occupation_list)
                                                $ Filter (Lt "age" threshold)
                                                $ Atom user_info


--Cosine


-- get the column of IDs
gatherIDs :: Table -> [Entry]
gatherIDs table = findCol "user_id" table

-- make a sorted list out of them (with no duplicates); use insertion sort
makeSortedIDlist :: [Entry] -> [String]
makeSortedIDlist [[]] = []
makeSortedIDlist [e:entries] = ins e (makeSortedIDlist [entries]) 

ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) | x < y     = x:y:ys
                | x == y    = y:ys
                | otherwise = y:(ins x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)

-- make a table only with the "movie_id" and "rating" columns for a user from the user_list
userMRTable :: Table -> String -> Table
userMRTable table userid = eval 
            $ Select ["movie_id", "rating"] 
            $ Filter (Eq "user_id" userid) 
            $ Atom table

-- keep only the entries for the above table
userMRList :: Table -> [Entry]
userMRList (Table header entries) = entries

--sort each user's list of movie-rating "pairs" by movies, in ascending order
sortByMovieID :: [[Entry]] -> [[Entry]]
sortByMovieID (m:mr_list) = (sortBy ascendingMR m) : sortByMovieID mr_list

ascendingMR (a1:b1) (a2:b2)
  | a1 < a2 = LT
  | a1 >= a2 = GT

{-
      makeMRLists:
      - call the above functions for each user. make a list of those lists (1 list for 1 user)
      - the order will match the user order in the list, so each n-th entry in the lists will mean
      the same user (user_id, list of movies and ratings)
-}
makeMRLists :: [String] -> Table -> [[Entry]]
makeMRLists [] _ = []
makeMRLists (u:user_ids) ratings = (userMRList (userMRTable ratings u)) 
                                    : makeMRLists user_ids ratings 

{-
      calcProdSum:
      - take two users' (sorted) lists of movies/ratings
      - if the movie IDs match -> they rated the same movie -> 
            add the product of their ratings to the sum
      - if they don't match -> if (m1 < m2) -> we need to keep the same second list
                                    and advance in the first list
                            -> if (m2 > m1) -> we need to keep the same first list
                                    and advance in the second list
      - recursive call :)
-}
calcProdSum :: [Entry] -> [Entry] -> Float
calcProdSum [] _ = 0
calcProdSum _ [] = 0
calcProdSum ((m1:r1:nothing1):ratings1) ((m2:r2:nothing2):ratings2)
      | m1 == m2 = (read r1::Float) * (read r2::Float) + calcProdSum ratings1 ratings2
      | m1 < m2 = calcProdSum ratings1 ((m2:r2:nothing2):ratings2)
      | otherwise = calcProdSum ((m1:r1:nothing1):ratings1) ratings2


-- take a user's list of movies/ratings, make the sum of each rating squared
calcSqSum :: [Entry] -> Float
calcSqSum [] = 0
calcSqSum ((m:r:nothing):m_ratings) = (read r::Float)^^2 + calcSqSum m_ratings

allSqSums :: [[Entry]] -> [Float]
allSqSums [] = []
allSqSums (mr : mr_lists) = calcSqSum mr : (allSqSums mr_lists)

-- calculate cosine
calcCos :: Float -> Float -> Float -> Float
calcCos sum norm1 norm2 = sum / ((sqrt norm1) * (sqrt norm2))

-- call the above functions to calculate the similarity between two users
calcSim :: [[Entry]] -> [Float] -> Int -> Int -> Float
calcSim mr_lists norm_list ind1 ind2 = calcCos sum norm1 norm2
      where sum = calcProdSum u1_mr_list u2_mr_list
            norm1 = norm_list !! ind1
            norm2 = norm_list !! ind2
            u1_mr_list = mr_lists !! ind1
            u2_mr_list = mr_lists !! ind2

{-
      goThroughUsers:
      - go through all pairs of users in lexigoraphical order, don't take the same pair twice
      - keep indexes so we don't have to find a user entry in the lists we created (we can
      access the entry by its index using "!!")
      - call calcSim for each pair
-}
goThroughUsers :: [String] -> [String] -> Int -> Int -> [[Entry]] -> [Float] -> [Float]
goThroughUsers (u:[]) _ _ _ _ _ = []
goThroughUsers (done:u1:user) []  ind1 ind2 mr_lists norm_list = goThroughUsers (u1:user) user 
                                                      (ind1 + 1) (ind1 + 2) mr_lists norm_list
goThroughUsers (u1:user1) (u2:user2) ind1 ind2 mr_lists norm_list = (calcSim mr_lists norm_list 
      ind1 ind2) : (goThroughUsers (u1:user1) user2 ind1 (ind2 + 1)) mr_lists norm_list

-- we want this format (with at most 4 digits after the decimal point) for each similarity
cosField :: Float -> String
cosField c = showFFloat (Just 4) c ""

-- finally, print the entries :)
makeSimEntries :: [String] -> [String] -> [Float] -> [Entry]
makeSimEntries (u:[]) _ _ = []
makeSimEntries (done:u1:user) [] cos = makeSimEntries (u1:user) user cos
makeSimEntries (u1:user1) (u2:user2) (c:cos) = [u1, u2, cosField c] 
                                    : (makeSimEntries (u1:user1) user2 cos)

