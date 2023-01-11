
import Control.Arrow        -- for second function 
import Data.List            --for insert function 
import qualified Data.Map as M      -- for building a codemap 
import Data.Function          -- for compare on function 
import System.IO       
import Data.Char             -- for digit to int function


 -- define an alias name for code map
type Codemap a = M.Map Char [a]           -- maping elements in the code map by the characters 

data HTree =  Leaf Char Int
           | Fork HTree HTree Int
           deriving (Show)            -- for enabling display of the data HTree 

removedup :: [Char] -> [Char]          -- remove the duplicate letters 
removedup (x:[]) = []
removedup (x:y:xs) = if (x == y)
                       then removedup (x:xs)
                       else y:removedup (x:xs)     

freqlist :: [Char]-> [(Char,Int)]               -- making a list with tuples of letter and the frequency of it       
freqlist [] = [] 
freqlist (x:xs) = (x,length (filter (==x) (x:xs))) : freqlist(removedup (x:xs))



weight :: HTree -> Int           -- return the weight of the leaf or the fork 
weight (Leaf _ w)    = w
weight (Fork _ _ w)  = w


merge :: HTree -> HTree -> HTree        -- mergeing the leafs and the forks to make a tree 
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)



buildleafs :: [(Char,Int)] -> HTree         -- sort the tuples then create a list of leafs and pass it to build function to build a tree 
buildleafs(x:y:xs) =  bld [Leaf (fst z) (snd z) | z <-   (sortBy (\x y ->  compare (snd x) (snd y )) (x:y:xs)) ]


bld :: [HTree] -> HTree        -- build a tree after comparing the weight of the nodes 
bld (t:[])    = t
bld (a:b:cs)  = bld $ insertBy (compare `on` weight) (merge a b) cs


-- adding 0 to the tothe elements at the left and 1 to the elements of the right and build code map to ease the traverse 
buildCodemap ::  Num a => HTree -> Codemap a      
buildCodemap = M.fromList . buildCodelist 
     where  buildCodelist (Leaf c w)    = [(c, [])]
            buildCodelist (Fork l r w)  = map (addBit 0) (buildCodelist l) ++ map (addBit 1) (buildCodelist r)
              where addBit b = second (b :)

-- simple function calls buildleafs and freqlist to build the tree 
buildTree :: String -> HTree
buildTree xs = buildleafs (freqlist xs )
-- calling the functions neccessary to build codemap 
stringCodemap ::  Num a => String -> Codemap a
stringCodemap xs = buildCodemap(buildTree xs)  

-- encoding the string by mapping the string with values in the list that represents the path to it 
encode' :: Num a => Codemap a -> String -> [a]
encode' m string = concat (map (m M.!) string)

-- converts the list of integers to string to put it in the file 
encode ::  String -> String
encode xs = intListToString (encode' (stringCodemap xs ) xs )


-- traversing the tree using integers to decode the string 
decode ::  HTree -> [Int] -> String
decode tree bs = traverse tree bs
     where  traverse (Leaf char _) []        = [char]
            traverse (Leaf char _) bs        = char : traverse tree bs
            traverse (Fork l r _) (b:bs)  = traverse (if (b == 0) then l else r) bs

-- converts a list of integers to string to put it in the file 
intListToString :: [Int] -> String
intListToString ints = concat $ map show ints

main :: IO ()
main = do
     putStrLn "started encoding the data inside the input file..."
     contents <- readFile "input.txt" -- read the content of the file 
         -- encode the input
     let encoded = encode contents
     
      -- write the encoded output to a file
     writeFile "output.txt" encoded
     let tree = buildTree contents           -- building a tree to use it for decoding 
     putStrLn ("A Tree has been built for the data")
     putStrLn ("the data was encoded successfully now you can find it in the output file.. ")
     putStrLn "Do you want to decode data using the previous tree  ?  Y/N "
     choice <- getLine     -- getting the choice whether to decode the data or not 
     if  choice  == "Y"  then  do 
         arr <- readFile "output.txt"          -- read the data in the output file 
         let array = map digitToInt arr      -- convert the string into list of integers 
         let decoded = decode tree array         -- decode the data 
         writeFile "output2.txt" decoded    -- print it in the file 
         putStrLn ("the data has been decoded successfully ")
     else  putStrLn "terminated "

     

          
          
        

 

