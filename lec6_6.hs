
module Monads where 

import Data.List (map) 

import Control.Applicative
import Data.Map.Strict as M 

import Prelude hiding (mapM, sequence)


data BinTree a = 
    Bin (BinTree a) a (BinTree a)
    | Empty 
    deriving (Eq , Show) 


example :: BinTree Char 
example =
    Bin
        (Bin
            (Bin Empty 'q' Empty)
            'a'
            Empty
        )
        'x'
        (Bin
            (Bin Empty 'h' Empty)
            'b'
            (Bin Empty 'p' Empty)
        )




tree5 :: BinTree Char
tree5 = Bin (Bin Empty 'A' Empty) 'B' (Bin Empty 'C' Empty)

tree6 :: BinTree Char
tree6 = Bin 
            (Bin 
                (Bin Empty 'A' Empty) 
                'B' 
                (Bin Empty 'C' Empty)
            ) 
            'D' 
            (Bin 
                (Bin Empty 'E' Empty) 
                'F' 
                (Bin 
                    (Bin Empty 'G' Empty) 
                    'H' 
                    (Bin Empty 'I' Empty)
                )
            )



labelTree :: BinTree a -> BinTree (a, Int)
labelTree tree = 
    fst (runCounter (labelTreeAux tree) 1) 

labelTreeAux :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux Empty = MkCounter (\ c0 -> (Empty, c0))
labelTreeAux (Bin l x r) = MkCounter (\ c0 -> 
    let 
        (newl , c1) = runCounter (labelTreeAux l) c0 
        (lx , c2) = runCounter stepCounter c1 
        (newr , c3) = runCounter (labelTreeAux r) c2 
    in 
        (Bin newl (x, lx) newr, c3))



newtype Counter a = MkCounter { runCounter :: Int -> (a , Int) }


stepCounter :: Counter Int 
stepCounter = MkCounter (\ c -> (c , c + 1)) 


-- ghci> labelTree tree5
-- Bin (Bin Empty ('A',1) Empty) ('B',2) (Bin Empty ('C',3) Empty)