

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


labelTree :: BinTree a -> BinTree (a, Int)
labelTree tree = fst (labelTreeAux tree 1)

labelTreeAux :: BinTree a -> Int -> (BinTree (a, Int), Int)
labelTreeAux Empty c0 = (Empty, c0)
labelTreeAux (Bin l x r) c0 = 
    let 
        (newl, c1) = labelTreeAux l c0 
        -- c2 = c1 + 1 
        (newr, c2) = labelTreeAux r (c1 + 1) 
    in 
        (Bin newl (x, c1) newr, c2)




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


labelTree2 :: BinTree a -> BinTree (a, Int)
labelTree2 tree = fst (labelTreeAux2 tree 1)

labelTreeAux2 :: BinTree a -> Int -> (BinTree (a, Int), Int)
labelTreeAux2 Empty c0 = (Empty, c0)
labelTreeAux2 (Bin l x r) c0 = 
    let 
        (newl, c1) = labelTreeAux2 l c0 
        c2 = c1 + 1 
        (newr, c3) = labelTreeAux2 r c2 
    in 
        (Bin newl (x, c1) newr, c3)


