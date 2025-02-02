

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



labelTreeAux2 :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux2 Empty = MkCounter (\ c0 -> (Empty, c0))
labelTreeAux2 (Bin l x r) = 
    labelTreeAux2 l >>>>= \ newl ->  MkCounter ( \ c1 ->
    let 
        (lx , c2) = runCounter stepCounter c1 
        (newr , c3) = runCounter (labelTreeAux r) c2 
    in 
        (Bin newl (x, lx) newr, c3))


labelTreeAux3 :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux3 Empty = MkCounter (\ c0 -> (Empty, c0))
labelTreeAux3 (Bin l x r) = 
    labelTreeAux3 l >>>>= \ newl ->  
    stepCounter >>>>= \ lx -> 
    labelTreeAux3 r >>>>= \ newr -> 
    MkCounter (\ c3 -> (Bin newl (x, lx) newr, c3))



labelTreeAux4 :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux4 Empty = returnCounter Empty 
labelTreeAux4 (Bin l x r) = 
    labelTreeAux4 l >>>>= \ newl ->  
    stepCounter >>>>= \ lx -> 
    labelTreeAux4 r >>>>= \ newr -> 
    returnCounter (Bin newl (x, lx) newr)

newtype Counter a = MkCounter { runCounter :: Int -> (a , Int) }


-- stepCounter :: Counter Int 
-- stepCounter = MkCounter (\ c -> (c , c + 1)) 




(>>>>=) :: Counter a -> (a -> Counter b ) -> Counter b 
comp >>>>= rest = 
    MkCounter ( \ c0 ->
        let
            (a , c1) = runCounter comp c0 
        in 
            runCounter (rest a) c1 
    
    )


returnCounter :: a -> Counter a 
returnCounter a = MkCounter (\c -> (a , c)) 


stepCounter :: Counter Int
stepCounter = MkCounter (\c -> (c , c+1)) 


