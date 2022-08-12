module Chapter3

import Data.Vect


myLength : List a -> Nat
myLength [] = 0
myLength (_ :: xs) = 1 + myLength xs


-- λΠ> :total myLength
-- Chapter3.myLength is Total


myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]


-- λΠ> :total myReverse
-- Chapter3.myReverse is Total


myMapList : (a -> b) -> List a -> List b
myMapList f [] = []
myMapList f (x :: xs) = f x :: myMapList f xs


-- λΠ> :total myMapList
-- Chapter3.myMapList is Total


myMapVector : (a -> b) -> Vect n a -> Vect n b
myMapVector f [] = []
myMapVector f (x :: xs) = f x :: myMapVector f xs


-- λΠ> :total myMapVector
-- Chapter3.myMapVector is Total


transposeMat_ : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat_ [] = replicate _ []
transposeMat_ (x :: xs) = let xsTrans = transposeMat_ xs in
                                      zipWith (::) x xsTrans

-- λΠ> :total transposeMat_
-- Chapter3.transposeMat_ is Total


addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let added = Data.Vect.zipWith (+) x y in
                                    added :: addMatrix xs ys


-- λΠ> :total addMatrix
-- Chapter3.addMatrix is Total


