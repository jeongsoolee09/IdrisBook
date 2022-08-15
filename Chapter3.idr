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


transposeMat : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

-- λΠ> :total transposeMat_
-- Chapter3.transposeMat_ is Total


addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let added = Data.Vect.zipWith (+) x y in
                                    added :: addMatrix xs ys


-- λΠ> :total addMatrix
-- Chapter3.addMatrix is Total


multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect k a) -> Vect n (Vect k a)
multMatrix lhs rhs = multMatrixInner lhs (transposeMat rhs)
  where
    sumOfProduct : (Num a, Num a) => (x : Vect m1 a) -> (y : Vect m1 a) -> a
    sumOfProduct x y = foldl (+) 0 (zipWith (*) x y)

    processSingle : Num a => (x : Vect m1 a) -> (y : Vect m1 a) -> (xs : Vect len (Vect m1 a)) -> (ys : Vect len1 (Vect m1 a)) -> Vect (S len1) a
    processSingle x y [] [] = sumOfProduct x y :: []
    processSingle x y [] (z :: xs) = ?sumOfProduct_rhs_4
    processSingle x y (z :: xs) [] = ?sumOfProduct_rhs_1
    processSingle x y (z :: xs) (w :: ys) = ?sumOfProduct_rhs_5

    multMatrixInner : Num a => Vect n (Vect m a) -> Vect k (Vect m a) -> Vect n (Vect k a)
    multMatrixInner [] [] = []
    multMatrixInner [] (x :: xs) = []
    multMatrixInner (x :: xs) [] = [] :: multMatrixInner xs []
    multMatrixInner (x :: xs) (y :: ys) = processSingle x y xs ys :: ?multMatrixInner_rhs_14
