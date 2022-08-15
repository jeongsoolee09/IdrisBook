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
    sumOfProduct : Num a => (x : Vect m1 a) -> (y : Vect m1 a) -> a
    sumOfProduct x y = foldl (+) 0 (zipWith (*) x y)

    sumsOfProducts : (Num a, Num a) => (x : Vect m1 a) -> (ys : Vect len1 (Vect m1 a)) -> Vect len1 a
    sumsOfProducts x [] = []
    sumsOfProducts x (y :: xs) = sumOfProduct x y :: sumsOfProducts x xs

    processSingle : Num a => (xs : Vect n1 (Vect m1 a)) -> (ys : Vect k1 (Vect m1 a)) -> Vect n1 (Vect k1 a)
    processSingle [] [] = []
    processSingle [] (x :: xs) = []
    processSingle (x :: xs) [] = [] :: processSingle xs []
    processSingle (x::xs) (y::ys) = (sumOfProduct x y :: (sumsOfProducts x ys)) :: processSingle xs (y::ys)

    multMatrixInner : Num a => Vect n (Vect m a) -> Vect k (Vect m a) -> Vect n (Vect k a)
    multMatrixInner [] [] = []
    multMatrixInner [] (x :: xs) = []
    multMatrixInner (x :: xs) [] = [] :: multMatrixInner xs []
    multMatrixInner xs ys = processSingle xs ys

-- λΠ> :total multMatrix
-- Chapter3.multMatrix is Total
