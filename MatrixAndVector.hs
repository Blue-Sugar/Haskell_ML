module MatrixAndVector where 
import Data.List

-- 基本的に行列の型などのチェックは行えない


type Vector = [Double]

plusForeachElement :: Vector -> Vector -> Vector
plusForeachElement [] [] = []
plusForeachElement (x:xs) (y:ys) = x + y : plusForeachElement xs ys

multForeachElement :: Vector -> Vector -> Vector
multForeachElement [] [] = []
multForeachElement (x:xs) (y:ys) = x * y : multForeachElement xs ys

sumOfElement :: Vector -> Double
sumOfElement = sum

dot :: Vector -> Vector -> Double
[] `dot` [] = 0.0
(x:xs) `dot` (y:ys) = x*y + (xs `dot` ys)


type Matrix = [[Double]]

mm :: Matrix -> Matrix -> Matrix
[xs] `mm` yss = [map (\ys -> xs `dot` ys) (transpose yss)]
(xs:xss) `mm` yss = ([xs] `mm` yss) ++ (xss `mm` yss)

--- ベクトルからn*1行列を作る
matrixByVector ::Vector -> Matrix
matrixByVector [x] = [[x]]
matrixByVector (x:xs) = [x] : matrixByVector xs

--- 1*1行列から要素, ベクトルへ
elementByMatrix :: Matrix -> Double
elementByMatrix [[x]] = x
vectorByMatrix :: Matrix -> Vector
vectorByMatrix = concat


-- Vector v と Matrix m の積を計算して、 Vector b をたす
forward :: Vector -> Matrix -> Vector -> Vector
forward v m = plusForeachElement (concat ([v] `mm` m))