module Ch3 where
import MatrixAndVector


stepFunc :: Vector -> Vector
stepFunc  = map (\x -> if x > 0 then 1 else 0) 

sigmoidFunc :: Vector -> Vector
sigmoidFunc = map (\x -> 1.0 / (1.0 + exp (-1 * x)))

reluFunc :: Vector -> Vector
reluFunc = map (`max` 0)

softmaxFunc :: Vector -> Vector
softmaxFunc v = map (\x -> exp (x-max_of_v)/sum_of_exp) v
    where sum_of_exp = sum  (map (\x -> exp (x - max_of_v)) v) 
          max_of_v = maximum v