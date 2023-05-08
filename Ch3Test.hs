module Ch3Test where
import MatrixAndVector
import Ch3
-- 活性化関数
funcTest :: (Vector -> Vector) -> [(Vector, Vector)] -> [Bool]
funcTest f = map (\x -> f (fst x) == snd x)


stepFuncTest :: [(Vector, Vector)]
stepFuncTest = [([1.0, 0.5, 0.0, -0.5], [1.0, 1.0, 0.0, 0.0])]

