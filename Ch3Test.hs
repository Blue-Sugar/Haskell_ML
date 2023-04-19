module Ch3Test where
import MatrixAndVector
import Ch3
-- 活性化関数
funcTest :: (Vector -> Vector) -> [(Vector, Vector)] -> [Bool]
funcTest f = map (\x -> f (fst x) == snd x)


stepFuncTest :: [(Vector, Vector)]
stepFuncTest = [([1.0, 0.5, 0.0, -0.5], [1.0, 1.0, 0.0, 0.0])]

w1 = [[0.1, 0.3, 0.5], [0.2, 0.4, 0.6]]
w2 = [[0.1, 0.4], [0.2, 0.5], [0.3, 0.6]]
w3 = [[0.1, 0.3], [0.2, 0.4]]
b1 = [0.1, 0.3, 0.5]
b2 = [0.1, 0.2]
b3 = [0.1, 0.2]

x = [1.0, 0.5]

z1 = sigmoidFunc (forward x w1 b1)
z2 = sigmoidFunc (forward z1 w2 b2)
z3 = forward z2 w3 b3