module Ch2 where
import MatrixAndVector
import Data.List

type Perceptron = Vector -> Double


makePerceptron :: Vector -> Double -> Perceptron
makePerceptron w bias x = if a + bias <= 0.0 then 0.0 else 1.0
    where a = elementByMatrix ((transpose . matrixByVector) w `mm` matrixByVector x)  :: Double

addOfPerceptron :: Perceptron
addOfPerceptron = makePerceptron [0.5, 0.5] (-0.7)
nandOfPerceptron :: Perceptron
nandOfPerceptron = makePerceptron [-0.5, -0.5] 0.7
orOfPerceptron :: Perceptron
orOfPerceptron = makePerceptron [0.5, 0.5] (-0.2)
xorOfPerceptron :: Perceptron
xorOfPerceptron [x1, x2] = addOfPerceptron [s1, s2]
    where
        s1 = nandOfPerceptron [x1, x2]
        s2 = orOfPerceptron [x1, x2]


