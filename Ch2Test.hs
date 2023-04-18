module Ch2Test (isCh2Test) where
import MatrixAndVector
import Ch2


perceptronTest :: Perceptron -> [(Vector, Double)] -> [Bool]
perceptronTest perceptron =
    map (\input -> perceptron (fst input) == snd input)

addTest :: [(Vector, Double)]
addTest = [([1.0, 1.0], 1.0), ([1.0, 0.0], 0.0), ([0.0, 1.0], 0.0), ([0.0, 0.0], 0.0)]
nandTest :: [(Vector, Double)]
nandTest = [([1.0, 1.0], 0.0), ([1.0, 0.0], 1.0), ([0.0, 1.0], 1.0), ([0.0, 0.0], 1.0)]
orTest :: [(Vector, Double)]
orTest = [([1.0, 1.0], 1.0), ([1.0, 0.0], 1.0), ([0.0, 1.0], 1.0), ([0.0, 0.0], 0.0)]
xorTest :: [(Vector, Double)]
xorTest = [([1.0, 1.0], 0.0), ([1.0, 0.0], 1.0), ([0.0, 1.0], 1.0), ([0.0, 0.0], 0.0)]

ch2Test :: [[Bool]]
ch2Test =
    [ perceptronTest addOfPerceptron addTest
    , perceptronTest nandOfPerceptron nandTest
    , perceptronTest orOfPerceptron orTest
    , perceptronTest xorOfPerceptron xorTest]

isCh2Test :: Bool
isCh2Test = and (concat ch2Test)