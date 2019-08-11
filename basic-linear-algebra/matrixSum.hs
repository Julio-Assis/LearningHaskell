import TypeDefinitions (Matrix, Dimension)
import MatrixBasics (getDimension)

matrixSum :: (Num a) => Matrix a -> Matrix a -> Either String (Matrix a)
matrixSum matrixA matrixB
  | dimA /= dimB = Left $ error $ "matrices have invalid dimensions! A is " ++ show dimA ++ " B is " ++ show dimB
  | otherwise = Right $ zipWith sumMatrixRow matrixA matrixB
  where dimA = getDimension matrixA
        dimB = getDimension matrixB
        sumMatrixRow rowA rowB = zipWith (+) rowA rowB

main = do
  blah <- getContents
  let a = [[1,2,3]] :: Matrix Int
  let dim = getDimension a
  print dim
