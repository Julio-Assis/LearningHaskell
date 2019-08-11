import Data.List.Split (chunksOf)
import MatrixBasics (getDimension, transpose, dotProduct)
import TypeDefinitions (Matrix, Dimension)

matrixMultiplication :: (Num a) => Matrix a -> Matrix a -> Either String (Matrix a)
matrixMultiplication matrixA matrixB
  | colsA /= rowsB = Left $ error $ "matrices have invalid dimensions! A has " ++ show colsA ++ " cols and B has " ++ show rowsB ++ " rows"
  | otherwise = Right $ chunksOf colsB $ dotProduct <$> matrixA <*> bTranspose
  where (rowsA,colsA) = getDimension matrixA
        (rowsB,colsB) = getDimension matrixB
        bTranspose = transpose matrixB
