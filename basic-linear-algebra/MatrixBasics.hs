module MatrixBasics(
  getDimension,
  transpose,
  dotProduct,
  identity,
  zeros
) where

import TypeDefinitions (Matrix, Dimension)

getDimension :: Matrix a -> Dimension
getDimension matrix = 
  let rows = length matrix
      cols = length $ matrix !! 0
  in if cols /= 0 then (rows, cols) else (0, 0)

transpose :: Matrix a -> Matrix a
transpose [] = []
transpose matrixA =
    let getFirstCol = (\matrix -> [[x | x:xs <- matrix]])
        removeFirstCol = (\matrix -> [xs | x:xs <- matrix])
        firstColumn = getFirstCol matrixA
        firstRow:_ = firstColumn
        newRow = if length firstRow > 0 then firstColumn else []
    in newRow ++ (transpose $ removeFirstCol matrixA)

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct = (\vectorA vectorB -> sum $ zipWith (*) vectorA vectorB)

identity :: (Num a) => Int -> Matrix a
identity size = [[if row == col then 1 else 0 | col <- [1..size]] | row <- [1..size]]

zeros :: (Num a) => Int -> Matrix a
zeros size = [[0 | col <- [1..size]] | row <- [1..size]]