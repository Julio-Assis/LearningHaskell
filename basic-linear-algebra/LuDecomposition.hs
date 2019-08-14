module LuDecomposition (
  luDecomposition
) where

import TypeDefinitions (Dimension, Matrix, Vector)
import MatrixBasics (getDimension, transpose, identity, zeros)
import MatrixMultiplication (matrixMultiplication)

type Lower a = [[a]]
type Upper a = [[a]]
type ColumnIndex = Int

luDecomposition :: (Fractional a, Num a) => Matrix a -> Either String (Lower a, Upper a)
luDecomposition inputMatrix
  | rows /= cols = Left $ error $ "Input matrix is not square it has shape (" ++ show rows ++ "," ++ show cols ++ ")"
  | otherwise = Right $ calculateLu inputMatrix 0 (initLower inputMatrix) inputMatrix
  where (rows, cols) = getDimension inputMatrix

calculateLu :: (Fractional a, Num a) => Matrix a -> ColumnIndex -> Lower a -> Upper a -> (Lower a, Upper a)
calculateLu matrix currentCol lower upper
  | length matrix == 1 = (lower, upper)
calculateLu matrix currentCol lower upper =
  let multipliers = getMultipliers matrix
      newLower = insertMultipliers lower currentCol multipliers
      newUpper = updateUpperMatrix upper multipliers
      removeFirstCol = (\matrix -> [xs | x:xs <- matrix])
      generateNewMatrix m = transpose $ drop (currentCol + 1 ) $ transpose $ drop (currentCol + 1) m
  in calculateLu (generateNewMatrix newUpper) (currentCol + 1) newLower newUpper

getMultipliers :: (Fractional a) => Matrix a -> Vector a
getMultipliers matrix =
    let firstRow = head matrix
        otherRows = tail matrix 
        pivot = head firstRow
    in map (/pivot) $ map (\row -> head row) otherRows

insertMultipliers :: Lower a -> ColumnIndex -> Vector a -> Lower a
insertMultipliers lower currentCol multipliers =
  let transposedLower = transpose lower
      (firstColumns, lastColumns) = splitAt currentCol transposedLower
      (desiredColumn, finalColumns) = splitAt 1 lastColumns
      lastIndex = (length $ desiredColumn !! 0) - 1
      newColumn = [if currentCol < position then multipliers !! (position - currentCol - 1) else desiredColumn !! 0 !! position | position <- [0..lastIndex]]
  in transpose $ firstColumns ++ [newColumn] ++ finalColumns

updateUpperMatrix :: (Num a) => Upper a -> Vector a -> Upper a
updateUpperMatrix upper multipliers =
  let startRow = (length upper) - length multipliers
      pivotRowIndex = startRow - 1
      pivotRow = upper !! pivotRowIndex
      (firstRows, lastRows) = splitAt (pivotRowIndex+1) upper
      multipliersLastIndex = (length multipliers) - 1
      subtractWithMultiplier multiplier el2 el1 = el1 - multiplier * el2
      newLowerPart = [zipWith (subtractWithMultiplier (multipliers !! i)) pivotRow (lastRows !! i) | i <- [0..multipliersLastIndex]]
  in firstRows ++ newLowerPart

initLower :: (Num a) => Matrix a -> Lower a
initLower inputMatrix =
  let (size,_) = getDimension inputMatrix
  in identity size

initUpper :: (Num a) => Matrix a -> Upper a
initUpper inputMatrix =
  let (size,_) = getDimension inputMatrix
  in identity size