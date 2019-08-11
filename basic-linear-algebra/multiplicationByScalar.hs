import TypeDefinitions (Matrix, Dimension)

multiplicationByScalar :: (Num a) => Matrix a -> a -> Matrix a
multiplicationByScalar matrixA scalar =
  let multRowByScalar = (\row -> map (*scalar) row)
  in map multRowByScalar matrixA