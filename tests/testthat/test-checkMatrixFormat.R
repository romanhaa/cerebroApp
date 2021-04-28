## define test matrix
matrix_dense <- matrix(
  c(0,1,2,0,4,1,4,0,0,2,5,0),
  nrow = 4,
  ncol = 3,
  dimnames = list(
    c('A', 'B', 'C', 'D'),
    c('cell-1', 'cell-2', 'cell-3')
  )
)
matrix_sparse <- as(matrix_dense, "sparseMatrix")
output_h5 <- tempfile()
if (file.exists(output_h5)) {
  unlink(output_h5)
}
quiet <- HDF5Array::writeTENxMatrix(Matrix::t(matrix_sparse), output_h5, group='expression')
matrix_h5 <- Matrix::t(HDF5Array::TENxMatrix(output_h5, group='expression'))

test_that("checkMatrixFormat - string", {
  expect_error(
    .checkMatrixFormat('test'),
    "provided object is not in one of the accepted matrix formats"
  )
})

test_that("checkMatrixFormat - dense matrix", {
  expect_equal(
    .checkMatrixFormat(matrix_dense),
    TRUE
  )
})

test_that("checkMatrixFormat - sparse matrix", {
  expect_equal(
    .checkMatrixFormat(matrix_sparse),
    TRUE
  )
})

test_that("checkMatrixFormat - h5 matrix", {
  expect_equal(
    .checkMatrixFormat(matrix_h5),
    TRUE
  )
})
