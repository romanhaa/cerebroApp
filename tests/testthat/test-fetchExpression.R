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

test_that("fetchExpression - dense matrix", {
  expect_equal(
    .fetchExpression(matrix=matrix_dense),
    c(0, 0, 0)
  )
})

test_that("fetchExpression - sparse matrix", {
  expect_equal(
    .fetchExpression(matrix=matrix_sparse),
    c(0, 0, 0)
  )
})

test_that("fetchExpression - h5 matrix", {
  expect_equal(
    .fetchExpression(matrix=matrix_h5),
    c(0, 0, 0)
  )
})

test_that("fetchExpression - matrix & API", {
  expect_equal(
    .fetchExpression(matrix='test', API='test'),
    NULL
  )
})
