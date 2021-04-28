## define test matrix
matrix <- matrix(
  c(0,1,2,0,4,1,4,0,0,2,5,0),
  nrow = 4,
  ncol = 3,
  dimnames = list(
    c('A', 'B', 'C', 'D'),
    c('cell-1', 'cell-2', 'cell-3')
  )
)
matrix <- as(matrix, "sparseMatrix")
output_h5 <- tempfile()
if (file.exists(output_h5)) {
  unlink(output_h5)
}
quiet <- HDF5Array::writeTENxMatrix(Matrix::t(matrix), output_h5, group='expression')
matrix_h5 <- Matrix::t(HDF5Array::TENxMatrix(output_h5, group='expression'))

test_that("fetchExpressionFromH5File - no gene", {
  expect_equal(
    .fetchExpressionFromH5File(matrix_h5),
    c(0, 0, 0)
  )
})

test_that("fetchExpressionFromH5File - single gene - missing", {
  expect_equal(
    .fetchExpressionFromH5File(matrix_h5, 'E'),
    c(0, 0, 0)
  )
})

test_that("fetchExpressionFromH5File - single gene - present", {
  expect_equal(
    .fetchExpressionFromH5File(matrix_h5, 'A'),
    c(0, 4, 0)
  )
})

test_that("fetchExpressionFromH5File - multiple genes - all present", {
  expect_equal(
    .fetchExpressionFromH5File(matrix_h5, c('A', 'B')),
    c(0.5, 2.5, 1)
  )
})

test_that("fetchExpressionFromH5File - multiple genes - only one present", {
  expect_equal(
    .fetchExpressionFromH5File(matrix_h5, c('C', 'E', 'F')),
    c(2, 4, 5)
  )
})
