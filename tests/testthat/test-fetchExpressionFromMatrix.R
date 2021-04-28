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

test_that("fetchExpressionFromMatrix - no gene", {
  expect_equal(
    .fetchExpressionFromMatrix(matrix),
    c(0, 0, 0)
  )
})

test_that("fetchExpressionFromMatrix - single gene - missing", {
  expect_equal(
    .fetchExpressionFromMatrix(matrix, 'E'),
    c(0, 0, 0)
  )
})

test_that("fetchExpressionFromMatrix - single gene - present", {
  expect_equal(
    .fetchExpressionFromMatrix(matrix, 'A'),
    c(0, 4, 0)
  )
})

test_that("fetchExpressionFromMatrix - multiple genes - all present", {
  expect_equal(
    .fetchExpressionFromMatrix(matrix, c('A', 'B')),
    c(0.5, 2.5, 1)
  )
})

test_that("fetchExpressionFromMatrix - multiple genes - only one present", {
  expect_equal(
    .fetchExpressionFromMatrix(matrix, c('C', 'E', 'F')),
    c(2, 4, 5)
  )
})
