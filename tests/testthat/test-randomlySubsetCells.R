test_that("randomlySubsetCells - 100%", {
  result <- .randomlySubsetCells(mtcars, 100)
  expect_equal(ncol(result), ncol(mtcars))
  expect_equal(nrow(result), nrow(mtcars))
})

test_that("randomlySubsetCells - 50%", {
  result <- .randomlySubsetCells(mtcars, 50)
  expect_equal(ncol(result), ncol(mtcars))
  expect_equal(nrow(result), nrow(mtcars)/2, tolerance=1)
})
