
test_that("setRowOrder - random - format", {
  expect_equal(
    class(.setRowOrder(mtcars, order="Random")),
    "data.frame"
  )
})

test_that("setRowOrder - random - missing input", {
  expect_error(.setRowOrder(mtcars))
})

test_that("setRowOrder - random - wrong input", {
  expect_error(.setRowOrder(c(1,2,3), order="Random"))
})

test_that("setRowOrder - highest expression on top", {
  expect_equal(
    .setRowOrder(
        mtcars %>% rename(level = 'wt'),
        "Highest expression on top"
      ) %>%
      pull(level),
    mtcars %>% arrange(wt) %>% pull(wt)
  )
})
