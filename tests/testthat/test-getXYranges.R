df <- tribble(
     ~x,    ~y,
  2.620, 16.46,
  2.875, 17.02,
  2.320, 18.61
)

test_that("getXYranges - 1", {
  expect_equal(
    .getXYranges(df),
    list(
      x = list(
        min = 2,
        max = 3
      ),
      y = list(
        min = 15,
        max = 20
      )
    )
  )
})
