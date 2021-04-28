df <- tribble(
  ~cell, ~sample, ~cluster, ~cell_cycle,
    'A',   'one',      '1',        'G1',
    'B',   'one',      '1',         'S',
    'C',   'one',      '2',         'S',
    'D',   'one',      '2',        'G1',
    'E',   'two',      '2',       'G2M',
    'F',   'two',      '2',        'G1',
    'G',   'two',      '3',       'G2M',
    'H',   'two',      '4',         'S'
)

test_that("calculateTableAB - groups - long - count", {
  expect_equal(
    .calculateTableAB(df, 'sample', 'cluster', 'long', FALSE),
    tribble(
      ~sample, ~cluster, ~count, ~total_cell_count,
        'one',      '1',      2,                 4,
        'one',      '2',      2,                 4,
        'two',      '2',      2,                 4,
        'two',      '3',      1,                 4,
        'two',      '4',      1,                 4
    ) %>%
    mutate(sample = factor(sample), cluster = factor(cluster))
  )
})

test_that("calculateTableAB - cell cycle - long - count", {
  expect_equal(
    .calculateTableAB(df, 'cluster', 'cell_cycle', 'long', FALSE),
    tribble(
       ~cluster, ~cell_cycle, ~count, ~total_cell_count,
            '1',     'G1',         1,                 2,
            '1',      'S',         1,                 2,
            '2',     'G1',         2,                 4,
            '2',    'G2M',         1,                 4,
            '2',      'S',         1,                 4,
            '3',    'G2M',         1,                 1,
            '4',      'S',         1,                 1
    ) %>%
    mutate(cluster = factor(cluster), cell_cycle = factor(cell_cycle))
  )
})

test_that("calculateTableAB - groups - wide - count", {
  expect_equal(
    .calculateTableAB(df, 'sample', 'cluster', 'wide', FALSE),
    tribble(
      ~sample, ~total_cell_count, ~'1', ~'2', ~'3', ~'4',
       'one',                  4,    2,    2,    0,    0,
       'two',                  4,    0,    2,    1,    1
    ) %>%
    mutate(sample = factor(sample))
  )
})

test_that("calculateTableAB - groups - wide - percent", {
  expect_equal(
    .calculateTableAB(df, 'sample', 'cluster', 'wide', TRUE),
    tribble(
      ~sample, ~total_cell_count, ~'1', ~'2', ~'3', ~'4',
        'one',                 4,  0.5,  0.5,    0,    0,
        'two',                 4,    0,  0.5, 0.25, 0.25
    ) %>%
    mutate(sample = factor(sample))
  )
})
