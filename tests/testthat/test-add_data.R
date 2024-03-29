library(leafdown)
library(shiny)

context("Test 'add_data'")

test_that("Add data correctly", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$curr_data
  data$y <- 1:nrow(data)
  my_leafdown$add_data(data)

  expect_true(identical(data, my_leafdown$curr_data))
})

test_that("Add 'Null' as data throws error", {
  my_leafdown <- init_leafdown()
  expect_error(my_leafdown$add_data(NULL), "The given data must be a data.frame")
})

test_that("Add empty List as data throws error", {
  my_leafdown <- init_leafdown()

  expect_error(my_leafdown$add_data(data.frame()), "You cannot remove columns from the existing meta-data. Only add to it")
})

test_that("Changed values in data throws error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$curr_data

  # change data
  col <- 1
  row <- 42
  data[row, col] <- NA

  expect_error(my_leafdown$add_data(data), "You cannot change or reorder the existing meta-data. Only add to it. Use left_joins to avoid reordering")
})

test_that("Missing columns in data throws error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$curr_data

  # change data
  # random col to delete
  col <- floor(runif(1, min = 1, max = dim(data)[2]))
  data <- data[, -col]

  expect_error(my_leafdown$add_data(data), "You cannot remove columns from the existing meta-data. Only add to it")
})

test_that("Unequal number of rows as meta-data throws an error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$curr_data

  data_too_long <- data
  data_too_long[nrow(data_too_long) + 1, ] <- data_too_long[1, ]
  err_msg_too_long <- sprintf(
    "The number of rows of the given data \\(52\\) is not equal to the number of rows of the initial data \\(51\\)"
  )

  data_too_short <- data
  data_too_short <- data_too_short[-1, ]
  err_msg_too_short <- c(
    "The number of rows of the given data \\(50\\) is not equal to the number of rows of the initial data \\(51\\)"
  )

  expect_error(my_leafdown$add_data(data_too_long), regexp = err_msg_too_long)
  expect_error(my_leafdown$add_data(data_too_short), err_msg_too_short)
})

test_that("Reordering Data throws correct error", {
  my_leafdown <- init_leafdown()
  data <- my_leafdown$curr_data
  data$y <- nrow(data):1

  data <- data[order(data$y), ]

  expect_error(my_leafdown$add_data(data), "You cannot change or reorder the existing meta-data. Only add to it. Use left_joins to avoid reordering")
})
