context("Configuration Options")

p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length, mode = 'markers', type = 'scatter')

mb_names <- modebar_names()
len_mb_names <- length(mb_names)

modebar_diff <- function(x, y = NULL) {
  y <- y %||% mb_names
  as.list(setdiff(unlist(y), unlist(x)))
}
test_that("modeBarButtonsToKeep works as expected", {
  mb_keep <- list("toImage", "zoom2d")
  mb_remove <- modebar_diff(mb_keep)
  
  L <-  config(p, modeBarButtonsToKeep = mb_keep)
  
  L_mb_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_mb_remove, mb_remove)
  expect_equal(length(L_mb_remove), len_mb_names - length(mb_keep))
})

test_that("modeBarButtonsToKeep works when empty", {
  mb_keep <- list()
  mb_remove <- modebar_diff(mb_keep)
  
  L <- config(p, modeBarButtonsToKeep = mb_keep)
  L_mb_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_mb_remove, as.list(mb_names))
  expect_equal(length(L_mb_remove), len_mb_names)
})

test_that("modeBarButtonsToKeep works when all names given", {
  mb_keep <- as.list(mb_names)
  mb_remove <- modebar_diff(mb_keep)
  
  L <-  config(p, modeBarButtonsToKeep = mb_keep)
  
  L_mb_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_mb_remove, list())
  expect_equal(length(L_mb_remove), 0)
})

test_that("modeBarButtonsToKeep works with vectors of length 1", {
  mb_keep <- c("toImage")
  mb_remove <- modebar_diff(mb_keep)
  
  L <-  config(p, modeBarButtonsToKeep = mb_keep)
  L_mb_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_mb_remove, mb_remove)
  expect_equal(length(L_mb_remove), len_mb_names - 1)
})
