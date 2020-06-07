context("Configuration Options")

p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length, mode = 'markers', type = 'scatter')

modebar_names <- modebar_names()
len_modebar_names <- length(modebar_names)

modebar_diff <- function(x, y = NULL) {
  y <- y %||% modebar_names
  as.list(setdiff(unlist(y), unlist(x)))
}
test_that("modeBarButtonsToKeep works as expected", {
  modebar_keep <- list("toImage", "zoom2d")
  modebar_remove <- modebar_diff(modebar_keep)
  
  L <- p  %>% config(modeBarButtonsToKeep = modebar_keep)
  
  L_modebar_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_modebar_remove, modebar_remove)
  expect_equal(length(L_modebar_remove), len_modebar_names - length(modebar_keep))
})

test_that("modeBarButtonsToKeep works when empty", {
  modebar_keep <- list()
  modebar_remove <- modebar_diff(modebar_keep)
  
  L <- p  %>% config(modeBarButtonsToKeep = modebar_keep)
  L_modebar_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_modebar_remove, as.list(modebar_names))
  expect_equal(length(L_modebar_remove), len_modebar_names)
})

test_that("modeBarButtonsToKeep works when all names given", {
  modebar_keep <- as.list(modebar_names)
  modebar_remove <- modebar_diff(modebar_keep)
  
  L <- p  %>% config(modeBarButtonsToKeep = modebar_keep)
  
  L_modebar_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_modebar_remove, list())
  expect_equal(length(L_modebar_remove), 0)
})

test_that("modeBarButtonsToKeep works with vectors of length 1", {
  modebar_keep <- c("toImage")
  modebar_remove <- modebar_diff(modebar_keep)
  
  L <- p  %>% config(modeBarButtonsToKeep = modebar_keep)
  L_modebar_remove <- L$x$config$modeBarButtonsToRemove
  
  expect_equal(L_modebar_remove, modebar_remove)
  expect_equal(length(L_modebar_remove), len_modebar_names - 1)
})
