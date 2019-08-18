# check assert_logical_one_elem() and assert_positive_numeric_one_elem()

test_that("adaptive_double_plateaus_histogram_equalization",
{
  t_up <- 1000
  t_down <- 100
  t_bad1 <- NULL
  t_bad2 <- NA
  t_bad3 <- "T"
  t_bad4 <- c(1,1)
  
  expect_error(EqualizeDP(gim_bad, t_down, t_up))
  expect_error(EqualizeDP(gim_uniform, t_down, t_up))
  
  expect_error(EqualizeDP(gim, t_down, t_up, range = range_bad1))
  
  expect_error(EqualizeDP(gim, t_down, t_up, N_bad1))
  expect_error(EqualizeDP(gim, t_down, t_up, N_bad2))
  expect_error(EqualizeDP(gim, t_down, t_up, N_bad3))
  expect_error(EqualizeDP(gim, t_down, t_up, N_bad4))
  expect_error(EqualizeDP(gim, t_down, t_up, N_bad5))
  
  expect_error(EqualizeDP(gim, t_down, t_bad1))
  expect_error(EqualizeDP(gim, t_down, t_bad2))
  expect_error(EqualizeDP(gim, t_down, t_bad3))
  expect_error(EqualizeDP(gim, t_down, t_bad4))
  expect_error(EqualizeDP(gim, t_bad1, t_up))
  expect_error(EqualizeDP(gim, t_bad2, t_up))
  expect_error(EqualizeDP(gim, t_bad3, t_up))
  expect_error(EqualizeDP(gim, t_bad4, t_up))
  expect_error(EqualizeDP(gim, t_up, t_down))
  
  expect_class(EqualizeDP(gim, t_down, t_up), class_imager)
  
  n_bad <- 0
  n_bad1 <- -1
  n_bad2 <- NULL
  n_bad3 <- NA
  n_bad4 <- "A"
  n_bad5 <- 2
  
  N_bad1 <- NA
  N_bad2 <- 1
  
  param_boats <- EqualizeADP(gim, returnparam = TRUE)
  
  expect_error(EqualizeADP(gim_bad))
  
  expect_error(EqualizeADP(gim, n_bad))
  expect_error(EqualizeADP(gim, n_bad1))
  expect_error(EqualizeADP(gim, n_bad2))
  expect_error(EqualizeADP(gim, n_bad3))
  expect_error(EqualizeADP(gim, n_bad4))
  expect_error(suppressWarnings(EqualizeADP(gim, n_bad5)))
  
  expect_error(EqualizeADP(gim, N = N_bad1))
  expect_error(EqualizeADP(gim, N = N_bad2))

  expect_error(EqualizeADP(gim, range = range_bad1))

  expect_error(EqualizeADP(gim, return_bad1))
  expect_error(EqualizeADP(gim, return_bad2))
  expect_error(EqualizeADP(gim, return_bad3))
  expect_error(EqualizeADP(gim, return_bad4))
  expect_error(EqualizeADP(gim, return_bad5))
  
  expect_equal(EqualizeDP(gim, param_boats[1], param_boats[2]), EqualizeADP(gim))
  expect_class(EqualizeADP(gim), class_imager)
  expect_class(EqualizeADP(gim, returnparam = TRUE), "numeric")
})