# check assert_im(), assert_range(), assert_s(), assert_s_left_right()

test_that("simplest_color_balance",
{
  s_c <- 0.1
  s_c2 <- "0.1%"
  s_bad1 <- -1
  s_bad2 <- 1000
  s_bad3_1 <- 60
  s_bad3_2 <- 70
  s_bad4 <- NULL
  s_bad5 <- NA
  s_bad6 <- "Hello"

  expect_error(BalanceSimplest(notim, s_c, s_c))
  expect_error(BalanceSimplest(gim2, s_c, s_c))
  expect_error(BalanceSimplest(im, s_c, s_c))
  expect_error(BalanceSimplest(gim_bad, s_c, s_c))
  
  expect_error(BalanceSimplest(gim, s_c, s_c, range = range_bad1))
  expect_error(BalanceSimplest(gim, s_c, s_c, range = range_bad2))
  expect_error(BalanceSimplest(gim, s_c, s_c, range = range_bad3))
  expect_error(BalanceSimplest(gim, s_c, s_c, range = range_badorder))
  
  expect_error(BalanceSimplest(gim, s_bad1, s_c))
  expect_error(BalanceSimplest(gim, s_c, s_bad2))
  expect_error(BalanceSimplest(gim, s_bad3_1, s_bad3_2))
  expect_error(BalanceSimplest(gim, s_c, s_bad4))
  expect_error(BalanceSimplest(gim, s_c, s_bad5))
  expect_error(BalanceSimplest(gim, s_bad6, s_C))
  
  expect_class(BalanceSimplest(gim, s_c, s_c), class_imager)
  expect_class(BalanceSimplest(gim, s_c2, s_c2), class_imager)
  expect_equal(BalanceSimplest(gim, s_c, s_c), BalanceSimplest(gim, s_c2, s_c2))
})
