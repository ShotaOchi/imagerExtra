# check assert_numeric_vec(), assert_numeric_one_elem(), and assert_positive0_numeric_one_elem()

test_that("Chan-Vese segmentation",
{
  num_one_bad1 <- "A"
  num_one_bad2 <- -1
  num_one_bad3 <- c(0.1,0.1,0.1)
  num_one_bad4 <- NA
  num_one_bad5 <- NULL
  num_one_bad6 <- 0
  
  expect_error(SegmentCV(gim_bad))
  
  expect_error(SegmentCV(gim, mu = num_one_bad1))
  expect_error(SegmentCV(gim, mu = num_one_bad3))
  expect_error(SegmentCV(gim, mu = num_one_bad4))
  expect_error(SegmentCV(gim, mu = num_one_bad5))
  
  expect_error(SegmentCV(gim, nu = num_one_bad4))
  
  expect_error(SegmentCV(gim, lambda1 = num_one_bad1))
  expect_error(SegmentCV(gim, lambda1 = num_one_bad3))
  expect_error(SegmentCV(gim, lambda1 = num_one_bad4))
  expect_error(SegmentCV(gim, lambda1 = num_one_bad5))
  
  expect_error(SegmentCV(gim, lambda2 = num_one_bad4))
  
  expect_error(SegmentCV(gim, tol = num_one_bad1))
  expect_error(SegmentCV(gim, tol = num_one_bad2))
  expect_error(SegmentCV(gim, tol = num_one_bad3))
  expect_error(SegmentCV(gim, tol = num_one_bad4))
  expect_error(SegmentCV(gim, tol = num_one_bad5))
  
  expect_error(SegmentCV(gim, maxiter = num_one_bad4))

  expect_error(SegmentCV(gim, dt = num_one_bad4))
  
  expect_error(SegmentCV(gim, initial = num_one_bad2))
  expect_error(SegmentCV(gim, initial = num_one_bad3))
  expect_error(SegmentCV(gim, initial = num_one_bad4))
  expect_error(SegmentCV(gim, initial = num_one_bad5))    
  expect_error(SegmentCV(gim, initial = 1))
  
  expect_error(SegmentCV(gim, returnstep = num_one_bad1))
  expect_error(SegmentCV(gim, returnstep = num_one_bad2))
  expect_error(SegmentCV(gim, returnstep = num_one_bad4))
  expect_error(SegmentCV(gim, returnstep = num_one_bad5))
  
  expect_class(SegmentCV(gim), class_pixset)
  expect_class(SegmentCV(gim, returnstep = c(1)), "list")
})
