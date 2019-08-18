library(imagerExtra)
library(checkmate)

class_imager <- "cimg"
class_pixset <- "pixset"

notim <- 1
im <- boats
im2 <- imrep(im, 2) %>% imappend(., "z")
gim <- grayscale(im)
im_c2 <- imrep(gim, 2) %>% imappend(., "c")
gim2 <- imrep(gim, 2) %>% imappend(., "z")
gim_bad <- gim
gim_bad[2,2] <- NA
im_bad <- im
im_bad[1,1,1] <- NA
gim_uniform <- as.cimg(matrix(1,100,100))

impix <- boats %>% as.pixset
gimpix <- gim %>% as.pixset
gim2pix <- gim2 %>% as.pixset
gim_badpix <- gim_bad %>% as.pixset

range_bad1 <- c(1,1,1)
range_bad2 <- c(-1,1)
range_bad3 <- c(NA, 255)
range_badorder <- c(255, 0)
range_bad4 <- c(1,1)

return_bad1 <- "A"
return_bad2 <- -1
return_bad3 <- c(0.1,0.1,0.1)
return_bad4 <- NA
return_bad5 <- NULL
