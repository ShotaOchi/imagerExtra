#' Optical Character Recognition with tesseract
#'
#' OCR and OCR_data are wrappers for ocr and ocr_data of tesseract package.
#' You need to install tesseract package to use these functions.
#' @name OCR
#' @param imorpx a grayscale image of class cimg or a pixel set
#' @param engine a tesseract engine. See the reference manual of tesseract for detail.
#' @param HOCR if TRUE return results as HOCR xml instead of plain text
#' @author Shota Ochi
#' @examples
#' hello <- DenoiseDCT(papers, 0.01) %>% ThresholdAdaptive(., 0.1, range = c(0,1))
#' if (requireNamespace("tesseract", quietly = TRUE))
#' {
#'   OCR(hello) %>% cat
#'   OCR_data(hello)
#' }
NULL

#' @rdname OCR
#' @export
OCR <- function(imorpx, engine = tesseract::tesseract("eng"), HOCR=FALSE) 
{
  CheckSanityimorpx(imorpx)
  if (is.pixset(imorpx)) 
  {
    imorpx <- as.cimg(imorpx)
  }
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  imager::save.image(imorpx, tmp)
  tesseract::ocr(tmp, engine = engine, HOCR = HOCR)
}

#' @rdname OCR
#' @export
OCR_data <- function(imorpx, engine = tesseract::tesseract("eng")) 
{
  CheckSanityimorpx(imorpx)
  if (is.pixset(imorpx)) 
  {
    imorpx <- as.cimg(imorpx)
  }
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  imager::save.image(imorpx, tmp)
  tesseract::ocr_data(tmp, engine = engine)
}