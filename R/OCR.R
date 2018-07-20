#' Optical Character Recognition with tesseract
#'
#' OCR and OCR_data are shortcuts to ocr and ocr_data of tesseract
#' @name OCR
#' @param imorpix a grayscale image of class cimg or a pixel set
#' @param engine a tesseract engine. See the reference manual of tesseract for detail.
#' @param HOCR if TRUE return results as HOCR xml instead of plain text
#' @author Shota Ochi
#' @examples
#' hello <- DenoiseDCT(papers, 0.01) %>% ThresholdAdaptive(., 0)
#' OCR(hello) %>% cat
#' OCR_data(hello)
NULL

#' @rdname OCR
#' @export
OCR <- function(imorpix, engine = tesseract("eng"), HOCR=FALSE) {
  CheckSanityimorpix(imorpix)
  if (is.pixset(imorpix)) 
  {
    imorpix <- as.cimg(imorpix)
  }
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  imager::save.image(imorpix, tmp)
  ocr(tmp, engine = engine, HOCR = HOCR)
}

#' @rdname OCR
#' @export
OCR_data <- function(imorpix, engine = tesseract("eng")) {
  CheckSanityimorpix(imorpix)
  if (is.pixset(imorpix)) 
  {
    imorpix <- as.cimg(imorpix)
  }
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  imager::save.image(imorpix, tmp)
  ocr_data(tmp, engine = engine)
}
