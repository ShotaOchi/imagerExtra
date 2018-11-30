# imagerExtra

[![Build Status](https://travis-ci.org/ShotaOchi/imagerExtra.svg?branch=master)](https://travis-ci.org/ShotaOchi/imagerExtra)
[![CRAN Version](https://www.r-pkg.org/badges/version/imagerExtra)](https://cran.r-project.org/package=imagerExtra)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![codecov](https://codecov.io/gh/ShotaOchi/imagerExtra/branch/master/graph/badge.svg)](https://codecov.io/gh/ShotaOchi/imagerExtra)

## About
imagerExtra is a R package for image processing based on the R package [imager](https://github.com/dahtah/imager).


imagerExtra provides advanced functions for image processing.


See the vignette [Getting Started with imagerExtara](https://cran.r-project.org/package=imagerExtra/vignettes/gettingstarted.html) to know what functions imagerExtra provides. 


See the [introduction of imager](http://dahtah.github.io/imager/) if you don't know imager.


See the [vignette of imager](https://CRAN.R-project.org/package=imager/vignettes/gettingstarted.html) if you aren't familiar with imager.


## Installation
You can install imagerExtra from CRAN or GitHub.


Run the following R code to install imagerExtra.
```r
# install from CRAN
install.packages("imagerExtra")
# install from GitHub
devtools::install_github("ShotaOchi/imagerExtra")
```

## Optical Character Recognition (OCR)
You need the R package [tesseract](https://github.com/ropensci/tesseract#tesseract) to do OCR with imagerExtra.


See the [installation guide of tesseract](https://github.com/ropensci/tesseract#installation) if you haven't installed tesseract.


Here is a small demo that shows imagerExtra can expand the scope of the application of tesseract.
```r
library(imagerExtra)
# OCR doesn't work well for degraded images
plot(papers)
OCR(papers)
# OCR works well for images with high contrast and little noise
hello <- DenoiseDCT(papers, 0.01) %>% ThresholdAdaptive(., 0.1, range = c(0,1))
plot(hello)
OCR(hello)
```


## Contribution
You can create issues for any bug report or suggestion on the [issues page](https://github.com/ShotaOchi/imagerExtra/issues).


You're welcome to fork this repository and send me a pull request for bug fixes or additional features.


## Development Blog
URL of development blog of imagerExtra is [https://shotaochi.github.io/](https://shotaochi.github.io/).
