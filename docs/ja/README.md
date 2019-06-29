# imagerExtra

[![Build Status](https://travis-ci.org/ShotaOchi/imagerExtra.svg?branch=master)](https://travis-ci.org/ShotaOchi/imagerExtra)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ShotaOchi/imagerExtra?branch=master&svg=true)](https://ci.appveyor.com/project/ShotaOchi/imagerExtra)
[![CRAN Version](https://www.r-pkg.org/badges/version/imagerExtra)](https://cran.r-project.org/package=imagerExtra)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![codecov](https://codecov.io/gh/ShotaOchi/imagerExtra/branch/master/graph/badge.svg)](https://codecov.io/gh/ShotaOchi/imagerExtra)

## 翻訳
[:us: English](/README.md)

## 概要
imagerExtraはR言語の画像処理用パッケージ[imager](https://github.com/dahtah/imager)を拡張したパッケージです。


imagerExtraは画像処理のためのより発展的な関数を提供します。


関数に関する詳細はこちらから [imagerExtaraの使い方](https://cran.r-project.org/package=imagerExtra/vignettes/gettingstarted.html)


imagerを知らない方はこちらをご確認ください。 [imagerの紹介](http://dahtah.github.io/imager/)


imagerに精通してない方はこちらをご確認ください。[imagerの使い方](https://CRAN.R-project.org/package=imager/vignettes/gettingstarted.html)


## インストール
CRANもしくはGitHubからインストール可能です。


以下Rコードを実行し、imagerExtraを使用できます。
```r
# install from CRAN
install.packages("imagerExtra")
# install from GitHub
devtools::install_github("ShotaOchi/imagerExtra")
```

## 光学文字認識 (OCR)
imagerExtraでOCRを行うには[tesseract](https://github.com/ropensci/tesseract#tesseract)というRパッケージが必要です。


まだtesseractをインストールしてなければ[tesseractのインストールガイド](https://github.com/ropensci/tesseract#installation)をご覧ください。


これはimagerExtraがtesseractの適用範囲を拡大できることを示す簡単なデモです。
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


## コントリビューション
バグの報告やコントリビューションをお願いします。 [issues page](https://github.com/ShotaOchi/imagerExtra/issues)


このリポジトリをフォークして、バグ修正や追加機能を求めてプルリクエストを送ってください。


## 開発ログ
imagerExtraの開発履歴はこちらから。[https://shotaochi.github.io/](https://shotaochi.github.io/)
