% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finalize_fun.R
\name{px2ha}
\alias{px2ha}
\title{Change units of raster to values/ha}
\source{
https://stackoverflow.com/questions/79353000/what-is-the-proper-way-to-change-the-units-of-a-raster-to-account-for-the-area-o
}
\usage{
px2ha(r, ha2px = FALSE, unit = "ha", ...)
}
\arguments{
\item{r}{Raster}

\item{ha2px}{FALSE (default) will compute *pixel value* to *pixel value / hectar*. TRUE, will compute *pixel/ha* to *pixel value*}

\item{unit}{Units (default = 'ha') passed to `terra::cellSize()`}

\item{...}{more arguments passed to `terra::cellSize()`}
}
\value{
Raster with the values divided by each pixel area
}
\description{
Change the units of a raster while taking into account the actual pixel area
with the `terra::cellSize()` function. If ha2px is FALSE (default), will
calculate `r/cellarea` or will divide the values of each pixel (assuming
it is unit) with the pixel area in hectar. If ha2px is TRUE (assuming
it is unit/ha) the `r*cellarea` will ouput each pixel in terms of `unit`.
}
\examples{
# Illustrative example, but not real example
f <- system.file("ex/elev.tif", package="terra")
r <- terra::rast(f)
px2ha(r)
}
