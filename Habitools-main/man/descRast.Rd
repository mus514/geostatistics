% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe_raster.R
\name{descRast}
\alias{descRast}
\title{Get a detailed description report from a raster file}
\usage{
descRast(pathRaster)
}
\arguments{
\item{pathRaster}{Path to the raster from which we want to get description}
}
\value{
List of 'terra::describe()' output, 'NoData Value' and data type from 'terra::datatype()'
}
\description{
Get a detailed description report from a raster file
}
\examples{
\dontrun{
f <- system.file("ex/elev.tif", package="terra")
descRast(f)
}
}
