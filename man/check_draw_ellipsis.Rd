% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-helpers.R
\name{check_draw_ellipsis}
\alias{check_draw_ellipsis}
\title{Checks for undesired arguments in ellipsis in $draw_leafdown method}
\usage{
check_draw_ellipsis(...)
}
\arguments{
\item{...}{Additional arguments given to \code{leaflet::addPolygons}}
}
\value{
List containing arguments in ... as elements
}
\description{
Checks arguments in ellipsis for undesired inputs such as `layerId` which may
collide with internal structure of leafdown and returns a "cleaned" version of
the arguments by removing or redefining problematic inputs.
e.g. `layerId` is removed from arg_list when set.
}
