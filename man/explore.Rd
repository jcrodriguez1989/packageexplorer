% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{explore}
\alias{explore}
\title{Explore packages and functions interactively}
\usage{
explore(names, include_deps = FALSE, plot = TRUE)
}
\arguments{
\item{names}{Character names of the packages and functions to explore.
Functions must be loaded in the namespace (with `library(FUN_PKG)`), or be
written as `FUN_PKG::FUN_NAME`.
It is recommended to write package names as `"package:PKG_NAME"`.}

\item{include_deps}{Logical if dependencies from other packages should be
included. Dependencies stand for "Depends" and "Imports".}

\item{plot}{logical indicating if graph should be plotted. It can take long
time to render if it is too big.}
}
\value{
the graph as list, with nodes and edges that can be directly used by
  visNetwork library.
}
\description{
Given package and function names interactively explore them in a graph.
}
\examples{
\dontrun{
  explore(c("package:packageexplorer", "base::library"),
          include_deps = TRUE)
}

}
