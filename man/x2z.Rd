% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{x2z}
\alias{x2z}
\title{Convert x to a z-score}
\usage{
x2z(x, mu = mean(x, na.rm = T), sigma = stats::sd(x, na.rm = T))
}
\arguments{
\item{x}{a numeric vector}

\item{mu}{mean}

\item{sigma}{standard deviation}
}
\description{
Convert x to a z-score
}
