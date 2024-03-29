% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{plotnorm}
\alias{plotnorm}
\title{Plot a normal distribution shading below x}
\usage{
plotnorm(
  x = 0,
  mu = 0,
  sigma = 1,
  below = TRUE,
  show_proportion = TRUE,
  show_x = TRUE,
  show_param = TRUE,
  text_size = 14,
  font_family = "sans",
  shade_fill = "royalblue"
)
}
\arguments{
\item{x}{number to divide normal distribution}

\item{mu}{mean of normal distribution}

\item{sigma}{standard deviation of normal distribution}

\item{below}{If TRUE, shade lower portion of normal distribution}

\item{show_proportion}{If TRUE, display proportions}

\item{show_x}{If TRUE, display x value}

\item{show_param}{If TRUE, display mean and standard deviation}

\item{text_size}{Base text size}

\item{font_family}{Name of font}

\item{shade_fill}{Color of shaded region}
}
\description{
Plot a normal distribution shading below x
}
\examples{
plotnorm(90, 100, 15)
}
