
#' Function that converts pdf files to svg. Must have the pdf2svg program installed (https://github.com/dawbarton/pdf2svg)
#' @export
#' @param s Filename of the pdf file. Omit the ".pdf" at the end.
pdf2svg <- function(s) {shell(paste0("pdf2svg ",s,".pdf ",s,".svg"))}


#' Computes covariances of composite scores given a covariance matrix and a weight matrix
#' @export
#' @param Sigma Covariance matrix
#' @param w Weight matrix. Must have the same number of rows as R
#' @param correlation If TRUE, return correlations instead of covariances
#' @examples
#' Sigma <- matrix(0.6, nrow = 4, ncol = 4)
#' diag(Sigma) <- 1
#' w <- matrix(c(1,0,
#'               1,0,
#'               0,1,
#'               0,1),
#'               nrow = 4, ncol = 2)
#' composite_covariance(Sigma, w)
composite_covariance <- function(Sigma,w, correlation = FALSE) {
  Sigma_composite <- t(w) %*% Sigma %*% w
  if (correlation) cov2cor(Sigma_composite) else Sigma_composite
  }

#' Plot a normal distribution shading below x
#' @param x number to divide normal distribution
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#' @param below If TRUE, shade lower portion of normal distribiton
#' @param show_proportion If TRUE, display proportions
#' @param show_x If TRUE, display x value
#' @param show_param If TRUE, display mean and standard deviation
#' @param text_size Base text size
#' @param shade_fill Color of shaded region
#' @import ggplot2
#' @example
#' plotnorm(90, 100, 15)
plotnorm <- function(x,mu,sigma, below = TRUE, show_proportion = TRUE, show_x = TRUE, show_param = TRUE, text_size = 14, shade_fill = "royalblue") {
  dx <- round(x,2)
  p <- ggplot(data.frame(x = c(mu - 4 * sigma, mu + 4 * sigma)), aes(x)) +
    stat_function(fun = stats::dnorm,
                  args = list(mean = mu,sd = sigma),
                  geom = "area",
                  col = NA,
                  fill = "gray",
                  n = 1001) +
    stat_function(fun = stats::dnorm,
                  args = list(mean = mu,sd = sigma),
                  xlim = c(ifelse(below,mu - 4 * sigma,x),
                           ifelse(below,x,mu + 4 * sigma)),
                  geom = "area",
                  fill = shade_fill,
                  col = NA,
                  n = 1001) +
    theme_classic(base_family = "serif", base_size = text_size) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = seq(-4, 4) * sigma + mu)
  if (show_proportion) {
    p <-  p +
      annotate(
        "text",
        x,
        0.05 * stats::dnorm(mu, mu, sigma),
        label = paste("P(x<", dx, ")==", round(stats::pnorm(x, mu, sigma), 2)),
        parse = TRUE,
        hjust = 1.05,
        family = "serif",
        size = text_size * 5 / 14
      ) +
      annotate(
        "text",
        x,
        0.05 * stats::dnorm(mu, mu, sigma),
        label = paste("P(x>", dx, ")==", round(1 - stats::pnorm(x, mu, sigma), 2)),
        parse = TRUE,
        hjust = -0.05,
        family = "serif",
        size = text_size * 5 / 14
      )
  }

  if (show_x) {
    p <- p +
      annotate(
        geom = "text",
        x = x,
        y = 0,
        label = paste("x == ",dx),
        vjust = 1.1,
        parse = TRUE,
        family = "serif",
        size = text_size * 5 / 14)
  }

  if (show_param) {
    p <- p +
      annotate(
        geom = "text",
        x = mu,
        y = stats::dnorm(mu,mu,sigma),
        label = paste("mu==",mu),
        vjust = -.1,
        parse = TRUE,
        family = "serif",
        size = text_size * 5 / 14) +
      annotate(
        geom = "text",
        x = mu + sigma / 2,
        y = stats::dnorm(mu + sigma, mu, sigma),
        label = paste("sigma==", sigma),
        vjust = -.3,
        parse = TRUE,
        family = "serif",
        size = text_size * 5 / 14) +
      annotate(
        geom = "segment",
        x = mu,
        y = stats::dnorm(mu + sigma, mu, sigma),
        xend = mu + sigma,
        yend = stats::dnorm(mu + sigma, mu, sigma),
        arrow = arrow(length = unit(0.015, "npc")))

  }
  p
}


