
#' Function that converts pdf files to svg. Must have the pdf2svg program installed (https://github.com/dawbarton/pdf2svg)
#'
#' @export
#' @param s Filename of the pdf file. Omit the ".pdf" at the end.
pdf2svg <- function(s) {shell(paste0("pdf2svg ",s,".pdf ",s,".svg"))}


#' Computes covariances of composite scores given a covariance matrix and a weight matrix
#'
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
  if (correlation) stats::cov2cor(Sigma_composite) else Sigma_composite
  }

#' Plot a normal distribution shading below x
#'
#' @export
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
#' @examples
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




#' Creats a plot of factor loadings
#'
#' @export
#' @param f output of a factor analysis from the psych::fa function
#' @param factor_names names of the factors #' @import patchwork
#' @import ggplot2
#' @import patchwork
#' @importFrom rlang .data
#' @examples
#' library(psych)
#' fit <- fa(psych::bfi[,1:25], nfactors = 5)
#' plot_loading(fit)
plot_loading <- function(f, factor_names = sort(colnames(f$loadings))) {
  factor_names <- stringr::str_replace_all(factor_names, "_", "\n")

  d_factor_names <- tibble::tibble(Factor = colnames(f$loadings), Factor_labels = factor_names) %>%
    dplyr::mutate(Factor_labels = forcats::fct_inorder(.data$Factor_labels))
  p1 <- f %>%
    psych::fa.sort() %>%
    `[[`("loadings") %>%
    unclass() %>%
    tibble::as_tibble(rownames = "Test") %>%
    dplyr::mutate(Test = forcats::fct_infreq(.data$Test) %>% forcats::fct_rev()) %>%
    tidyr::gather(key = "Factor", value = "Loadings", -.data$Test) %>%
    dplyr::mutate(Test = forcats::fct_inorder(.data$Test) %>% forcats::fct_rev()) %>%
    dplyr::arrange(.data$Test) %>%
    dplyr::left_join(d_factor_names, by = "Factor") %>%
    ggplot(aes(.data$Factor_labels, .data$Test)) +
    geom_tile(aes(fill = .data$Loadings)) +
    geom_text(aes(label = formatC(.data$Loadings, 2, format = "f") %>%
                    stringr::str_remove("^0") %>%
                    stringr::str_replace("^-0", "-")),
              family = "serif") +
    scale_fill_gradient2(low = "firebrick", mid = "white", high = "royalblue", midpoint = 0, limits = c(-1,1)) +
    theme_minimal(base_family = "serif", base_size = 14) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none") +
    ggtitle(paste0("Factor Loadings", ifelse(any(abs(f$loadings) > 1), " (Heywood case detected)","")))

  if (f$factors == 1) p1 else {
    p2 <-  corrr::as_cordf(f$Phi, diagonal = 1) %>%
      dplyr::rename(Factor = .data$rowname) %>%
      tidyr::gather(key = "f", value = "r", -.data$Factor) %>%
      dplyr::left_join(d_factor_names, by = "Factor") %>%
      dplyr::mutate(Factor_col = forcats::fct_rev(.data$Factor_labels)) %>%
      dplyr::select(-.data$Factor_labels) %>%
      dplyr::left_join(
        d_factor_names %>%
          dplyr::rename(f = .data$Factor), by = "f") %>%
      dplyr::rename(Factor_row = .data$Factor_labels) %>%
      dplyr::mutate(Factor_col = forcats::fct_relabel(.data$Factor_col, stringr::str_replace_all, pattern = "\n", " ")) %>%
      ggplot(aes(.data$Factor_row, .data$Factor_col)) +
      geom_tile(aes(fill = .data$r)) +
      geom_text(aes(label = formatC(.data$r, 2, format = "f") %>%
                      stringr::str_remove("^0") %>%
                      stringr::str_replace("^-0", "-")),
                family = "serif") +
      scale_fill_gradient2(
        low = "firebrick",
        mid = "white",
        high = "royalblue",
        midpoint = 0,
        limits = c(-1,1)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL) +
      ggtitle("Factor Correlations")


    p1 + p2 + patchwork::plot_layout(ncol = 1, heights = c(nrow(f$loadings) / ncol(f$loadings), 1))
  }


}



#' Format text of a correlation matrix
#'
#' @export
#' @param r a matrix of correlations
#' @param digits Number of digits to round
#' @param remove_leading_zero If TRUE, remove leading zero from all correlations
#' @param remove_diagonal If TRUE, remove diagonal of ones
#' @return a matrix of correlations as text
#'
#' @examples
#' cor_text(matrix(.5,2,2))
cor_text <- function(
  r,
  digits = 2,
  remove_leading_zero = TRUE,
  remove_diagonal = FALSE) {
  rho <- formatC(r, digits = digits, format = "f")

  if (remove_leading_zero) {
    space_replace <- ifelse(any(r < 0)," ","")
    rho <- apply(rho, MARGIN = c(1,2), stringr::str_replace, pattern = "^0", replacement = space_replace)
    rho <- apply(rho, MARGIN = c(1,2), stringr::str_replace, pattern = "^-0", replacement = "-")
  }

  if (remove_diagonal) {
    diag(rho) <- ""
  } else {
    diag(rho) <- "1"
  }
rho
}



# Correlation plot
#' Title
#'
#' @export
#' @param d Data
#' @param test_names Vector of names of variables
#' @param margins Width of margins for labels
#' @param text_size Size of text
#' @param dendrograms If TRUE, add dendrograms
#' @param palette_col A vector of three colors for heatmap
#' @param x_label_angle Angle of x-axis label
#' @param reorder_vars If TRUE, reorder variables based on cluster analysis
#' @param heat.lim Vector of the lower and upper bounds on heat map
#' @param heat.pal.values Vector of values of where on the scale each color in palette_col falls. Defaults to even spacing.
#' @param ... Additional parameters passed to superheat::superheat
cor_heat <- function(
  d,
  test_names = colnames(d),
  margins = 0.55,
  text_size = 4,
  dendrograms = TRUE,
  palette_col = c("firebrick", "white", "royalblue"),
  x_label_angle = 90,
  reorder_vars = TRUE,
  heat.lim = c(-1, 1),
  heat.pal.values = seq(0,1, 1 / (length(palette_col) - 1)),
  ...) {
  r <- stats::cor(d, use = "pairwise")



  dimnames(r) <- list(test_names, test_names)

  superheat::superheat(
    X = r,
    X.text = cor_text(r),
    X.text.size = text_size,
    col.dendrogram = dendrograms & reorder_vars,
    row.dendrogram = dendrograms & reorder_vars,
    smooth.heat = F,
    heat.pal = palette_col,
    heat.lim = heat.lim,
    heat.pal.values = heat.pal.values,
    legend = F,
    bottom.label.text.angle = x_label_angle,
    heat.na.col = "white",
    bottom.label.size = margins,
    left.label.size = margins,
    left.label.text.size = text_size,
    bottom.label.text.size = text_size,
    padding = 0,
    pretty.order.rows = reorder_vars,
    pretty.order.cols = reorder_vars,
    ...
  )
}

#' APA p-value rounding
#'
#' @param p probabiity
#' @param inline to be used in an inline rmarkdown (default is FALSE)
#' @param mindigits minimum rounding digits
#' @param maxdigits maximum rounding digits
#'
#' @return character
#' @export
#'
#' @examples
#' pvalueAPA(0.01111)

pvalueAPA <- function(p, inline = FALSE, mindigits = 2, maxdigits = 3){
  p.round <- ifelse(p > 0.5 * 10 ^ (-1 * mindigits),mindigits,maxdigits)
  if (p > 0.5 * 10 ^ (-1 * p.round)) {
    paste0(ifelse(inline,"$p=", ""),
           sub(pattern = "0.",
               replacement = ".",
               formatC(p, p.round, format = "f")),
           ifelse(inline,"$", ""))
  } else {
    paste0(ifelse(inline, "$p<","<"),
           sub(pattern = "0.",
               replacement =  ".",
               10 ^ (-1 * maxdigits)),
           ifelse(inline,"$",""))
  }
}

#' Rounds proportions to significant digits both near 0 and 1
#'
#' @param p probabiity
#' @param digits rounding digits
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' proportion_round(0.01111)

proportion_round <- function(p, digits = 2) {
  p1 <- round(p, digits)
  lower_limit <- 0.95 * 10 ^ (-1 * digits)
  upper_limit <- 1 - lower_limit
  p1[p > upper_limit & p <= 1] <- 1 - signif(1 - p[p > upper_limit & p <= 1], digits - 1)
  p1[p < lower_limit & p >= 0] <- signif(p[p < lower_limit & p >= 0], digits - 1)
  p1
}

#' Rounds proportions to significant digits both near 0 and 1, then converts to percentiles
#'
#' @param p probability
#' @param digits rounding digits
#' @param remove_leading_zero If TRUE, remove leading zero
#' @param add_percent_character If TRUE, add percent character
#'
#' @return character vector
#' @export
#'
#' @examples
#' proportion2percentile(0.011111)

proportion2percentile <- function(p,
                                  digits = 2,
                                  remove_leading_zero = TRUE,
                                  add_percent_character = FALSE) {
  p1 <- as.character(100 * proportion_round(p, digits = digits))
  if (remove_leading_zero) {
    p1 <- remove_leading_zero(p1)
  }

  if (add_percent_character) {
    p1 <- paste0(p1,"%")
  }

  p1

}


#' Random beta distribution with specified mean and sd
#'
#' @param n Number of data points
#' @param mu Mean of random beta distribution
#' @param sigma SD of random beta distribution
#'
#' @return a vector of numeric values
#' @export
#'
#' @examples
#' rbeta_ms(n = 5, mu = 0.8, sigma = 0.1)
rbeta_ms <- function(n = 1,
                     mu = 0.5,
                     sigma = 0.025) {
  if (sigma == 0)
    return(rep(mu, n))

  # Check to make sure mu is between 0 and 1
  if (mu >= 1 | mu <= 0)
    stop("mu must be between 0 and 1.")
  # variance
  v <- sigma ^ 2
  # Check to make sure the variance is not impossibly large
  if (v > mu * (1 - mu))
    stop("sigma is too large. sigma cannot be larger than mu * (1 - m).")

  # Hits
  a <- mu * ((mu * (1 - mu) / v) - 1)
  # Misses
  b <- (1 - mu) * a / mu
  # Random data
  stats::rbeta(n, a, b)
}


#' Make latent variable indicators with rbeta_ms function
#'
#' @param latent name of latent variable
#' @param indicators vector of indicator names (assigned automatically if left NULL)
#' @param mu mean of standardized coefficients
#' @param sigma sd of standardized coeficients
#' @param k number of indicator variables
#' @param digits number of digits to round coefficients
#'
#' @return lavaan code for latent variable assignment
#' @export
#'
#' @examples
#' make_indicators("depression", mu = 0.8, sigma = 0.05, k = 4)
make_indicators <- function(latent, indicators = NULL, mu = 0.8, sigma = 0.05, k = 3, digits = 3) {
  if (is.null(indicators)) indicators <- paste0(latent,"_",1:k)
  if (length(mu) == 1) mu <- rep(mu, length(indicators))
  if (length(sigma) == 1) sigma <- rep(sigma, length(indicators))

  loadings <- round(purrr::pmap_dbl(list(mu = mu,sigma = sigma), rbeta_ms), digits = digits)
  ll <- paste0(loadings," * ", indicators, collapse = " + ")
  paste0(latent, " =~ ", ll)
}


#' ggplot of parallel analysis from the psych package
#'
#' @param d data to be analyzed
#' @param fm factor method passed to psych::fa.parallel
#' @param vcolors vector of 2 colors for lines
#' @param ... parameters passed to psych::fa.parallel
#' @importFrom rlang .data
#' @import ggplot2
#' @export
#'
#' @examples
#' d <- psych::bfi[,1:25]
#' parallel_analysis(d)
parallel_analysis <- function(d, fm = "pa", vcolors = c("firebrick", "royalblue"), ...) {
  invisible(utils::capture.output( pa <- psych::fa.parallel(d, fm = fm, plot = F, ...)))
  x <- pa$nfact
  y <- pa$fa.values[pa$nfact]
  tibble::tibble(
    `Observed Data` = pa$fa.values,
    `Simulated Data` = pa$fa.sim,
    Factors = seq_along(pa$fa.values)
  ) %>%
    tidyr::gather("Type", "Eigenvalues",-.data$Factors) %>%
    ggplot(aes(.data$Factors,
               .data$Eigenvalues,
               color = .data$Type)) +
    geom_line() +
    geom_point() +
    theme_minimal(base_family = "serif") +
    scale_color_manual(NULL, values = vcolors) +
    scale_x_continuous("Eigenvalues", minor_breaks = NULL, breaks = seq_along(pa$fa.values)) +
    scale_y_continuous("Factors") +
    theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = "white", color = NA)
    ) +
    annotate(
      "text",
      x = x + 0.05,
      y = y,
      label = "Last observed eigenvalue above simulated data",
      hjust = 0,
      vjust = -0.5,
      family = "serif"
    ) +
    annotate("point",
             x = x,
             y = y,
             size = 2) +
    ggtitle(paste0("Parallel analysis suggests ", x, " factor",ifelse(x == 1, "","s"),"."))
}

#' Convert x to a z-score
#'
#' @param x a numeric vector
#' @param mu mean
#' @param sigma standard deviation
#' @export
#'
x2z <- function(x, mu = mean(x, na.rm = T), sigma = stats::sd(x, na.rm = T)) {
  (x - mu) / sigma
}

#' Converts the default values of a function's arguments to variables and attaches them to the global environment
#'
#' @export
#' @param f Function
#' @return Attaches function arguments to global environment
#' @export
#'
#' @examples
#' my_function <- function(x, y = 2) x + y
#'
#' # Sets y to 2
#' attach_function(my_function)
attach_function <- function(f) {
  attach(rlist::list.clean(as.list(formals(f)),is.name))
}


#' Remove leading zero from numbers
#'
#' @param x vector of  numbers
#' @param digits rounding digits
#'
#' @return vector of characters
#' @export
#'
#' @examples
#' remove_leading_zero(c(0.5,-0.2))
remove_leading_zero <- function(x, digits = NA) {
  if (!is.na(digits)) {
    x <- formatC(x , digits = digits, format = "f")
  }
  sub("^-0+","-", sub("^0+","",x))
}

#' Convert data.frame and tibbles to matrices with named rows and columns
#'
#' @param d data.frame or tibble
#' @param first_col_is_row_names TRUE if first column has row names
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' d <- data.frame(rname = c("x1", "x2"), x1 = c(1,2), x2 = c(3,4))
#' df2matrix(d)
df2matrix <- function(d, first_col_is_row_names = TRUE) {
  d <- as.data.frame(d)
  if (first_col_is_row_names) {
    rownames(d) <- dplyr::pull(d, 1)
    d <- d[,-1, drop = F]
  }
  as.matrix(d, rownames.force = T)
}





