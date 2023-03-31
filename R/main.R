
#' Function that converts pdf files to svg. Must have the pdf2svg program installed (https://github.com/dawbarton/pdf2svg)
#'
#' @export
#' @param f Filename of the pdf file. Omit the ".pdf" at the end.
pdf2svg <- function(f) {shell(paste0("pdf2svg ",f,".pdf ",f,".svg"))}


#' Save ggplot as .pdf, then convert to .svg via pdf2svg
#'
#' @param f Filename of the svg file. Omit the ".svg" at the end.
#' @param width width passed to ggplot2::ggsave
#' @param height width passed to ggplot2::ggsave
#' @param ... Additional parameters passed to ggplot2::ggsave
#' @export
ggsvg <- function(f, width = 6.5, height = 6.5, ...) {
  ggplot2::ggsave(
    filename = paste0(f, ".pdf"),
    device = grDevices::cairo_pdf,
    width = width,
    height = height,
    ...
  )
  pdf2svg(f)
  file.show(paste0(f, ".svg"))
}




#' Plot a normal distribution shading below x
#'
#' @export
#' @param x number to divide normal distribution
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#' @param below If TRUE, shade lower portion of normal distribution
#' @param show_proportion If TRUE, display proportions
#' @param show_x If TRUE, display x value
#' @param show_param If TRUE, display mean and standard deviation
#' @param text_size Base text size
#' @param font_family Name of font
#' @param shade_fill Color of shaded region
#' @import ggplot2
#' @examples
#' plotnorm(90, 100, 15)
plotnorm <- function(x = 0,
                     mu = 0,
                     sigma = 1,
                     below = TRUE,
                     show_proportion = TRUE,
                     show_x = TRUE,
                     show_param = TRUE,
                     text_size = 14,
                     font_family = "sans",
                     shade_fill = "royalblue") {
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
    theme_classic(base_family = font_family,
                  base_size = text_size,
                  base_line_size = .25) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = seq(-4, 4) * sigma + mu, labels = label_parsed)
  if (show_proportion) {
    p <-  p +
      annotate(
        "text",
        x,
        0.05 * stats::dnorm(mu, mu, sigma),
        label = paste("P(italic(x)<", dx, ")=='", prob_label(stats::pnorm(x, mu, sigma)), "'"),
        parse = TRUE,
        hjust = 1.05,
        family = font_family,
        size = text_size * 5 / 14
      ) +
      annotate(
        "text",
        x,
        0.05 * stats::dnorm(mu, mu, sigma),
        label = paste("P(italic(x)>", dx, ")=='", prob_label(1 - stats::pnorm(x, mu, sigma)), "'"),
        parse = TRUE,
        hjust = -0.05,
        family = font_family,
        size = text_size * 5 / 14
      )
  }

  if (show_x) {
    p <- p +
      annotate(
        geom = "text",
        x = x,
        y = 0,
        label = paste("italic(x) == ",dx),
        vjust = 1.1,
        parse = TRUE,
        family = font_family,
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
        family = font_family,
        size = text_size * 5 / 14) +
      annotate(
        geom = "text",
        x = mu + sigma / 2,
        y = stats::dnorm(mu + sigma, mu, sigma),
        label = paste("sigma==", sigma),
        vjust = -.3,
        parse = TRUE,
        family = font_family,
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
#' @param font_family Name of font
#' @param font_size Size of font
#' @param loading_text_size size of loading font,
#' @param factor_names names of the factors #'
#' @param nudge_loadings nudge loadings on x dimension
#' @import patchwork
#' @import ggplot2
#' @import patchwork
#' @importFrom rlang .data
#' @examples
#' library(GPArotation)
#' library(psych)
#' fit <- fa(psych::bfi[,1:25], nfactors = 5)
#' plot_loading(fit)
plot_loading <- function(f,
                         font_family = "sans",
                         font_size = 14,
                         loading_text_size = font_size * .8,
                         factor_names = sort(colnames(f$loadings)),
                         nudge_loadings = 0.05) {
  factor_names <- stringr::str_replace_all(factor_names, "_", "\n")

  d_factor_names <- tibble::tibble(Factor = colnames(f$loadings),
                                   Factor_labels = factor_names) %>%
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
    geom_text(aes(label = prob_label(.data$Loadings)),
              family = font_family,
              hjust = 1,
              nudge_x = nudge_loadings,
              size = ggtext_size(loading_text_size, ratio = 1)) +
    scale_fill_gradient2(low = "firebrick", mid = "white", high = "royalblue", midpoint = 0, limits = c(-1,1)) +
    scale_x_discrete(expand = expansion()) +
    theme_minimal(base_family = font_family, base_size = font_size) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none", panel.grid = element_blank(), plot.title = element_text(family = font_family, size = font_size)) +
    ggtitle(paste0("Factor Loadings", ifelse(any(abs(f$loadings) > 1), " (Heywood case detected)","")))

  if (f$factors == 1) p1 else {
    p2 <-  corrr::as_cordf(f$Phi, diagonal = 1) %>%
      dplyr::rename(Factor = .data$term) %>%
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
      geom_text(aes(label = prob_label(.data$r)),
                family = font_family,
                size = ggtext_size(loading_text_size, ratio = 1),
                hjust = 1,
                nudge_x = nudge_loadings) +
      scale_fill_gradient2(
        low = "firebrick",
        mid = "white",
        high = "royalblue",
        midpoint = 0,
        limits = c(-1,1)) +
      scale_x_discrete(expand = expansion()) +
      theme_minimal(base_family = font_family, base_size = font_size) +
      theme(legend.position = "none", panel.grid = element_blank(), plot.title = element_text(family = font_family, size = font_size)) +
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



#' Correlation plot
#'
#' @export
#' @param d Data or correlation matrix
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

  if (is.matrix(d)) {
    if (isSymmetric(d)) {
      r <- d
    }
    } else {
    r <- stats::cor(d, use = "pairwise")}



  dimnames(r) <- list(test_names, test_names)

  superheat::superheat(
    X = r,
    X.text = prob_label(r),
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
    p1 <- stringr::str_remove(p1, "^0")
  }

  if (add_percent_character) {
    p1 <- paste0(p1,"%")
  }

  stringr::str_remove_all(p1, " ")

}


#' Format numeric probabilities as text labels
#'
#' @param p numeric vector of probabilities
#' @param accuracy accuracy of rounding
#' @param digits Optional. Number of digits to round. Overrides accuracy parameter
#' @param max_digits Optional. Maximum zeros or nines before rounding to 0 or 1
#' @param remove_leading_zero Removes leading zero from probability
#' @param round_zero_one Apply rounding to 0 and 1
#'
#' @return character vector
#' @export
#'
#' @examples
#' prob_label(seq(0,1, 0.1))
prob_label <- function(p,
                       accuracy = 0.01,
                       digits = NULL,
                       max_digits = NULL,
                       remove_leading_zero = TRUE,
                       round_zero_one = TRUE) {
  if (is.null(digits)) {
    l <- scales::number(p, accuracy = accuracy)
  } else {
    sig_digits <- abs(ceiling(log10(p + p / 1000000000)) - digits)
    pgt99 <- p > 0.99
    sig_digits[pgt99] <- abs(ceiling(log10(1 - p[pgt99])) - digits + 1)
    sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] <- sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] - 1
    sig_digits[is.infinite(sig_digits)] <- 0
    l <- purrr::map2_chr(p,
                         sig_digits,
                         formatC,
                         format = "f",
                         flag = "#")

  }
  if (remove_leading_zero) l <- sub("^-0","-", sub("^0","", l))

  if (round_zero_one) {
    l[p == 0] <- "0"
    l[p == 1] <- "1"
    l[p == -1] <- "-1"
  }

  if (!is.null(max_digits)) {
    if (round_zero_one) {
      l[round(p, digits = max_digits) == 0] <- "0"
      l[round(p, digits = max_digits) == 1] <- "1"
      l[round(p, digits = max_digits) == -1] <- "-1"
    } else {
      l[round(p, digits = max_digits) == 0] <- paste0(".", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == 1] <- paste0("1.", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == -1] <- paste0("-1.", paste0(rep("0", max_digits), collapse = ""))
    }
  }

  l <- sub(pattern = "-", replacement = "\u2212", x = l)
  Encoding(l) <- "UTF-8"

  dim(l) <- dim(p)
  l
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
  if (any(sigma == 0))
    return(rep(mu, n))

  # Check to make sure mu is between 0 and 1
  if (any(mu >= 1) | any(mu <= 0))
    stop("mu must be between 0 and 1.")
  # variance
  v <- sigma ^ 2
  # Check to make sure the variance is not impossibly large
  if (any(v > mu * (1 - mu)))
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
#' @param factor_based TRUE is factor-based and FALSE is principal component-based
#' @param vcolors vector of 2 colors for lines
#' @param font_family Name of font
#' @param ... parameters passed to psych::fa.parallel
#' @importFrom rlang .data
#' @import ggplot2
#' @export
#'
#' @examples
#' d <- psych::bfi[,1:25]
#' parallel_analysis(d)
parallel_analysis <- function(d,
                              fm = "pa",
                              factor_based = TRUE,
                              vcolors = c("firebrick", "royalblue"),
                              font_family = "sans",
                              ...) {
  invisible(utils::capture.output( pa <- psych::fa.parallel(d, fm = fm, plot = F, ...)))
  if (factor_based) {
    x <- pa$nfact
    y <- pa$fa.values[pa$nfact]
    df <- tibble::tibble(`Observed Data` = pa$fa.values,
                 `Simulated Data` = pa$fa.sim,
                 Factors = seq_along(pa$fa.values))
  } else {
    x <- pa$ncomp
    y <- pa$pc.values[pa$ncomp]
    df <- tibble::tibble(`Observed Data` = pa$pc.values,
                 `Simulated Data` = pa$pc.sim,
                 Factors = seq_along(pa$pc.values))
    }



  df %>%
    tidyr::gather("Type", "Eigenvalues",-.data$Factors) %>%
    ggplot(aes(.data$Factors,
               .data$Eigenvalues,
               color = .data$Type)) +
    geom_line() +
    geom_point() +
    theme_minimal(base_family = font_family) +
    scale_color_manual(NULL, values = vcolors) +
    scale_x_continuous("Factors", minor_breaks = NULL, breaks = seq_along(pa$fa.values)) +
    scale_y_continuous("Eigenvalues") +
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
      family = font_family
    ) +
    annotate("point",
             x = x,
             y = y,
             size = 2) +
    ggtitle(paste0("Parallel analysis (based on ",ifelse(factor_based, "factor analysis", "principal component analysis"),") suggests ", x, " factor",ifelse(x == 1, "","s"),"."))
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
  args <- rlist::list.clean(as.list(formals(f)),is.name)
  purrr::walk2(names(args), args, assign, envir = .GlobalEnv)
}


#' Remove leading zero from numbers
#'
#' @param x vector of  numbers
#' @param digits rounding digits
#' @param ... Arguments passed to formatC
#' @return vector of characters
#' @export
#'
#' @examples
#' remove_leading_zero(c(0.5,-0.2))
remove_leading_zero <- function(x, digits = 2, ...) {
  x <- formatC(x, digits, format = "f", ...)
  sub("^-0","-", sub("^0","",x))
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


#' Convert ggplot theme font size to geom_text size
#'
#' @param base_size theme font size
#' @param ratio ratio of text size to theme font size. Defaults to .8 so that geom text will be the same size as default sized axis labels.
#'
#' @return numeric vector
#' @export
#'
#' @examples
#'
#' ggtext_size(16)
ggtext_size <- function(base_size, ratio = 0.8) {
  ratio * base_size / 2.845276
}


#' Paste matrix code from clipboard
#'
#' @param digits Number of digits to round
#' @param as_matrix Convert to matrix. Defaults to `TRUE`
#' @export
paste_matrix_from_clipboard <- function(digits = 2, as_matrix = TRUE) {
  x <- readr::read_tsv(I(utils::readClipboard()))
  rn <- colnames(x)[1]
  x <- x %>%
    dplyr::mutate(
      dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                    .fns = formatC,
                    digits = digits,
                    format = "f")) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    apply(1, format) %>%
    apply(1, paste, collapse = "\t") %>%
    paste0(collapse = "\n")

    my_matrix <- paste0('"\n',
           x,
           '" %>%\n\treadr::read_tsv()',
           ifelse(as_matrix,
                  paste0(" %>%\n\ttibble::column_to_rownames(\"",
                         rn,
                         "\") %>%\n\tas.matrix()"),
                  ""))

  rstudioapi = rstudioapi::insertText(my_matrix)
}

#' Make latex array
#'
#' @param M A matrix
#' @param left left delimiter options: [(|
#' @param right right delimiter options: ])|
#' @param env Array environment
#' @param includenames Include column and row names
#' @param align Column alignment. For a three column matrix, alignment defaults to centering columns("ccc"). If there are row labels, the default would be "r|ccc" to right-align the row labels and separate them with vertical line.
#' @param lines Include lines separating column and row names
#' @return character string
#' @export
#' @examples
#' M <- diag(3)
#' colnames(M) <- LETTERS[1:3]
#' rownames(M) <- LETTERS[1:3]
#' latexarray(M)
latexarray <- function(M,
                       left = "",
                       right = "",
                       env = "array",
                       includenames = TRUE,
                       align = NULL,
                       lines = TRUE) {

    if (is.null(align)) newalign <- paste0(rep("c", ncol(M)), collapse = "") else newalign = align

    if (includenames) {
      if (!is.null(colnames(M))) {
        M <- rbind(colnames(M), M)
      }

      if (!is.null(rownames(M))) {
        M <- cbind(rownames(M), M)
        if (lines & is.null(align)) newalign <- paste0("r|", newalign)
      }

      if (!is.null(colnames(M)) & lines) {
        # M[1, 1] <- paste0("\\hline ", M[1, 1])
        M[2, 1] <- paste0("\\hline ", M[2, 1])
      }

    }
    strM <-  paste(apply(
      M,
      MARGIN = 1,
      FUN = paste0,
      collapse = " & "
    ),
    collapse = "\\\\\n")

    # if (includenames & !is.null(colnames(M)) & lines) {
    #   strM <- paste0(strM, "\\\\\n\\hline")
    # }

    if (nchar(left) > 0 | is.na(left) | is.null(left)) {
      left <- paste0("\\left", left)
      }
    if (nchar(right) > 0 | is.na(right) | is.null(right)) {
      right <- paste0("\\right", right)
      }


    if (!is.null(env)) {
      strM <- paste0(
        left,
        "\\begin{",
        env,
        "}{",
        newalign,
        "}\n",
        strM,
        "\n\\end{",
        env ,
        "}",
        right
      )
    }
    strM
  }


#' Insert latex colors into a matrix
#'
#' @param m matrix of values
#' @param color_cells matrix of latex colors for matrix cells
#' @param color_rownames vector of latex colors for row names
#' @param color_colnames vector of latex colors for column names
#'
#' @return matrix
#' @export
#'
#' @examples
#' # A matrix of zeros
#' m <- matrix(0, nrow = 2, ncol = 2)
#'
#' # A matrix of NA values the same size as m
#' latex_colors <- m * NA
#'
#' # Make the top row red
#' latex_colors[1,] <- "red"
#'
#' # Insert colors into m
#' insert_latex_color(m, latex_colors)
insert_latex_color <- function(m,
                               color_cells,
                               color_rownames = NULL,
                               color_colnames = NULL) {
  m[!is.na(color_cells)] <- paste0("\\color{",
                          color_cells[!is.na(color_cells)],
                          "}{",
                          m[!is.na(color_cells)],
                          "}")
  if (is.matrix(m)) {
    if (!is.null(color_colnames) & !is.null(colnames(m))) {
      colnames(m)[!is.na(color_colnames)] <- paste0(
        "\\color{",
        color_colnames[!is.na(color_colnames)],
        "}{",
        colnames(m)[!is.na(color_colnames)],
        "}")
    }

    if (!is.null(color_rownames) & !is.null(rownames(m))) {
      rownames(m)[!is.na(color_rownames)] <- paste0(
        "\\color{",
        color_rownames[!is.na(color_rownames)],
        "}{",
        rownames(m)[!is.na(color_rownames)],
        "}")
    }
  }

  m
}


#' Create unique combinations of vectors
#'
#' @param ... vectors
#' @param sep Separate character
#'
#' @return A character vector
#' @export
#'
#' @examples
#' cross_vectors(c("a", "b"), c(1,2))
cross_vectors <- function(..., sep = "_") {
  tidyr::expand_grid(...) %>%
    apply(1,
          paste0,
          collapse = sep)
}


#' @title Simple slopes plot
#' @description generates simple slopes plot from moderated regression equation
#' @param predictor_range a length 2 vector of the range of values to be plotted on the predictor variable, Default: c(-4, 4)
#' @param moderator_values a vector of moderator values to be plotted, Default: c(-1, 0, 1)
#' @param intercept the intercept of the regression equation, Default: 0
#' @param predictor_coef the regression coefficient for the predictor variable, Default: 0
#' @param moderator_coef the regression coefficient for the moderator variable, Default: 0
#' @param interaction_coef the interaction term coefficent, Default: 0
#' @param predictor_label the label for the predictor variable, Default: 'X'
#' @param criterion_label the label for the moderator variable, Default: 'Y'
#' @param moderator_label PARAM_DESCRIPTION, Default: 'Moderator'
#' @return a ggplot of the simple slopes
#' @examples
#' modregplot(
#'   predictor_range = c(-2, 2),
#'   moderator_values = c(Low = -1, High = 1),
#'   intercept = 6,
#'   predictor_coef = 2,
#'   moderator_coef = 0,
#'   interaction_coef = 1,
#'   predictor_label = "Psychopathy",
#'   criterion_label = "Aggression",
#'   moderator_label = "Impulsivity"
#' )
#' @rdname modregplot
#' @export
modregplot <- function(predictor_range = c(-4,4),
                       moderator_values = c(-1,0,1),
                       intercept = 0,
                       predictor_coef = 0,
                       moderator_coef = 0,
                       interaction_coef = 0,
                       predictor_label = "X",
                       criterion_label = "Y",
                       moderator_label = "Moderator") {
  d <- tidyr::crossing(x = predictor_range,
                       m = moderator_values)
  d <- dplyr::mutate(
    d,
    xm = .data$x * .data$m,
    yhat = intercept +
      .data$x * predictor_coef +
      .data$m * moderator_coef +
      .data$xm * interaction_coef
  )

  if (is.null(names(moderator_values))) {
    d <- dplyr::mutate(
      d,
      m = factor(.data$m)
    )
  } else {
    d <- dplyr::mutate(
      d,
      m = factor(.data$m,
                 levels = moderator_values,
                 labels = names(moderator_values))
    )
  }


  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$yhat, color = .data$m)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = predictor_label,
                  y = criterion_label,
                  color = moderator_label)

}

#' Probability a true score will be below a threshold
#'
#' @param x observed score
#' @param threshold threshold
#' @param rxx reliability coefficient
#' @param mu population mean
#' @param sigma population standard deviation
#'
#' @return probability
#' @export
#'
#' @examples
#' pthreshold(x = .5, threshold = 1, rxx = 0.9)
pthreshold <- function(x, threshold, rxx, mu = 0, sigma = 1) {
  est_true <- (x - mu) * rxx + mu
  see <- sigma * sqrt(rxx - rxx ^ 2)
  z <- (threshold - est_true) / see
  stats::pnorm(z)
}



#' Regression from correlation matrix
#'
#' @param R correlation matrix
#' @param ind independent variables
#' @param dep dependent variable
#'
#' @return list of coefficients and R2
#' @export
lm_matrix <- function(R, ind, dep) {
  R <- R[c(dep, ind), c(dep, ind), drop = F]
  Rxx <- R[ind, ind, drop = F]
  iR <- solve(Rxx)
  Rxy <- R[ind, dep, drop = F]
  b <- iR %*% Rxy
  R2 <- 1 - det(R) / det(Rxx)
  sr2 <- (b ^ 2) / diag(iR)
  list(b = b, R2 = R2, sr2 = sr2)
}


#' Create square correlation matrix from lower triangle
#'
#' @param x vector of correlations
#'
#' @return square matrix
#' @export
#'
#' @examples
#' tri2cor(c(.2,.3,.4))
tri2cor <- function(x) {
  l <- length(x)
  n <- (1 + sqrt(1 + 8 * l)) / 2
  if (n != round(n)) stop("Not a square matrix")
  R <- matrix(0, n, n)
  R[lower.tri(R)] <- x
  R <- R + t(R)
  diag(R) <- 1
  R
}


#' Generate correlation ellipse data
#'
#' @param r Correlation coefficient
#' @param mean Vector of 2 means
#' @param sd Vector of 2 standard deviations
#' @param p Proportion of data ellipse covers
#' @param split_x Split ellipse at x value
#' @param split_y Split ellipse at y value
#' @param n_points Number of points
#'
#' @return data.frame
#' @export
#'
#' @examples
#' cor_ellipse(r = .75)
cor_ellipse <- function(r = 0,
                        mean = c(0,0),
                        sd = c(1,1),
                        p = 0.95,
                        split_x = NULL,
                        split_y = NULL,
                        n_points = 10000) {

  sigma <- diag(sd) %*% matrix(c(1,r,r,1),2,2) %*% diag(sd)
  eigen.info <- eigen(sigma)

  theta <- atan(eigen.info$vectors[1] / eigen.info$vectors[2])
  lengths <- sqrt(eigen.info$values * stats::qchisq(p, 2))
  a = lengths[1]
  b = lengths[2]
  t = seq(0,2 * pi, length.out = n_points)

  d <- data.frame(
    y = a * cos(theta) * cos(t) - b * sin(theta) * sin(t) + mean[2],
    x = a * sin(theta) * cos(t) + b * cos(theta) * sin(t) + mean[1]
  )

  if (!is.null(split_x) & !is.null(split_y)) {
    lx <- d$x < split_x
    ly <- d$y < split_y
    ux <- d$x >= split_x
    uy <- d$y >= split_y

    d[ux & uy, "group"] <- "1"
    d[lx & uy, "group"] <- "2"
    d[ux & ly, "group"] <- "4"
    d[lx & ly, "group"] <- "3"

    d1 <- d
    d1[d1$group != "1", "x"] <- min(d1[d1$group == "1", "x"])
    d1[d1$group != "1", "y"] <- min(d1[d1$group == "1", "y"])
    d1$group = "1"
    d1 <- unique(d1)

    d2 <- d
    d2[d2$group != "2", "x"] <- max(d2[d2$group == "2", "x"])
    d2[d2$group != "2", "y"] <- min(d2[d2$group == "2", "y"])
    d2$group = "2"
    d2 <- unique(d2)

    d3 <- d
    d3[d3$group != "3", "x"] <- max(d3[d3$group == "3", "x"])
    d3[d3$group != "3", "y"] <- max(d3[d3$group == "3", "y"])
    d3$group = "3"
    d3 <- unique(d3)

    d4 <- d
    d4[d4$group != "4", "x"] <- min(d4[d4$group == "4", "x"])
    d4[d4$group != "4", "y"] <- max(d4[d4$group == "4", "y"])
    d4$group = "4"
    d4 <- unique(d4)
    d <- rbind(d1,d2,d3, d4)
  }


  if (!is.null(split_x) & is.null(split_y)) {
    d[,"group"] <- as.character((d$x < split_x) + 1)
  }

  if (is.null(split_x) & !is.null(split_y)) {
    d[,"group"] <- as.character((d$y < split_y) + 1)
  }

  d
}

wisc_SS <- c(
  vci = 130,
  vsi = 130,
  fri = 70,
  wmi = 130,
  psi = 130
)

rxx_wisc <- c(
  vci = .92,
  vsi = .92,
  fri = .93,
  wmi = .92,
  psi = .88
)



#' General a multivariate confidence interval for a set of scores
#'
#' @param x a vector of scores
#' @param rxx a vector reliability coefficients
#' @param mu  a vector means
#' @param sigma a covariance matrix
#' @param ci confidence level
#' @param v_names a vector of names
#'
#' @return data.frame
#' @export
#'
#' @examples
#' x_wisc <- c(
#'   vci = 130,
#'   vsi = 130,
#'   fri = 70,
#'   wmi = 130,
#'   psi = 130
#' )
#' rxx_wisc <- c(
#'   vci = .92,
#'   vsi = .92,
#'   fri = .93,
#'   wmi = .92,
#'   psi = .88
#'   )
#' R_wisc <- ("
#'   index	vci 	vsi 	fri 	wmi 	psi
#'   vci  	1.00	0.59	0.59	0.53	0.30
#'   vsi  	0.59	1.00	0.62	0.50	0.36
#'   fri  	0.59	0.62	1.00	0.53	0.31
#'   wmi  	0.53	0.50	0.53	1.00	0.36
#'   psi  	0.30	0.36	0.31	0.36	1.00") |>
#'     readr::read_tsv() |>
#'     tibble::column_to_rownames("index") |>
#'     as.matrix()
#'  multivariate_ci(
#'    x = x_wisc,
#'    rxx = rxx_wisc,
#'    mu = rep(100, 5),
#'    sigma = R_wisc * 225
#'  )
multivariate_ci <- function(x, rxx, mu, sigma, ci = .95, v_names = names(x)) {
  v_observed <- paste0(v_names, "_observed")
  v_true <- paste0(v_names, "_true")
  v_all <- c(v_true, v_observed)
  sigma_true <- `diag<-`(sigma , rxx * diag(sigma))
  sigma_all <- `dimnames<-`(rbind(cbind(sigma_true, sigma_true),
                                  cbind(sigma_true, sigma)),
                            list(v_all,
                                 v_all))
  mu_univariate = rxx * (x - mu) + mu

  lower_p <- (1 - ci) / 2
  upper_p <- 1 - lower_p

  mu_conditional <- mu + sigma_true %*% solve(sigma) %*% (x - mu)
  sigma_conditional <-
    sigma_true - sigma_true %*% solve(sigma) %*% t(sigma_true)
  see_univariate <- sqrt(diag(sigma) * (rxx - rxx ^ 2))
  see_multivariate <- sqrt(diag(sigma_conditional))

  data.frame(
    score = v_names,
    x = x,
    rxx = rxx,
    mu_univariate = mu_univariate,
    see_univariate = see_univariate,
    mu_multivariate = mu_conditional,
    see_multivariate = see_multivariate,
    upper_univariate = stats::qnorm(upper_p, mu_univariate, see_univariate),
    lower_univariate = stats::qnorm(lower_p, mu_univariate, see_univariate),
    upper_multivariate = stats::qnorm(upper_p, mu_conditional, see_multivariate),
    lower_multivariate = stats::qnorm(lower_p, mu_conditional, see_multivariate)
  )


}



#' Convert angles to ggplot2 vjust
#'
#' @param theta angle in radians
#' @param multiplier distance from point
#' @param as_degrees use degrees instead of radians
#'
#' @return numeric
#' @export
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' library(dplyr)
#' xy_ratio <- pi
#' tibble(theta = seq(0,2*pi, length.out = 9),
#'        y =  sin(theta),
#'        slope = cos(theta) * xy_ratio,
#'        text_angle = atan(slope) + pi / 2) %>%
#'    ggplot(aes(theta,y)) +
#'      stat_function(fun = sin) +
#'      geom_point() +
#'      geom_label(aes(label = LETTERS[1:9],
#'                     vjust = angle2vjust(text_angle, multiplier = 1.5),
#'                     hjust = angle2hjust(text_angle, multiplier = 1.5)),
#'                 label.size = NA,
#'                 label.padding = unit(1, "mm")) +
#'   scale_x_continuous(expression(theta),
#'                      breaks = seq(0,2*pi, length.out = 9),
#'                     labels = label_parsed(c("0", "frac(pi,4)", "frac(pi,2)",
#'                                              "frac(3 * pi,4)", "pi", "frac(5*pi,4)",
#'                                              "frac(3 * pi,2)", "frac(7*pi,4)", "2*pi"))) +
#'   scale_y_continuous(expression(sin(theta))) +
#'   coord_fixed(ratio = xy_ratio, clip = "off") +
#'   theme_minimal()
angle2vjust <- function(theta, multiplier = 1.5, as_degrees = FALSE) {
  if (as_degrees) theta <- theta * pi / 180
  (((sin(theta + pi) + 1) / 2) - 0.5) * multiplier + 0.5
}


#' Convert angles to ggplot2 hjust
#'
#' @param theta angle in radians
#' @param multiplier distance from point
#' @param as_degrees use degrees instead of radians
#'
#' @return numeric
#' @export
angle2hjust <- function(theta, multiplier = 1.5, as_degrees = FALSE) {
  if (as_degrees) theta <- theta * pi / 180
  (((cos(theta + pi) + 1) / 2) - 0.5) * multiplier + 0.5
}


#' Rotate a 2-column matrix
#'
#' @param x a 2-column matrix
#' @param theta angle
#' @param degrees if TRUE, theta is in degrees instead of radians
#' @param origin point of rotation
#'
#'
#' @return a rotated 2-column matrix
#' @export
#'
#' @examples
#' x <- matrix(seq(10), ncol = 2)
#' rotate2dmatrix(x, pi)
rotate2dmatrix <- function(x, theta, degrees = FALSE, origin = c(0,0)) {

  if (!is.numeric(x)) stop("x must be numeric")
  if (!is.numeric(origin)) stop("origin must be numeric")

  if ("matrix" %in% class(x)) {
    if (ncol(x) != 2) stop("x must be a 2-column matrix or a length-2 vector")
  } else {
    if (length(x) == 2) x = matrix(x, ncol = 2, nrow = 1) else stop("x must be a 2-column matrix or a length-2 vector")
  }



  if ("matrix" %in% class(origin)) {
    if (ncol(origin) != 2) stop(
      "origin must be a 2-column matrix or a length-2 vector")
    if (nrow(origin == 1)) origin <- matrix(origin, nrow = nrow(x), ncol = 2, byrow = TRUE)
    if (nrow(origin) != nrow(x)) stop(
      "origin must have 1 row or the same number of rows as x")
  } else {
    if (length(origin) == 2) origin <- matrix(origin, nrow = nrow(x), ncol = 2, byrow = TRUE) else stop(
      "origin must be a 2-column matrix or a length-2 vector")
  }

  if (degrees) theta <- theta * pi / 180

  # https://www.wikiwand.com/en/Rotation_matrix

    ((x - origin) %*%  matrix(c(cos(theta),
                    -sin(theta),
                    sin(theta),
                    cos(theta)),
                  nrow = 2, ncol = 2)) + origin
}

skewed_axis <- function(theta,
                        axis_title = "X",
                        draw_ticks = TRUE,
                        draw_axis_text = TRUE,
                        remove_origin = TRUE,
                        tick_height = .02,
                        lwd = .5,
                        text_size = 12,
                        color = NA,
                        family = NA,
                        mu = 0,
                        sigma = 1,
                        tick_label_interval = ifelse(sigma %% 3 == 0, 3, 2)) {
  ticks <- seq(-4 * sigma + mu, 4 * sigma + mu, sigma / 3)
  if (remove_origin)
    ticks <- ticks[ticks != mu]

  d_ticks <- cbind(x = ticks, y = mu) %>%
    rotate2dmatrix(theta, origin = c(mu, mu)) %>%
    `colnames<-`(c("x", "y")) %>%
    tibble::as_tibble()

  p <- geom_line(data = d_ticks,
                 aes(x, y),
                 lwd = lwd,
                 color = color)

  u_ticks <- cbind(x = ticks, y = mu + tick_height * sigma) %>%
    rotate2dmatrix(theta, origin = c(mu, mu)) %>%
    `colnames<-`(c("x", "y"))

  l_ticks <- cbind(ticks, mu - tick_height * sigma) %>%
    rotate2dmatrix(theta, origin = c(mu, mu)) %>%
    `colnames<-`(c("xend", "yend"))

  d_tick_lines <- dplyr::bind_cols(u_ticks, l_ticks)


  if (draw_ticks) {
    p <- list(p,
              geom_segment(
                data = d_tick_lines,
                aes(
                  x = x,
                  y = y,
                  xend  = xend,
                  yend = yend
                ),
                lwd = lwd / 2,
                lty = 1,
                color = color
              ))


  }

  if (draw_axis_text) {
    tick_angle <-
      ifelse(theta > pi / 2, (theta - pi) * 180 / pi , theta * 180 / pi)
    tick_vjust <- ifelse(theta > pi / 2, 1, 1)

    tick_labels <-
      seq(-4 * sigma + mu, 4 * sigma + mu, tick_label_interval)
    if (remove_origin)
      tick_labels <- tick_labels[tick_labels != mu]

    d_labels <- cbind(x = tick_labels, y = mu) %>%
      rotate2dmatrix(theta, origin = c(mu, mu)) %>%
      `colnames<-`(c("x", "y")) %>%
      tibble::as_tibble()

    p <- list(
      p,
      ggtext::geom_richtext(
        data = d_labels,
        aes(x, y, label = tick_labels),
        label.margin = unit(1.5, "mm"),
        label.color = NA,
        vjust = tick_vjust,
        angle = tick_angle,
        size = ggtext_size(text_size),
        color = color,
        family = family,
        label.padding = unit(0, "mm")
      )
    )
  }
  p

}


#' A wrapper for ggtext::geom_richtext
#'
#' @param mapping aes()
#' @param label.margin grid unit margin
#' @param label.padding grid unit margin
#' @param label.color color
#' @param fill color
#' @param text_size text size in point units
#' @param ... additional parameters passed to ggtext::geom_richtext
#'
#' @return ggtext::geom_richtext
#' @export
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' library(dplyr)
#' library(WJSmisc)
#' tibble(x = 0, y = 0, l = "A") %>%
#'   ggplot() +
#'   geom_richlabel(aes(x,y,label = l))
geom_richlabel <- function(mapping,
                           label.margin = unit(2,"mm"),
                           label.padding = unit(.5,"mm"),
                           label.color = NA,
                           fill = "white",
                           text_size = 12,
                           ...) {
  ggtext::geom_richtext(
    mapping = mapping,
    label.margin = label.margin,
    label.padding = label.padding,
    label.color = label.color,
    fill = fill,
    size = ggtext_size(text_size),
    ...
  )
}


#' Convert snake case to subscript
#'
#' @param x character
#' @param sep separator defaults to "_"
#' @param prefix character prefix
#' @param suffix character suffix
#' @param collapse character parameter passed to paste0
#' @param recycle0 parameter passed to paste0
#'
#' @return a character
#' @export
#'
#' @examples
#' snake2subscript("x_1")
snake2subscript <- function(x,
                            sep = "_",
                            prefix = "<sub>",
                            suffix = "</sub>",
                            collapse = NULL,
                            recycle0 = FALSE) {
      paste0(
        stringr::str_replace(x,
                    pattern = sep,
                    replacement = prefix),
        suffix,
        collapse = collapse,
        recycle0 = recycle0)
  }



#' Compute the angle of a vector
#'
#' @param x A length-2 vector
#' @param origin A length-2 vector
#' @param degrees If TRUE, returns angles in degrees instead of radians
#' @param allow_negative If TRUE, returns angles between -pi and pi (-180 and +180. If FALSE, returns angles between 0 and 2 * pi (0 and 360)
#'
#' @return A length-1 vector
#' @export
#'
#' @examples
#' vector_angle(c(1,1))
#' vector_angle(c(1,1), degrees = TRUE)
vector_angle <- function(x, origin = c(0, 0), degrees = FALSE, allow_negative = FALSE) {
  if (length(x) != 2) stop("x must be a length-2 vector")
  if (length(origin) != 2) stop("origin must be a length-2 vector")
  x_0 <- x - origin
  theta <- atan2(x_0[2], x_0[1])
  if (theta < 0 & !allow_negative) theta <- 2 * pi + theta
  if (degrees) theta <- theta * 180 / pi

  theta
}


#' Name for square arrays like correlation matrices
#'
#' @param x a square array, matrix, or data.frame
#' @param value a vector of names
#'
#' @return a named square array, matrix, or data.frame
#' @export
#'
#' @examples
#' R <- tri2cor(.5)
#' cornames(R) <- c("A", "B")
`cornames<-` <- function(x, value) {
  dimnames(x) <- list(value, value)
  x
}


#' Converts a correlation matrix to a partial correlation matrix
#'
#' @param R correlation matrix
#'
#' @return Partial correlation matrix
#' @export
#'
#' @examples
#' R <- matrix(.6, nrow = 3, ncol = 3)
#' diag(R) <- 1
#' cor2pcor(R)
cor2pcor <- function(R) {
  K <- solve(R)
  P <- stats::cov2cor(K) * -1
  diag(P) <- 1
  P
}

#' IRT Plot Shiny App
#' @export
irt_plot_app <- function() {
  shiny::runApp(appDir = system.file('shiny/irtplot', package='WJSmisc'))
}


#' Retrieve text within a div with a named id
#'
#' @param id the id string (without #).
#' @param file file name
#' @param blockquote return as block quote (defaults to TRUE)
#'
#' @return a character vector of length 1
#' @export
get_quote <- function(id, file, blockquote = TRUE) {

  # Insert blockquote text
  if (blockquote) {
    blockchr <- "\n> "
    } else {
    blockchr <- ""
  }

  #read in lines from file
  filetext <- readLines(file)

  # collapse lines and search for span with id
  s <- filetext |>
    paste0(collapse = "\n") |>
    stringr::str_match(pattern = paste0("(?<=\\[).+(?=\\]\\{\\#",
                               id,
                               "\\})"))  |>
    getElement(1)

  # If no span found, search for div with id
  if (is.na(s)) {
    s <- filetext |>
      paste0(collapse = "|||") |>
      stringr::str_match(pattern = paste0(":::\\{\\#",
                                 id,
                                 "\\}(.*?):::"))  |>
      getElement(2) |>
      stringr::str_replace_all("\\|\\|\\|", blockchr)
  } else {
    s <- paste0(blockchr, s)
  }

  if (is.na(s)) stop("Could not find id = ", id)

  s
}


