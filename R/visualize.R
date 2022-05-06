#' Standard scatterplot with 45 degree line
#'
#' @param tbl dataset of summary statistics. Each row must represent a point.
#'  Statistics of each point, like the point estimate, 5% quantile, etc.., must
#'  be in columns.
#' @param xvar Variable to put on x-axis, unquoted
#' @param yvar Variable to put on y-axis, unquoted
#' @param lblvar Variable to use as labels for `geom_text_repel`, unquoted
#' @param colvar Variable to use as colors, unquoted
#' @param ubvar,lbvar Variable to use as upper and lower bounds for `geom_errorbar`, unqoted
#' @param xlab,ylab x and y-axis labels, respectively
#' @param xlim,ylim x and y-axis limits, respectively
#' @param by_form If the dataset is in long form with separate rows for different
#'  model estimates, you can supply a formula to be passed on to `facet_rep_wrap()` to
#'  have separate facets for each model.
#' @param percent Are axis values in percent? Defaults to `TRUE`
#' @param pct_accuracy If using percents on axes, what is the accuracy. Defaults to `1`, which displays
#'  whole numbers
#' @param by_nrow If using facets, how many rows should the facet take? Defaults to NULL,
#' which is facet_wrap's default
#' @param by_labels A named vector for the facets, where the names are the names
#' of the unique values of the variable by specified in `by_form` (e.g. "model") and
#' the values are the corresponding characters to recode to.
#' @param alpha.point,alpha.CI,alpha.text The transparency value for the
#'  points (`.point`), intervals (`.CI`), and text labels (`.text`).s
#' @param alpha.segment The transparency value for the segments linking the labels to the points, ranging from 0 to 1.
#' @param size.point Size of points to use in ggplot
#' @param size.text Size for the labels
#' @param size.errorstat Size for the error statistic
#' @param max.overlaps To be passed on to \code{geom_text_repel} if a label
#'  is used.
#' @param repeat.axis.text Whether to reproduce the axis texts for every facets in
#'  `facet_rep_wrap()`. Defaults to `FALSE`
#' @param show_error  Which error(s) to show if any. Currently supports
#'   c(`"rmse"`, `"mean`, `"bias"`, `"corr"`). `NULL` for now display.
#' @param expand_axes Whether to expand the axes so that the plot is a square,
#'  even if there is more whitespace. Overrides xlim and ylim.
#' @param ... Additional arguments sent to the \code{error_lbl} function
#'
#'
#'
#' @import ggplot2
#' @importFrom lemon facet_rep_wrap
#' @importFrom scales percent_format
#' @importFrom ggrepel geom_text_repel
#' @importFrom stringr str_replace str_trim str_remove
#' @importFrom dplyr pull enquo `%>%`
#' @importFrom tibble enframe
#' @importFrom purrr is_formula
#' @importFrom stats terms
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ccesMRPrun)
#'
#' mrp_df <- summ_sims(poststrat_draws(fit_GA, poststrat_tgt = acs_GA)) %>%
#'   left_join(elec_GA)
#'
#'
#' scatter_45(mrp_df,
#'            clinton_vote,
#'            p_mrp_est,
#'            lblvar = cd,
#'            lbvar = p_mrp_050,
#'            ubvar = p_mrp_950,
#'            xlab = "Clinton Vote",
#'            ylab = "MRP Estimate")
#'
#'
scatter_45 <- function(tbl, xvar, yvar,
                       lblvar = NULL,
                       xlab = NULL, ylab = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       size.point = 0.8,
                       size.text = 2,
                       size.errorstat = 2,
                       percent = TRUE,
                       pct_accuracy = 1,
                       ubvar = NULL, lbvar = NULL,
                       colvar = NULL,
                       alpha.CI = 0.75,
                       alpha.point = 0.8,
                       alpha.text = 0.5,
                       alpha.segment = 0.5,
                       max.overlaps = 20,
                       repeat.axis.text = FALSE,
                       by_form = NULL,
                       by_nrow = NULL,
                       by_labels = NULL,
                       show_error = "rmse",
                       expand_axes = TRUE, ...) {
  # setup
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)

  lblvar <- enquo(lblvar)
  lbl_name <- quo_name(lblvar)

  lbvar <- enquo(lbvar)
  lb_name <- quo_name(lbvar)
  ubvar <- enquo(ubvar)
  ub_name <- quo_name(ubvar)

  colvar <- enquo(colvar)
  col_name <- quo_name(colvar)

  if (!is.null(by_form))
    stopifnot(is_formula(by_form))

  axis_lim <- range(c(pull(tbl, !!xvar), pull(tbl, !!yvar)))

  if (expand_axes) {
    xlim = ylim = axis_lim
    }

  # main plot -- defaults
  gg0 <- ggplot(tbl, aes(x = {{xvar}}, y = {{yvar}}, color = {{colvar}})) +
    geom_point(size = size.point, alpha = alpha.point) +
    coord_equal(xlim = xlim, ylim = ylim) +
    theme_bw()

  gg1 <- gg0 +
    geom_abline(linetype = "dashed", alpha = 0.75)

  if (percent) {
    gg1 <- gg1 +
    scale_x_continuous(labels = percent_format(accuracy = pct_accuracy)) +
      scale_y_continuous(labels = percent_format(accuracy = pct_accuracy))
  }

  if (!is.null(by_form)) {
    form_char <- str_trim(str_remove(attr(terms(by_form), "term.labels"), "~"))
    formvar <- enquo(form_char)
    gg1 <- gg1 +
      facet_rep_wrap(by_form,
                     labeller = as_labeller(by_labels),
                     repeat.tick.labels = repeat.axis.text,
                     nrow = by_nrow)
  }

  if (ub_name != "NULL" & lb_name != "NULL") {
    gg1 <- gg1 +
      geom_errorbar(aes(ymin = {{lbvar}}, ymax = {{ubvar}}), width = 0, alpha = alpha.CI)
  }

  if (lbl_name != "NULL") {
    gg1 <- gg1 +
      geom_text_repel(aes(label = {{lblvar}}),
                      alpha = alpha.text,
                      size = size.text,
                      segment.alpha = alpha.segment,
                      max.overlaps = max.overlaps)
  }

  if (!is.null(xlab))
    gg1 <- gg1 + labs(x = xlab)

  if (!is.null(ylab))
    gg1 <- gg1 + labs(y = ylab)

  if (!is.null(show_error)) {
    if (is.null(by_form)) {
      err_txt <- error_lbl(truth = pull(tbl, !!xvar),
                           estimate = pull(tbl, !!yvar),
                           show_metrics = show_error,
                           is_percent = percent,
                           ...)
      gg1 <- gg1 +
        labs(caption = err_txt)
    }

    # separate stats by model
    if (!is.null(by_form)) {
      err_df <- tbl %>%
        group_by(across(all_of(form_char))) %>%
        summarize(
          text_to_show = error_lbl({{xvar}},
                                   {{yvar}},
                                   show_metrics = show_error,
                                   is_percent = percent,
                                   ...),
          .groups = "drop")

      gg1 <- gg1 +
        geom_text(data = err_df,
                  mapping = aes(label = text_to_show),
                  x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
                  lineheight = 1, size = size.errorstat, inherit.aes = FALSE)
    }
  }

  gg1
}


#' Labels for accuracy metric
#'
#' @param truth Vector of true values
#' @param estimate Vector of estimates, must be the same length as \code{truth}.
#'  In fact, the metrics are invariant to which goes in which.
#' @param show_metrics The metrics to show. Defaults to RMSE and accuracy
#' @param metrics_lbl The labels to show for each metric. Named vector
#' @param err_accuracy Significant digits for percentage points. Corresponds to
#' the accuracy argument in scales::percent
#' @param is_percent Is the error displayed in percent (pp)?
#' @param suff Suffix units to put on
#'
#' @importFrom scales percent_format percent number
#' @importFrom stringr str_c
#' @importFrom glue glue
#'
#' @export
error_lbl <- function(truth, estimate,
                      show_metrics = c("rmse", "mean", "bias", "corr"),
                      metrics_lbl = c(rmse = "RMSE", mean = "Mean Abs. Dev.", bias = "Mean Dev.", corr = "Correlation"),
                      err_accuracy = 0.1,
                      is_percent = TRUE,
                      suff = ifelse(is_percent, "pp", "")) {

  rmse_stat <- sqrt(mean((truth - estimate)^2))
  mean_stat <- mean(abs(truth - estimate))
  bias_stat <- mean(estimate - truth)
  corr_stat <- cor(estimate, truth)

  stat_vec <- c(rmse = rmse_stat, mean = mean_stat, bias = bias_stat, corr = corr_stat)

  if (is_percent)
    show_stat <- percent(stat_vec[show_metrics], accuracy = err_accuracy, suffix = suff)

  if (!is_percent)
    show_stat <- number(stat_vec[show_metrics], accuracy = err_accuracy, suffix = suff)

  show_lbl <- str_c(str_c(metrics_lbl[show_metrics], show_stat, sep = ": "),
                    collapse = "\n")

  show_lbl
}




