% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{scatter_45}
\alias{scatter_45}
\title{Standard scatterplot with 45 degree line}
\usage{
scatter_45(
  tbl,
  xvar,
  yvar,
  lblvar = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  size.point = 0.8,
  size.text = 2,
  size.errorstat = 2,
  percent = TRUE,
  pct_accuracy = 1,
  ubvar = NULL,
  lbvar = NULL,
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
  expand_axes = TRUE,
  ...
)
}
\arguments{
\item{tbl}{dataset of summary statistics. Each row must represent a point.
Statistics of each point, like the point estimate, 5\% quantile, etc.., must
be in columns.}

\item{xvar}{Variable to put on x-axis, unquoted}

\item{yvar}{Variable to put on y-axis, unquoted}

\item{lblvar}{Variable to use as labels for \code{geom_text_repel}, unquoted}

\item{xlab, ylab}{x and y-axis labels, respectively}

\item{xlim, ylim}{x and y-axis limits, respectively}

\item{size.point}{Size of points to use in ggplot}

\item{size.text}{Size for the labels}

\item{size.errorstat}{Size for the error statistic}

\item{percent}{Are axis values in percent? Defaults to \code{TRUE}}

\item{pct_accuracy}{If using percents on axes, what is the accuracy. Defaults to \code{1}, which displays
whole numbers}

\item{ubvar, lbvar}{Variable to use as upper and lower bounds for \code{geom_errorbar}, unqoted}

\item{colvar}{Variable to use as colors, unquoted}

\item{alpha.point, alpha.CI, alpha.text}{The transparency value for the
points (\code{.point}), intervals (\code{.CI}), and text labels (\code{.text}).s}

\item{alpha.segment}{The transparency value for the segments linking the labels to the points, ranging from 0 to 1.}

\item{max.overlaps}{To be passed on to \code{geom_text_repel} if a label
is used.}

\item{repeat.axis.text}{Whether to reproduce the axis texts for every facets in
\code{facet_rep_wrap()}. Defaults to \code{FALSE}}

\item{by_form}{If the dataset is in long form with separate rows for different
model estimates, you can supply a formula to be passed on to \code{facet_rep_wrap()} to
have separate facets for each model.}

\item{by_nrow}{If using facets, how many rows should the facet take? Defaults to NULL,
which is facet_wrap's default}

\item{by_labels}{A named vector for the facets, where the names are the names
of the unique values of the variable by specified in \code{by_form} (e.g. "model") and
the values are the corresponding characters to recode to.}

\item{show_error}{Which error(s) to show if any. Currently supports
c(\code{"rmse"}, \verb{"mean}, \code{"bias"}, \code{"corr"}). \code{NULL} for now display.}

\item{expand_axes}{Whether to expand the axes so that the plot is a square,
even if there is more whitespace. Overrides xlim and ylim.}

\item{...}{Additional arguments sent to the \code{error_lbl} function}
}
\description{
Standard scatterplot with 45 degree line
}
\examples{
library(dplyr)
library(ccesMRPrun)

mrp_df <- summ_sims(poststrat_draws(fit_GA, poststrat_tgt = acs_GA)) \%>\%
  left_join(elec_GA)


scatter_45(mrp_df,
           clinton_vote,
           p_mrp_est,
           lblvar = cd,
           lbvar = p_mrp_050,
           ubvar = p_mrp_950,
           xlab = "Clinton Vote",
           ylab = "MRP Estimate")


}
