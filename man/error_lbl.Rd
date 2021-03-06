% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{error_lbl}
\alias{error_lbl}
\title{Labels for accuracy metric}
\usage{
error_lbl(
  truth,
  estimate,
  show_metrics = c("rmse", "mean", "bias", "corr"),
  metrics_lbl = c(rmse = "RMSE", mean = "Mean Abs. Dev.", bias = "Mean Dev.", corr =
    "Correlation"),
  err_accuracy = 0.1,
  is_percent = TRUE,
  suff = ifelse(is_percent, "pp", "")
)
}
\arguments{
\item{truth}{Vector of true values}

\item{estimate}{Vector of estimates, must be the same length as \code{truth}.
In fact, the metrics are invariant to which goes in which.}

\item{show_metrics}{The metrics to show. Defaults to RMSE and accuracy}

\item{metrics_lbl}{The labels to show for each metric. Named vector}

\item{err_accuracy}{Significant digits for percentage points. Corresponds to
the accuracy argument in scales::percent}

\item{is_percent}{Is the error displayed in percent (pp)?}

\item{suff}{Suffix units to put on}
}
\description{
Labels for accuracy metric
}
