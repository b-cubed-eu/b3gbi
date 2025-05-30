% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bootstrap_ci.R
\name{get_bootstrap_ci}
\alias{get_bootstrap_ci}
\title{Calculate confidence intervals for list of \code{boot} objects}
\usage{
get_bootstrap_ci(bootstrap_list, ..., temporal_list_name = "year")
}
\arguments{
\item{bootstrap_list}{A list of objects of class \code{"boot"} per year.}

\item{...}{Additional argument to be passed to the \code{boot::boot.ci()}
function.}

\item{temporal_list_name}{The temporal list names of \code{bootstrap_list}
(e.g., year, month ...) containing time point values. Default \code{year}.}
}
\value{
The returned value is a dataframe containing the time point,
the type of interval (\code{int_type}), the lower limit of the confidence
interval (\code{ll}), the upper limit of the confidence interval (\code{ul}), and the
confidence level of the intervals (\code{conf_level}).
}
\description{
This function calculates confidence intervals for a list of objects of class
\code{"boot"} per year into a dataframe containing all required summaries.
}
