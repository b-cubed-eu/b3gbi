% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{available_indicators}
\alias{available_indicators}
\title{Indicators Available for Use in the Package}
\format{
A special object of class 'available_indicators' containing a list of indicators and six fields with information about them
\describe{
\item{indicator_class}{class of the indicator}
\item{indicator_name}{name of the indicator}
\item{plot_title}{title to be used when plotting with automated title generation}
\item{legend_label}{title to be used when plotting with automated legend title generation}
\item{legend_transformation}{any transformation to perform on the legend when plotting, to improve visualization of maps}
\item{map_wrapper}{wrapper function to use when calculating indicator as a map}
\item{ts_wrapper}{wrapper function to use when calculating indicator as a time series}
\item{map_function_arguments}{any special arguments to consider when using the function to calculate an indicator map}
\item{ts_function_arguments}{any special arguments to consider when using the function to calculate an indicator time series}
}
}
\source{
N/A
}
\usage{
available_indicators
}
\description{
A list of all biodiversity indicators available within the package, along with the dimensions they can be calculated across, the functions to access them, and any special arguments
}
\keyword{datasets}
