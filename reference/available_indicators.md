# Indicators Available for Use in the Package

A list of all biodiversity indicators available within the package,
along with the dimensions they can be calculated across, the functions
to access them, and any special arguments

## Usage

``` r
available_indicators
```

## Format

A special object of class 'available_indicators' containing a list of
indicators and six fields with information about them

- indicator_class:

  class of the indicator

- indicator_name:

  name of the indicator

- plot_title:

  title to be used when plotting with automated title generation

- legend_label:

  title to be used when plotting with automated legend title generation

- legend_transformation:

  any transformation to perform on the legend when plotting, to improve
  visualization of maps

- map_wrapper:

  wrapper function to use when calculating indicator as a map

- ts_wrapper:

  wrapper function to use when calculating indicator as a time series

- map_function_arguments:

  any special arguments to consider when using the function to calculate
  an indicator map

- ts_function_arguments:

  any special arguments to consider when using the function to calculate
  an indicator time series

## Source

N/A
