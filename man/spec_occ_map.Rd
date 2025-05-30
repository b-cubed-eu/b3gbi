% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator_wrappers.R
\name{spec_occ_map}
\alias{spec_occ_map}
\alias{spec_occ_ts}
\title{Calculate Species Occurrences Over Space or Time}
\usage{
spec_occ_map(data, ...)

spec_occ_ts(data, ...)
}
\arguments{
\item{data}{A data cube object (class 'processed_cube').}

\item{...}{
  Arguments passed on to \code{\link[=compute_indicator_workflow]{compute_indicator_workflow}}
  \describe{
    \item{\code{ci_type}}{Type of bootstrap confidence intervals to calculate. (Default: "norm".
Select "none" to avoid calculating bootstrap CIs.)}
    \item{\code{cell_size}}{Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)}
    \item{\code{level}}{Spatial level: 'cube', 'continent', 'country', 'world', 'sovereignty',
or 'geounit'. (Default: 'cube')}
    \item{\code{region}}{The region of interest (e.g., "Europe"). (Default: "Europe")}
    \item{\code{ne_type}}{The type of Natural Earth data to download: 'countries', 'map_units',
'sovereignty', or 'tiny_countries'. (Default: "countries")}
    \item{\code{ne_scale}}{The scale of Natural Earth data to download: 'small' - 110m,
'medium' - 50m, or 'large' - 10m. (Default: "medium")}
    \item{\code{output_crs}}{The CRS you want for your calculated indicator. (Leave blank
to let the function choose a default based on grid reference system)}
    \item{\code{first_year}}{Exclude data before this year. (Uses all data in the cube by default.)}
    \item{\code{last_year}}{Exclude data after this year. (Uses all data in the cube by default.)}
    \item{\code{spherical_geometry}}{If set to FALSE, will temporarily disable spherical geometry
while the function runs. Should only be used to solve specific issues. (Default is TRUE)}
    \item{\code{make_valid}}{Calls st_make_valid() from the sf package. Increases processing
time but may help if you are getting polygon errors. (Default is FALSE).}
    \item{\code{num_bootstrap}}{Set the number of bootstraps to calculate for generating
confidence intervals. (Default: 1000)}
    \item{\code{crs_unit_convert}}{Force a particular output CRS even when it has
different units than the input CRS. (Default: FALSE)}
    \item{\code{shapefile_path}}{Path of an external shapefile to merge into the workflow. For example,
if you want to calculate your indicator particular features such as protected areas or wetlands.}
    \item{\code{invert}}{Calculate an indicator over the inverse of the shapefile (e.g.
if you have a protected areas shapefile this would calculate an indicator over
all non protected areas)}
  }}
}
\value{
An S3 object with the classes 'indicator_map' or 'indicator_ts' and
'spec_occ' containing the calculated indicator values and metadata.
}
\description{
This function calculates the number of occurrences for individual
species over a gridded map or as a time series (see 'Details' for more
information).
}
\details{
Species occurrences are considered an essential biodiversity variable (EBV).
They are mapped by calculating the total number of occurrences
of a given species for each cell. This represents the occurrence frequency
distribution, and also indicates the observed species distribution. The
number of occurrences can act as a proxy for relative abundance of species
with a similar detectability, which is an important aspect of biodiversity
although not an indicator when calculated in isolation.
}
\section{Functions}{
\itemize{
\item \code{spec_occ_map()}: 

\item \code{spec_occ_ts()}: 

}}
\examples{
\dontrun{
so_map <- spec_occ_map(example_cube_1, level = "country", region = "Denmark")
plot(so_map, c(2440728, 4265185))
}
so_ts <- spec_occ_ts(example_cube_1, first_year = 1985)
plot(so_ts, c(2435767, 2434793))

}
\seealso{
compute_indicator_workflow
}
