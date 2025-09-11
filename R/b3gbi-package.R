#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @importFrom purrr map keep map2
#' @importFrom readr read_csv
#' @importFrom dplyr group_split mutate bind_rows distinct select n_distinct
#' @importFrom dplyr group_by summarise select arrange full_join inner_join
#'  left_join
#' @importFrom dplyr rename filter slice_max across any_of
#' @importFrom tidyr pivot_wider pivot_longer gather spread
#' @importFrom ggplot2 ggplot geom_sf geom_smooth geom_ribbon geom_errorbar
#'  geom_point geom_line
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom boot boot boot.ci
#' @importFrom stringr str_detect str_extract
#' @importFrom sf st_as_sf st_transform st_bbox st_make_grid st_cast st_sf
#'  st_make_valid st_area st_agr st_join st_intersects sf_use_s2 st_intersection
#'  st_crs
#' @importFrom units set_units
#' @importFrom rnaturalearth ne_countries
#' @importFrom patchwork wrap_plots
#' @importFrom stats approx cor loess median predict quantile sd
#' @importFrom utils data find modifyList
#' @importFrom rlang := %||% caller_env
#' @importFrom labeling extended
## usethis namespace: end
NULL
