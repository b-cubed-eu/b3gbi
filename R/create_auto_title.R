#' @noRd
create_auto_title <- function(auto_title, min_year, max_year) {

  title <- paste(auto_title,
                 " (",
                 min_year,
                 "-",
                 max_year,
                 ")",
                 sep = "")

  return(title)

}
