calc_ci <- function(df, ...) {

  # Bootstrapping
  bootstrap_data <- perform_bootstrap_ts(
    data_cube_df = df,
    fun = calc_ts,
    samples = 1000,
    seed = 123)

}
