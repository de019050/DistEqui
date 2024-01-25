#' Z-scaling
#'
#' @param data frame
#' @param charge_col name of col
#' @param cols_to_scale
#'
#' @return A dataframe with additional z scaled coloumns
#' @export
#'
#' @examples jojo
#'
scale_per_charge <- function(data, charge_col, cols_to_scale) {
  # Nested function to perform Z-scaling
  z_scale <- function(x) {
    (x - base::mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  }

  # Apply the Z-scaling for each charge
  data %>%
    dplyr::group_by(!!sym(charge_col)) %>%
    dplyr::mutate(across(all_of(cols_to_scale), z_scale, .names = "Z_{.col}")) %>%
    dplyr::ungroup()
}

