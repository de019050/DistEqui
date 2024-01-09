
# Define a function to apply Z-scaling for each charge
scale_per_charge <- function(data, charge_col, cols_to_scale) {
  # Nested function to perform Z-scaling
  z_scale <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }

  # Apply the Z-scaling for each charge
  data %>%
    group_by(!!sym(charge_col)) %>%
    mutate(across(all_of(cols_to_scale), z_scale, .names = "Z_{.col}")) %>%
    ungroup()
}
