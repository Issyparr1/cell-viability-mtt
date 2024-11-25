remove_outliers <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ {
      Q1 <- quantile(.x, 0.25, na.rm = TRUE)
      Q3 <- quantile(.x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      .x[.x < (Q1 - 1.5 * IQR) | .x > (Q3 + 1.5 * IQR)] <- NA
      .x
    }))
}
