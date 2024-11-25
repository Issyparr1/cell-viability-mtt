rename_columns <- function(colnames) {
  new_colnames <- sapply(colnames, function(col_name) {
    if (grepl("^mean_\\d", col_name)) { 
      num_part <- sub("mean_(\\d+)", "\\1", col_name)
      if (as.numeric(num_part) != 0) {
        new_num <- as.numeric(num_part) / 2
        return(paste("mean", new_num, sep = "_"))  # Return the updated name
      } else {
        return(col_name)  # Return the name unchanged if the number is 0
      }
    } else {
      return(col_name)  # Keep original name for non-matching columns
    }
  })
  return(new_colnames)
}
