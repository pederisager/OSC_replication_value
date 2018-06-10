
#### Load meta analysis table and fill in missing values ####

fillRows <- function(table, fill = "x_study", reference = "x_es") {
  for (i in 1:nrow(table)) {
    if (is.na(table[i, fill]) & !is.na(table[i, reference]))
      table[i, fill] <- table[i-1, fill]
  }
  return(table)
}

