
## ma_table_combination

#### Load list of complete tables ####

files <- list.files("../Meta-Data/EXCEL_FILES/", pattern = "[0-9].*.csv", recursive = TRUE)

x <- read.csv(paste0("../Meta-Data/EXCEL_FILES/",files[1]), stringsAsFactors=FALSE)

for (i in files[-c(2,24)]) {
  y <- read.csv(paste0("../Meta-Data/EXCEL_FILES/",i), stringsAsFactors=FALSE)
  keep.vars <- c(names(x), names(y))[duplicated(c(names(x), names(y)))]
  y <- y[, keep.vars]
  y$x_study <- as.character(y$x_study)
  y$x_doi <- as.character(y$x_doi)

  x <- merge(x, y, all = TRUE)
  x <- x[, keep.vars]
}


x <- sapply(1:length(files), 
            function(i) { 
              read.csv(paste0("../Meta-Data/EXCEL_FILES/", files[i]), stringsAsFactors=FALSE)
            }
)



# Check which tables contain basic info
sum(sapply(x, 
           function(i) {
             "x_study" %in% names(i) & "x_n" %in% names(i) & "x_es" %in% names(i)
           }
)
)
