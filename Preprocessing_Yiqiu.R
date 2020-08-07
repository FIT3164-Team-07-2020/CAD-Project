# Package "readxl" is used for reading "xlsx / xls" files.
library(readxl)
ZAS_Original <- read_excel("Z_Alizadeh_Sani_Dataset.xlsx")

# Check whether there is any na or null value in the dataset.
# The results indicate there is no special value in the dataset.
sum(is.na(ZAS_Original))
sum(is.null(ZAS_Original))
