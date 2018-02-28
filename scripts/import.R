# Loading data
base <- here::here()
setwd(paste0(base, "/raw/"))
library(xlsx)
# One file for each session
df <- read.xlsx(file = "raw.xlsx", sheetIndex = 1, startRow = 1, header = TRUE, endRow = 197)

save.image("raw.RData")

