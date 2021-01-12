

path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
getwd()

list.files()
library(dplyr)

csv <- 'SDG_15_cleanmerged.csv'
df <- read.csv(csv, stringsAsFactors = F) %>%
  dplyr::rename("CountryCode" = "countrycode")
View(df)

unique(df$CountryCode)
df1 <- df %>%
  filter(df$CountryCode %in% c("AFG")) ## I take only one nations' data as an example

str(df1)
view(df1)

## load function
source('function_fill_na2.R')

function_fill_na_wb(df)
