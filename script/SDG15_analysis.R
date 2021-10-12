### SDG 15 Analysis
### 2020-06-11
### Yuqian Zhang


# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# setwd(dir)
# getwd()

source( file="script/reference.R" ); 

# Library deployment

library(plyr)
library(tibble)
# library(tidyr)
library(reshape2)
library(writexl)
library(tidyverse)
# library(ggplot2)
library(lubridate)
# library(dplyr)
library(plm)


SDG_15_finalclean <- read.csv("data/SDG_15_finalclean_all.csv")
str(SDG_15_finalclean)
View(SDG_15_finalclean)
  # colnames(SDG_15_finalclean)
SDG_15_finalclean$value_cal <- SDG_15_finalclean$fill_all_Nas
colnames(SDG_15_finalclean)[1] <- "Country"
SDG_15_NoCal_2000_2018 <- subset(SDG_15_finalclean, Year!=2019)
write.csv(SDG_15_NoCal_2000_2018,"data/SDG_15_NoCal_2000-2018.csv")


  
SDG_15_weight <- read.csv("data/SDG 15 weight.csv")
str(SDG_15_weight)
View(SDG_15_weight)
colnames(SDG_15_weight)[1] <- "SDG"


## Calculation by country over years

## Select one country for example 
unique(SDG_15_finalclean$CountryCode)
df <- SDG_15_finalclean %>%
  filter(SDG_15_finalclean$CountryCode %in% unique(SDG_15_finalclean$CountryCode)) ## Select one country for example
str(df)
   view(df)
df$value_caled <- df$value_cal 
  
  n1 <- length(df$value_cal)
  n2 <- length(SDG_15_weight$sdg)
     for (i in seq(1:n1)) {
       for (j in seq(1:n2)) {
         if (df$SDG[i] == SDG_15_weight$sdg[j]) {
         df$value_caled[i] <- df$value_cal[i] * SDG_15_weight$Cal_weight[j]
         }
       }
     }
   
view(df)

# write.csv(df, "SDG_15_caled.csv")

# df$value_all <- 0
# 
# columns <- c(df$Country, df$Year)
# df_new <- df %>% group_by(columns)
# 
# df_new
# 
# 
# df %>%
#    group_by(df$Country, df$Year)
# value_all <- aggregate(df$value_caled, by = list(df$Country, df$Year), FUN = sum)

value_sum <- aggregate(df$value_caled,
                       by = list(df$Country, df$Year),
                       FUN = sum)
colnames(value_sum) <- c("Country", "Year", "Value_sum")
view(value_sum)

# Create new dataset
m1 <- length(unique(df$Country))
m2 <- length(unique(df$Year))

n_row <- m1 * m2
n_col <- 4

df_cal <- data.frame(matrix(0, nrow= n_row, ncol=n_col))
colnames(df_cal) <- c("Country", "CountryCode", "Year", "SDG")
columns <- c("Country", "CountryCode", "Year", "SDG")

view(df_cal)

   for (i in seq(1:m1)){
      df_cal$Country[((i-1)*m2+1):((i-1)*m2+20)] <- unique(df$Country)[i]
      df_cal$CountryCode[((i-1)*m2+1):((i-1)*m2+20)] <- unique(df$CountryCode)[i]
      df_cal$Year[((i-1)*m2+1):((i-1)*m2+20)] <- unique(df$Year)
      df_cal$SDG[((i-1)*m2+1):((i-1)*m2+20)] <- 15
   }
  
View(df_cal)
      
SDG_15_Cal <- merge(df_cal, value_sum, by=c("Country", "Year"))
view(SDG_15_Cal)
str(SDG_15_Cal)

SDG_15_Cal_2000_2018 <- subset(SDG_15_Cal, Year!=2019)
View(SDG_15_Cal_2000_2018)
str(SDG_15_Cal_2000_2018)
write.csv(SDG_15_Cal_2000_2018,"data/SDG_15_Cal_2000-2018.csv")








# 
# view(df)
# 
# 
# df1 <- unique(df$Country)
# view(df1)
# 
# 
# df2 <- unique(df$Year)
# view(df2)
# 
# 
# df3 <- cbind(df1, df2)
# view(df3)
# 



## Visualize the calculated SDG



## For loop of countires, run all 92 countries
## In the loop   
   
   ## for loop of years, run 2000-2018
   ## In the loop 
   
   ## {
   ## IF SDG 15_1_1, value * weight
   ## IF SDG 15_1_2_1, value * weight
   ## ...
   ## Add all calcaultions up
   ## }
   
   ## Loop for another year
   ## Fill each year data for a country
   
## Loop for another country
## Outside loop, print out all countries with years of data.
   
   
