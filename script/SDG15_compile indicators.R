
## SDG 15 Indicator compilation
## 2020-06-01
## Yuqian Zhang


# Library deployment

library(plyr)
library(tibble)
library(tidyr)
library(reshape2)
library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Set working directory

# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG/data")
getwd()

##############################
## Clean dataset SDG 15.1.1 ##
##############################

# Load data
SDG_15_1_1 <- read.csv("short_SDG_15_1_1.csv")
   # View(SDG_15_1_1)
   # colnames(SDG_15_1_1)

# Drop irrelevant columns
   SDG_15_1_1c = subset(SDG_15_1_1, select = -c(X, CountryCode))
   # view(SDG_15_1_1c)

# data_long <- gather(data=SDG_15_1_1, key=values, 2000:2016, factor_key=TRUE)

# Clean data with melting from wide to long
SDG_15_1_1_long <- melt(SDG_15_1_1c, id.vars = c("Country","countrycode"))
  # View(SDG_15_1_1_long)

# Add Year to a new column name
names(SDG_15_1_1_long)[names(SDG_15_1_1_long)=="variable"] <- "Year"
  # View(SDG_15_1_1_long)


# Sort data by Country, year
attach(SDG_15_1_1_long)

SDG_15_1_1_long$Year <- gsub("X", "", SDG_15_1_1_long$Year)
  # view(SDG_15_1_1_long)

  # Add SDG subgoals in a new column
SDG_15_1_1_sorted <- add_column(SDG_15_1_1_long, SDG = "15_1_1", .after = "countrycode")
  # View(SDG_15_1_1_sorted)

SDG_15_1_1_clean <- SDG_15_1_1_sorted[order(SDG_15_1_1_sorted$Country, SDG_15_1_1_sorted$SDG, SDG_15_1_1_sorted$Year),]
view(SDG_15_1_1_clean)





##############################
## Clean dataset SDG 15.1.2 ##
##############################

# SDG 15.1.2.1
SDG_15_1_2_1 <- read.csv("short_SDG_15_1_2_1.csv")
  # View(SDG_15_1_2_1)

# Drop irrelevant columns
SDG_15_1_2_1c = subset(SDG_15_1_2_1, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_1_2_1_long <- melt(SDG_15_1_2_1c, id.vars = c("Country","countrycode"))
  # View(SDG_15_1_2_1_long)

# Add Year to a new column name
names(SDG_15_1_2_1_long)[names(SDG_15_1_2_1_long)=="variable"] <- "Year"
  # View(SDG_15_1_2_1_long)

# Sort data by Country, year
attach(SDG_15_1_2_1_long)

SDG_15_1_2_1_long$Year <- gsub("X", "", SDG_15_1_2_1_long$Year)
  # view(SDG_15_1_2_1_long)

SDG_15_1_2_1_sorted <- SDG_15_1_2_1_long[order(SDG_15_1_2_1_long$Country,SDG_15_1_2_1_long$Year),]
  # view(SDG_15_1_2_1_sorted)

# Add SDG subgoals in a new column
SDG_15_1_2_1_clean <- add_column(SDG_15_1_2_1_sorted, SDG = "15_1_2_1", .after = "countrycode")
  View(SDG_15_1_2_1_clean)


# SDG 15.1.2.2
SDG_15_1_2_2 <- read.csv("short_SDG_15_1_2_2.csv")
  # View(SDG_15_1_2_2)

# Drop irrelevant columns
SDG_15_1_2_2c = subset(SDG_15_1_2_2, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_1_2_2_long <- melt(SDG_15_1_2_2c, id.vars = c("Country","countrycode"))
  # View(SDG_15_1_2_2_long)

# Add Year to a new column name
names(SDG_15_1_2_2_long)[names(SDG_15_1_2_2_long)=="variable"] <- "Year"
  # View(SDG_15_1_2_2_long)

# Sort data by Country, year
attach(SDG_15_1_2_2_long)

SDG_15_1_2_2_long$Year <- gsub("X", "", SDG_15_1_2_2_long$Year)
  # view(SDG_15_1_2_2_long)

SDG_15_1_2_2_sorted <- SDG_15_1_2_2_long[order(SDG_15_1_2_2_long$Country,SDG_15_1_2_2_long$Year),]
  # view(SDG_15_1_2_2_sorted)

# Add SDG subgoals in a new column
SDG_15_1_2_2_clean <- add_column(SDG_15_1_2_2_sorted, SDG = "15_1_2_2", .after = "countrycode")
   View(SDG_15_1_2_2_clean)


# # number of unique countries
# paste ("Number of countries:", length(unique(test$Country)))
# 
# # number of unique country code
# paste ("Number of country code:", length(unique(test$CountryCode)))


# Merge two data frames (SDG 15.1.2.1 and 15.1.2.2) by Country
SDG_15_1_2_clean <- rbind(SDG_15_1_2_1_clean, SDG_15_1_2_2_clean)

# Sort data by Country, CountryCode, SDG, Year
SDG_15_1_2_2_cleansorted <- SDG_15_1_2_clean[order(SDG_15_1_2_clean$Country,SDG_15_1_2_clean$countrycode,
                                                   SDG_15_1_2_clean$SDG,SDG_15_1_2_clean$Year),]
view(SDG_15_1_2_2_cleansorted)



#########################
## Merge into SDG 15.1 ##
#########################

SDG_15_1_clean <- rbind(SDG_15_1_1_clean, SDG_15_1_2_2_cleansorted)
  # view(SDG_15_1_clean)

# Sort data by Country, CountryCode, SDG, Year
SDG_15_1_cleansorted <- SDG_15_1_clean[order(SDG_15_1_clean$Country,SDG_15_1_clean$SDG,
                                             SDG_15_1_clean$countrycode,SDG_15_1_clean$Year),]
  view(SDG_15_1_cleansorted)
  
# write.csv(SDG_15_1_cleansorted,"SDG_15_1_cleanmerged.csv")
  
  csv <- 'SDG_15_1_cleanmerged.csv'
  df <- read.csv(csv, stringsAsFactors = F)
  
  source("function_fill_na.R")
  function_fill_na_wb(df)
  
  
   
 
# ######################################################################       
# ### Validate and fill data gaps for SDG 15.1 between 2009 and 2018 ###
# ###################################################################### 
# 
# # Fill in all country code
# 
# function_fill_na_wb <- function(df) {
#   dfs <- data.frame()
# 
#   for (i in seq(1:length(df$iso3_eora))) {
#     print(i)
#     ### loop each country code
#     print(df$iso3_eora[i])
# 
#     df1 <- df %>%
#       filter(iso3_eora == df$iso3_eora[i]) %>% ## ROW, ERI, KWT, MNE
#       gather(key = 'year', value =  'value', -c(1:6)) %>%
#       mutate(year = gsub('X', '', year)) %>%
#       mutate(year = year(as.Date(year, format="%Y"))) %>%
#       mutate(value = as.numeric(value)) %>%
#       arrange(year) # %>% ## order by date
# 
# 
#     ### if too many NA, then NA; if less, use interpolation
#     if (sum(is.na(df1$value)) > (length(df1$iso3_eora)-2)) {
#       df1 <- df1 %>%
#         mutate(value_ks = value)}
#     else {
#       df1 <- df1 %>%
#         mutate(value_ks = na_interpolation(x = value, option = 'stine'))}
# 
# 
#     ### ifelse within dplyr does NOT work well
#     # mutate(value_ks = ifelse(
#     #   sum(is.na(value)) > (length(iso3_eora)-2),
#     #   value,
#     #   na_interpolation(x = value, option = 'stine'))) ## linear, stine
# 
#     dfs <- rbind(dfs, df1)
#   }
# 
# 
#   ### plot and examine the interpolation
#   # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
#   plot <- dfs %>%
#     ### randomly choose 20 countries and plot
#     # slice(sample(1:190, size = 20, replace = F)) %>%
#     # slice(1:39*10) %>%
#     filter(iso3_eora %in% df$iso3_eora[sample(1:190, size = 20, replace = T)]) %>%
# 
#     ggplot()+
#     geom_line(aes(x = year, y = value),    color = 'blue', size = 6) +
#     geom_line(aes(x = year, y = value_ks), color = 'red', size = 2) +
#     facet_wrap(~iso3_eora, scales = 'free_y') +
#     # xlim(1990, 2005) +
#     # ylim(0, 7*10^5)+
#     theme_bw()
#   #
# 
#   dfs_update <- dfs %>%
#     select(-value) %>%
#     spread(key = year, value = value_ks) %>%
#     left_join(x=ctr_eora, y = ., by=c('country_eora', 'iso3_eora')) ### make sure the same order
#   fname <- paste0(dir.output, iname, '_FILLNA.xlsx'); print(fname)
#   # fname <- paste0(dir.output, gsub('.csv', '_FILLNA.xlsx', csv)); print(fname)
#   # write.csv(x = dfs_update, file = fname, row.names = F)
#   write_xlsx(x = dfs_update, path = fname)
#   return(plot)
# }
# 
# #######################################################################################

##############################
## Clean dataset SDG 15.2.1 ##
##############################

# SDG 15.2.1.1
SDG_15_2_1_1 <- read.csv("short_SDG_15_2_1_1.csv")
# View(SDG_15_2_1_1)

# Drop irrelevant columns
SDG_15_2_1_1c = subset(SDG_15_2_1_1, select = -c(X, CountryCode))
# view(SDG_15_2_1_1c)

# Clean data with melting from wide to long
SDG_15_2_1_1_long <- melt(SDG_15_2_1_1c, id.vars = c("Country","countrycode"))
  # View(SDG_15_2_1_1_long)

# Add Year to a new column name
names(SDG_15_2_1_1_long)[names(SDG_15_2_1_1_long)=="variable"] <- "Year"
  # View(SDG_15_2_1_1_long)

# Sort data by Country, year
attach(SDG_15_2_1_1_long)

SDG_15_2_1_1_long$Year <- gsub("X", "", SDG_15_2_1_1_long$Year)
  # view(SDG_15_2_1_1_long)

SDG_15_2_1_1_sorted <- SDG_15_2_1_1_long[order(SDG_15_2_1_1_long$Country,SDG_15_2_1_1_long$Year),]
  # view(SDG_15_1_2_1_sorted)

# Add SDG subgoals in a new column
SDG_15_2_1_1_clean <- add_column(SDG_15_2_1_1_sorted, SDG = "15_2_1_1", .after = "countrycode")
View(SDG_15_2_1_1_clean)



# SDG 15.2.1.2
SDG_15_2_1_2 <- read.csv("short_SDG_15_2_1_2.csv")
# View(SDG_15_2_1_2)

# Drop irrelevant columns
SDG_15_2_1_2c = subset(SDG_15_2_1_2, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_2_1_2_long <- melt(SDG_15_2_1_2c, id.vars = c("Country","countrycode"))
# View(SDG_15_2_1_2_long)

# Add Year to a new column name
names(SDG_15_2_1_2_long)[names(SDG_15_2_1_2_long)=="variable"] <- "Year"
# View(SDG_15_2_1_2_long)

# Sort data by Country, year
attach(SDG_15_2_1_2_long)

SDG_15_2_1_2_long$Year <- gsub("X", "", SDG_15_2_1_2_long$Year)
# view(SDG_15_2_1_2_long)

SDG_15_2_1_2_sorted <- SDG_15_2_1_2_long[order(SDG_15_2_1_2_long$Country,SDG_15_2_1_2_long$Year),]
# view(SDG_15_2_1_2_sorted)

# Add SDG subgoals in a new column
SDG_15_2_1_2_clean <- add_column(SDG_15_2_1_2_sorted, SDG = "15_2_1_2", .after = "countrycode")
View(SDG_15_2_1_2_clean)





# SDG 15.2.1.3
SDG_15_2_1_3 <- read.csv("short_SDG_15_2_1_3.csv")
View(SDG_15_2_1_3)

# Drop irrelevant columns
SDG_15_2_1_3c = subset(SDG_15_2_1_3, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_2_1_3_long <- melt(SDG_15_2_1_3c, id.vars = c("Country","countrycode"))
# View(SDG_15_2_1_3_long)

# Add Year to a new column name
names(SDG_15_2_1_3_long)[names(SDG_15_2_1_3_long)=="variable"] <- "Year"
# View(SDG_15_2_1_3_long)

# Sort data by Country, year
attach(SDG_15_2_1_3_long)

SDG_15_2_1_3_long$Year <- gsub("X", "", SDG_15_2_1_3_long$Year)
# view(SDG_15_2_1_3_long)

SDG_15_2_1_3_sorted <- SDG_15_2_1_3_long[order(SDG_15_2_1_3_long$Country,SDG_15_2_1_3_long$Year),]
# view(SDG_15_2_1_3_sorted)

# Add SDG subgoals in a new column
SDG_15_2_1_3_clean <- add_column(SDG_15_2_1_3_sorted, SDG = "15_2_1_3", .after = "countrycode")
View(SDG_15_2_1_3_clean)





#########################
## Merge into SDG 15.2 ##
#########################

SDG_15_2_clean <- rbind(SDG_15_2_1_1_clean, SDG_15_2_1_2_clean, SDG_15_2_1_3_clean)
view(SDG_15_2_clean)

# Sort data by Country, CountryCode, SDG, Year
SDG_15_2_cleansorted <- SDG_15_2_clean[order(SDG_15_2_clean$Country,SDG_15_2_clean$SDG,
                                             SDG_15_2_clean$countrycode,SDG_15_2_clean$Year),]
view(SDG_15_2_cleansorted)


##############################
## Clean dataset SDG 15.4.1 ##
##############################

# Load data
SDG_15_4_1 <- read.csv("short_SDG_15_4_1.csv")
# View(SDG_15_4_1)

# Drop irrelevant columns
SDG_15_4_1c = subset(SDG_15_4_1, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_4_1_long <- melt(SDG_15_4_1c, id.vars = c("Country","countrycode"))
# View(SDG_15_4_1_long)

# Add Year to a new column name
names(SDG_15_4_1_long)[names(SDG_15_4_1_long)=="variable"] <- "Year"
# View(SDG_15_4_1_long)

# Sort data by Country, year
attach(SDG_15_4_1_long)

SDG_15_4_1_long$Year <- gsub("X", "", SDG_15_4_1_long$Year)
# view(SDG_15_4_1_long)

# Add SDG subgoals in a new column
SDG_15_4_1_sorted <- add_column(SDG_15_4_1_long, SDG = "15_4_1", .after = "countrycode")
# View(SDG_15_4_1_sorted)

SDG_15_4_1_clean <- SDG_15_4_1_sorted[order(SDG_15_4_1_sorted$Country, SDG_15_4_1_sorted$SDG, SDG_15_4_1_sorted$Year),]
view(SDG_15_4_1_clean)


#########################
## Merge into SDG 15.4 ##
#########################

SDG_15_4_cleansorted <- SDG_15_4_1_clean
view(SDG_15_4_cleansorted)



##############################
## Clean dataset SDG 15.5.1 ##
##############################

# Load data
SDG_15_5_1 <- read.csv("short_SDG_15_5_1.csv")
# View(SDG_15_5_1)

# Drop irrelevant columns
SDG_15_5_1c = subset(SDG_15_5_1, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_5_1_long <- melt(SDG_15_5_1c, id.vars = c("Country","countrycode"))
# View(SDG_15_5_1_long)

# Add Year to a new column name
names(SDG_15_5_1_long)[names(SDG_15_5_1_long)=="variable"] <- "Year"
# View(SDG_15_5_1_long)

# Sort data by Country, year
attach(SDG_15_5_1_long)

SDG_15_5_1_long$Year <- gsub("X", "", SDG_15_5_1_long$Year)
# view(SDG_15_5_1_long)

# Add SDG subgoals in a new column
SDG_15_5_1_sorted <- add_column(SDG_15_5_1_long, SDG = "15_5_1", .after = "countrycode")
# View(SDG_15_5_1_sorted)

SDG_15_5_1_clean <- SDG_15_5_1_sorted[order(SDG_15_5_1_sorted$Country, SDG_15_5_1_sorted$SDG, SDG_15_5_1_sorted$Year),]
# view(SDG_15_5_1_clean)

#########################
## Merge into SDG 15.5 ##
#########################

SDG_15_5_cleansorted <- SDG_15_5_1_clean
view(SDG_15_5_cleansorted)



##############################
## Clean dataset SDG 15.6.1 ##
##############################

# SDG 15.6.1.1
SDG_15_6_1_1 <- read.csv("short_SDG_15_6_1_1.csv")
 View(SDG_15_6_1_1)

# Drop irrelevant columns
SDG_15_6_1_1c = subset(SDG_15_6_1_1, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_6_1_1_long <- melt(SDG_15_6_1_1c, id.vars = c("Country","countrycode"))
# View(SDG_15_6_1_1_long)

# Add Year to a new column name
names(SDG_15_6_1_1_long)[names(SDG_15_6_1_1_long)=="variable"] <- "Year"
# View(SDG_15_6_1_1_long)

# Sort data by Country, year
attach(SDG_15_6_1_1_long)

SDG_15_6_1_1_long$Year <- gsub("X", "", SDG_15_6_1_1_long$Year)
# view(SDG_15_6_1_1_long)

SDG_15_6_1_1_sorted <- SDG_15_6_1_1_long[order(SDG_15_6_1_1_long$Country,SDG_15_6_1_1_long$Year),]
# view(SDG_15_6_1_1_sorted)

# Add SDG subgoals in a new column
SDG_15_6_1_1_clean <- add_column(SDG_15_6_1_1_sorted, SDG = "15_6_1_1", .after = "countrycode")
View(SDG_15_6_1_1_clean)




# SDG 15.6.1.2
SDG_15_6_1_2 <- read.csv("short_SDG_15_6_1_2.csv")
# View(SDG_15_6_1_2)

# Drop irrelevant columns
SDG_15_6_1_2c = subset(SDG_15_6_1_2, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_6_1_2_long <- melt(SDG_15_6_1_2c, id.vars = c("Country","countrycode"))
# View(SDG_15_6_1_2_long)

# Add Year to a new column name
names(SDG_15_6_1_2_long)[names(SDG_15_6_1_2_long)=="variable"] <- "Year"
# View(SDG_15_6_1_2_long)

# Sort data by Country, year
attach(SDG_15_6_1_2_long)

SDG_15_6_1_2_long$Year <- gsub("X", "", SDG_15_6_1_2_long$Year)
# view(SDG_15_6_1_2_long)

SDG_15_6_1_2_sorted <- SDG_15_6_1_2_long[order(SDG_15_6_1_2_long$Country,SDG_15_6_1_2_long$Year),]
# view(SDG_15_6_1_2_sorted)

# Add SDG subgoals in a new column
SDG_15_6_1_2_clean <- add_column(SDG_15_6_1_2_sorted, SDG = "15_6_1_2", .after = "countrycode")
View(SDG_15_6_1_2_clean)



# SDG 15.6.1.3
SDG_15_6_1_3 <- read.csv("short_SDG_15_6_1_3.csv")
# View(SDG_15_6_1_3)

# Drop irrelevant columns
SDG_15_6_1_3c = subset(SDG_15_6_1_3, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_6_1_3_long <- melt(SDG_15_6_1_3c, id.vars = c("Country","countrycode"))
# View(SDG_15_6_1_3_long)

# Add Year to a new column name
names(SDG_15_6_1_3_long)[names(SDG_15_6_1_3_long)=="variable"] <- "Year"
# View(SDG_15_6_1_3_long)

# Sort data by Country, year
attach(SDG_15_6_1_3_long)

SDG_15_6_1_3_long$Year <- gsub("X", "", SDG_15_6_1_3_long$Year)
# view(SDG_15_6_1_3_long)

SDG_15_6_1_3_sorted <- SDG_15_6_1_3_long[order(SDG_15_6_1_3_long$Country,SDG_15_6_1_3_long$Year),]
# view(SDG_15_6_1_3_sorted)

# Add SDG subgoals in a new column
SDG_15_6_1_3_clean <- add_column(SDG_15_6_1_3_sorted, SDG = "15_6_1_3", .after = "countrycode")
View(SDG_15_6_1_3_clean)


# SDG 15.6.1.4
SDG_15_6_1_4 <- read.csv("short_SDG_15_6_1_4.csv")
View(SDG_15_6_1_4)

# Drop irrelevant columns
SDG_15_6_1_4c = subset(SDG_15_6_1_4, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_6_1_4_long <- melt(SDG_15_6_1_4c, id.vars = c("Country","countrycode"))
# View(SDG_15_6_1_4_long)

# Add Year to a new column name
names(SDG_15_6_1_4_long)[names(SDG_15_6_1_4_long)=="variable"] <- "Year"
# View(SDG_15_6_1_4_long)

# Sort data by Country, year
attach(SDG_15_6_1_4_long)

SDG_15_6_1_4_long$Year <- gsub("X", "", SDG_15_6_1_4_long$Year)
# view(SDG_15_6_1_4_long)

SDG_15_6_1_4_sorted <- SDG_15_6_1_4_long[order(SDG_15_6_1_4_long$Country,SDG_15_6_1_4_long$Year),]
# view(SDG_15_6_1_4_sorted)

# Add SDG subgoals in a new column
SDG_15_6_1_4_clean <- add_column(SDG_15_6_1_4_sorted, SDG = "15_6_1_4", .after = "countrycode")
View(SDG_15_6_1_4_clean)




# SDG 15.6.1.5
SDG_15_6_1_5 <- read.csv("short_SDG_15_6_1_5.csv")
# View(SDG_15_6_1_5)

# Drop irrelevant columns
SDG_15_6_1_5c = subset(SDG_15_6_1_5, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_6_1_5_long <- melt(SDG_15_6_1_5c, id.vars = c("Country","countrycode"))
# View(SDG_15_6_1_5_long)

# Add Year to a new column name
names(SDG_15_6_1_5_long)[names(SDG_15_6_1_5_long)=="variable"] <- "Year"
# View(SDG_15_6_1_5_long)

# Sort data by Country, year
attach(SDG_15_6_1_5_long)

SDG_15_6_1_5_long$Year <- gsub("X", "", SDG_15_6_1_5_long$Year)
# view(SDG_15_6_1_5_long)

SDG_15_6_1_5_sorted <- SDG_15_6_1_5_long[order(SDG_15_6_1_5_long$Country,SDG_15_6_1_5_long$Year),]
# view(SDG_15_6_1_5_sorted)

# Add SDG subgoals in a new column
SDG_15_6_1_5_clean <- add_column(SDG_15_6_1_5_sorted, SDG = "15_6_1_5", .after = "countrycode")
View(SDG_15_6_1_5_clean)



#########################
## Merge into SDG 15.6 ##
#########################

SDG_15_6_clean <- rbind(SDG_15_6_1_1_clean, SDG_15_6_1_2_clean, SDG_15_6_1_3_clean,
                        SDG_15_6_1_4_clean, SDG_15_6_1_5_clean)
view(SDG_15_6_clean)

# Sort data by Country, CountryCode, SDG, Year
SDG_15_6_cleansorted <- SDG_15_6_clean[order(SDG_15_6_clean$Country,SDG_15_6_clean$SDG,
                                             SDG_15_6_clean$countrycode,SDG_15_6_clean$Year),]
view(SDG_15_6_cleansorted)




##############################
## Clean dataset SDG 15.8.1 ##
##############################

# Load data
SDG_15_8_1 <- read.csv("short_SDG_15_8_1.csv")
# View(SDG_15_8_1)

# Drop irrelevant columns
SDG_15_8_1c = subset(SDG_15_8_1, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_8_1_long <- melt(SDG_15_8_1c, id.vars = c("Country","countrycode"))
# View(SDG_15_8_1_long)

# Add Year to a new column name
names(SDG_15_8_1_long)[names(SDG_15_8_1_long)=="variable"] <- "Year"
# View(SDG_15_8_1_long)

# Sort data by Country, year
attach(SDG_15_8_1_long)

SDG_15_8_1_long$Year <- gsub("X", "", SDG_15_8_1_long$Year)
  # view(SDG_15_8_1_long)

# Add SDG subgoals in a new column
SDG_15_8_1_sorted <- add_column(SDG_15_8_1_long, SDG = "15_8_1", .after = "countrycode")
  # View(SDG_15_8_1_sorted)

SDG_15_8_1_clean <- SDG_15_8_1_sorted[order(SDG_15_8_1_sorted$Country, SDG_15_8_1_sorted$SDG, SDG_15_8_1_sorted$Year),]
  # view(SDG_15_8_1_clean)

#########################
## Merge into SDG 15.8 ##
#########################

SDG_15_8_cleansorted <- SDG_15_8_1_clean
view(SDG_15_8_cleansorted)

##############################
## Clean dataset SDG 15.9.1 ##
##############################

# Load data
SDG_15_9_1 <- read.csv("short_SDG_15_9_1.csv")
View(SDG_15_9_1)

# Drop irrelevant columns
SDG_15_9_1c = subset(SDG_15_9_1, select = -c(X, CountryCode))

# Clean data with melting from wide to long
SDG_15_9_1_long <- melt(SDG_15_9_1c, id.vars = c("Country","countrycode"))
# View(SDG_15_9_1_long)

# Add Year to a new column name
names(SDG_15_9_1_long)[names(SDG_15_9_1_long)=="variable"] <- "Year"
# View(SDG_15_9_1_long)

# Sort data by Country, year
attach(SDG_15_9_1_long)

SDG_15_9_1_long$Year <- gsub("X", "", SDG_15_9_1_long$Year)
# view(SDG_15_9_1_long)

# Add SDG subgoals in a new column
SDG_15_9_1_sorted <- add_column(SDG_15_9_1_long, SDG = "15_9_1", .after = "countrycode")
# View(SDG_15_9_1_sorted)

SDG_15_9_1_clean <- SDG_15_9_1_sorted[order(SDG_15_9_1_sorted$Country, SDG_15_9_1_sorted$SDG, SDG_15_9_1_sorted$Year),]
# view(SDG_15_9_1_clean)

#########################
## Merge into SDG 15.9 ##
#########################

SDG_15_9_cleansorted <- SDG_15_9_1_clean
view(SDG_15_9_cleansorted)


############################
############################
## Merge into full SDG 15 ##
############################
############################

SDG_15_clean <- rbind(SDG_15_1_cleansorted, SDG_15_2_cleansorted,
                      SDG_15_4_cleansorted, SDG_15_5_cleansorted,
                      SDG_15_6_cleansorted, SDG_15_8_cleansorted,
                      SDG_15_9_cleansorted)
view(SDG_15_clean)

# Sort data by Country, CountryCode, SDG, Year
SDG_15_cleansorted <- SDG_15_clean[order(SDG_15_clean$Country,SDG_15_clean$SDG,
                                             SDG_15_clean$countrycode,SDG_15_clean$Year),]
view(SDG_15_cleansorted)


### Save the working dataset (MERGED SDG 15)
write.csv(SDG_15_cleansorted,"SDG_15_cleanmerged.csv")

#############################################################################################################################

### Data further cleaning and analysis ###

# number of unique countries
  paste ("Number of countries:", length(unique(SDG_15_cleansorted$Country)))

# number of unique country code
  paste ("Number of country code:", length(unique(SDG_15_cleansorted$countrycode)))

# list of countries
  paste("Country names:")
  paste(unique(SDG_15_cleansorted$Country), collapse=";")
  
  # # Country with a code 
  # paste("Country without a code:")
  # select(SDG_15_cleansorted$Country if (SDG_15_cleansorted$CountryCode[1:]==NA))
  # paste(unique((SDG_15_cleansorted$Country if (SDG_15_cleansorted$CountryCode==NA)))
  #       
  #       country2drop <- subset(SDG_15_cleansorted, CountryCode == NA, select)

  

########################################################################################


######################################
## Save for SDG 15.ab.1 and 15.ab.2 ##
######################################


########################################################################################




