
### Updated SDG 14 Data Configuration
### 2022-04-26
### Yuqian Zhang

# 
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir

setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG")
getwd()


source( file="script/reference.R" ); 


# Load data and cleaning --------------------------------------------------


# Create a country list for later use - list source: ISO 3166 - 3 digit ISO country/region code (#249)
df <- read.csv("data/SDSN2/world_entity.csv")
df2 <- df[c(4,3)]
names(df2)[1] <- "Entity"
names(df2)[2] <- "Code"
df2$Code_upper <- toupper(df2$Code)
countrylist <- df2[c(1,3)] # In total, there are 249 countries/regions initially considered in this project
names(countrylist)[2] <- "Code"


interest_year <- c(2000:2020)
empty_year <- data.frame(matrix(ncol=2, nrow=21))
colnames(empty_year) <- c("year", "value")
empty_year$year <- interest_year


country_sdg <- countrylist %>%
  add_column("14_1_1_1" = NA, "14_1_1_2" = NA, "14_2_1" = NA, "14_5_1" = NA,
             "14_6_1" = NA, "14_7_1" = NA, "14_a_1" = NA, "14_b_1" = NA,
             "14_c_1_1" = NA, "14_c_1_2" = NA)

country_sdg_long <- melt(country_sdg, id.vars = c("Entity", "Code"))
names(country_sdg_long)[3] <- "SDG"

country_sdg_2 <- country_sdg_long[,1:3]

country_sdg_year <- country_sdg_2 %>%
  add_column("2000" = NA, "2001" = NA, "2002" = NA, "2003" = NA, "2004" = NA,
             "2005" = NA, "2006" = NA, "2007" = NA, "2008" = NA, "2009" = NA,
             "2010" = NA, "2011" = NA, "2012" = NA, "2013" = NA, "2014" = NA,
             "2015" = NA, "2016" = NA, "2017" = NA, "2018" = NA, "2019" = NA,
             "2020" = NA)
country_sdg_year_long <- melt(country_sdg_year, id.vars = c("Entity", "Code", "SDG"))
names(country_sdg_year_long)[4] <- "Year"

full_country_sdg_year <- country_sdg_year_long[,1:4]
full_country_sdg_year$Value <- NA
# write.csv(full_country_sdg_year, "data/SDSN2/empty_full_country_sdg_year.csv")


##################
### SDG_14_1_1 ###
##################

N_SDG_14_1_1_1 <- read.csv("data/SDSN2/14.1.1.1.beach-litter.csv")
names(N_SDG_14_1_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_1_1_1 <- add_column(N_SDG_14_1_1_1, SDG = "14_1_1_1", .after = "Code")
N_SDG_14_1_1_1 <- subset(N_SDG_14_1_1_1, nchar(as.character(N_SDG_14_1_1_1$Code))==3)

N_SDG_14_1_1_1$log.value <- log(N_SDG_14_1_1_1$value)
# hist(log(N_SDG_14_1_1_1$value))
# summary(N_SDG_14_1_1_1)
N_SDG_14_1_1_1$log.percent <- N_SDG_14_1_1_1$log.value/max(N_SDG_14_1_1_1$log.value)
N_SDG_14_1_1_1$log.percent.reverse <- (1-N_SDG_14_1_1_1$log.percent)*100
N_SDG_14_1_1_1$value <- N_SDG_14_1_1_1$log.percent.reverse # higher the score, the better (less litter on beach)

N_SDG_14_1_1_1 <- N_SDG_14_1_1_1[,1:5]
# length(unique(N_SDG_14_1_1_1$Code)) # unique country code = 141
# write.csv(N_SDG_14_1_1_1,"data/SDSN2/N_SDG_14_1_1_1.csv")

#### if NAs, keep NAs OR assign the value of 1 to those countries!!!

N_SDG_14_1_1_2 <- read.csv("data/SDSN2/14.1.1.2.chlorophyll-a-deviation-from-the-global-average.csv")
names(N_SDG_14_1_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_1_1_2 <- add_column(N_SDG_14_1_1_2, SDG = "14_1_1_2", .after = "Code")
N_SDG_14_1_1_2 <- subset(N_SDG_14_1_1_2, nchar(as.character(N_SDG_14_1_1_2$Code))==3)
N_SDG_14_1_1_2$value <- 100 - N_SDG_14_1_1_2$value # higher the score, the better (less % of chlorophyll)
# length(unique(N_SDG_14_1_1_2$Code)) # unique country code = 180
# write.csv(N_SDG_14_1_1_2,"data/SDSN2/N_SDG_14_1_1_2.csv")

# Integrate to SDG_14_1_1
N_SDG_14_1_1 <- rbind(N_SDG_14_1_1_1, N_SDG_14_1_1_2)
N_SDG_14_1_1 <- subset(N_SDG_14_1_1, nchar(as.character(N_SDG_14_1_1$Code))==3)
# length(unique(N_SDG_14_1_1$Code)) # unique country code = 198
# write.csv(N_SDG_14_1_1,"data/SDSN2/N_SDG_14_1_1.csv")

### All good to fill NAs ###

 
##################
### SDG_14_2_1 ###
##################

N_SDG_14_2_1 <- read.csv("data/SDSN2/14.2.1.countries-using-ecosystem-based-approaches-to-manage-marine-areas.csv")
names(N_SDG_14_2_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_2_1 <- add_column(N_SDG_14_2_1, SDG = "14_2_1", .after = "Code")

# If the year > 2020, set as 2020
N_SDG_14_2_1$Year[N_SDG_14_2_1$Year >= 2020] <- 2020

# Convert all values in 100%
N_SDG_14_2_1$value <- N_SDG_14_2_1$value *100
# length(unique(N_SDG_14_2_1$Code)) # unique country code = 18
# write.csv(N_SDG_14_2_1,"data/SDSN2/N_SDG_14_2_1.csv")

### All good to fill NAs ###
 

##################
### SDG_14_5_1 ###
##################

Np_SDG_14_5_1 <- read.csv("data/SDSN2/SDG_14_5.csv", skip = 1)

# Drop irrelevant columns and rename columns
Np_SDG_14_5_1c <- Np_SDG_14_5_1[,c(3,7,11:30)]
names(Np_SDG_14_5_1c)[1] <- "SDG"
names(Np_SDG_14_5_1c)[2] <- "Entity"
Np_SDG_14_5_1c$SDG <- "14_5_1"

# Clean data with melting from wide to long
Np_SDG_14_5_1clong <- melt(Np_SDG_14_5_1c, id.vars = c("Entity", "SDG"))

# Add Year to a new column name
names(Np_SDG_14_5_1clong)[names(Np_SDG_14_5_1clong)=="variable"] <- "Year"

# Clean column name for YEAR
Np_SDG_14_5_1clong$Year <- gsub("X", "", Np_SDG_14_5_1clong$Year)

# Add Country Code column
N_SDG_14_5_1 <- merge(countrylist, Np_SDG_14_5_1clong, by="Entity", all=TRUE)

N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "United States Virgin Islands"] <- "VIR"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "United Republic of Tanzania"] <- "TZA"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "Republic of Korea"] <- "KOR"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "Democratic People's Republic of Korea"] <- "PRK"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "Curaçao"] <- "CUW"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "Côte d'Ivoire"] <- "CIV"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "China, Macao Special Administrative Region"] <- "MAC"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "China, Hong Kong Special Administrative Region"] <- "HKG"
N_SDG_14_5_1$Code[N_SDG_14_5_1$Entity == "British Virgin Islands"] <- "VGB"

N_SDG_14_5_1 <- na.omit(N_SDG_14_5_1)
# length(unique(N_SDG_14_5_1$Code)) # unique country code = 169
# write.csv(N_SDG_14_5_1, "data/SDSN2/N_SDG_14_5_1.csv")


### All good to fill NAs ###


##################
### SDG_14_6_1 ###
##################

N_SDG_14_6_1 <- read.csv("data/SDSN2/14.6.1.regulation-illegal-fishing.csv")
names(N_SDG_14_6_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_6_1 <- add_column(N_SDG_14_6_1, SDG = "14_6_1", .after = "Code")
N_SDG_14_6_1 <- subset(N_SDG_14_6_1, nchar(as.character(N_SDG_14_6_1$Code))==3)
N_SDG_14_6_1$value <- N_SDG_14_6_1$value/(max(N_SDG_14_6_1$value)) *100

# Only keep the max year of 2020, if year > 2020, mean of 2020 and 2022
# Convert from long to wide
N_SDG_14_6_1_wide <- N_SDG_14_6_1 %>%
  spread(Year, value)
N_SDG_14_6_1_wide$`2021` <- 0.5*(N_SDG_14_6_1_wide$`2020` + N_SDG_14_6_1_wide$`2022`) 

# Loop through all countries, and assign values if necessary - 
# if 2021 != na, assign value (2021 -> 2020);
# if 2021 == na && 2022 != na, assign value (2022 -> 2020)
# Otherwise, keep the original value of 2020 (either a number or NA)
for (i in 1:length(N_SDG_14_6_1_wide$Code)){
  if (!(is.na(N_SDG_14_6_1_wide$`2021`[i]))){
    N_SDG_14_6_1_wide$`2020.2`[i] <- N_SDG_14_6_1_wide$`2021`[i]
  }
  else if (!(is.na(N_SDG_14_6_1_wide$`2022`[i]))){
    N_SDG_14_6_1_wide$`2020.2`[i] <- N_SDG_14_6_1_wide$`2022`[i]
  }
}

# Drop irrelevant columns
N_SDG_14_6_1_wide <- N_SDG_14_6_1_wide[,c(1:4,8)]
names(N_SDG_14_6_1_wide)[5] <- 2020

# Convert back from wide to long
N_SDG_14_6_1_long <- melt(N_SDG_14_6_1_wide, id.vars = c("Entity","Code","SDG"))
names(N_SDG_14_6_1_long)[4] <- "Year"

N_SDG_14_6_1 <- N_SDG_14_6_1_long

# length(unique(N_SDG_14_6_1$Code)) # unique country code = 133
# write.csv(N_SDG_14_6_1,"data/SDSN2/N_SDG_14_6_1.csv")

### All good to fill NAs ###


##################
### SDG_14_7_1 ###
##################

N_SDG_14_7_1 <- read.csv("data/SDSN2/14.7.1.sustainable-fisheries-as-a-proportion-of-gdp.csv")
names(N_SDG_14_7_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_7_1 <- add_column(N_SDG_14_7_1, SDG = "14_7_1", .after = "Code")
N_SDG_14_7_1 <- subset(N_SDG_14_7_1, nchar(as.character(N_SDG_14_7_1$Code))==3)

# length(unique(N_SDG_14_7_1$Code)) # unique country code = 115
# write.csv(N_SDG_14_7_1,"data/SDSN2/N_SDG_14_7_1.csv")

### All good to fill NAs ###


##################
### SDG_14_a_1 ###
##################

N_SDG_14_a_1 <- read.csv("data/SDSN2/14.a.1.ocean-research-funding.csv")
names(N_SDG_14_a_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_a_1 <- add_column(N_SDG_14_a_1, SDG = "14_a_1", .after = "Code")
N_SDG_14_a_1 <- subset(N_SDG_14_a_1, nchar(as.character(N_SDG_14_a_1$Code))==3)

# length(unique(N_SDG_14_a_1$Code)) # unique country code = 33
# write.csv(N_SDG_14_a_1,"data/SDSN2/N_SDG_14_a_1.csv")

### All good to fill NAs ###


##################
### SDG_14_b_1 ###
##################

N_SDG_14_b_1 <- read.csv("data/SDSN2/14.b.1.protection-of-the-rights-of-small-scale-fisheries.csv")
names(N_SDG_14_b_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_b_1 <- add_column(N_SDG_14_b_1, SDG = "14_b_1", .after = "Code")
N_SDG_14_b_1 <- subset(N_SDG_14_b_1, nchar(as.character(N_SDG_14_b_1$Code))==3)
N_SDG_14_b_1$value <- N_SDG_14_b_1$value/(max(N_SDG_14_b_1$value)) *100

# Only keep the max year of 2020, if year > 2020, mean of 2020 and 2022
# Convert from long to wide
N_SDG_14_b_1_wide <- N_SDG_14_b_1 %>%
  spread(Year, value)
N_SDG_14_b_1_wide$`2021` <- 0.5*(N_SDG_14_b_1_wide$`2020` + N_SDG_14_b_1_wide$`2022`) 

# Loop through all countries, and assign values if necessary - 
# if 2021 != na, assign value (2021 -> 2020);
# if 2021 == na && 2022 != na, assign value (2022 -> 2020)
# Otherwise, keep the original value of 2020 (either a number or NA)
for (i in 1:length(N_SDG_14_b_1_wide$Code)){
  if (!(is.na(N_SDG_14_b_1_wide$`2021`[i]))){
    N_SDG_14_b_1_wide$`2020.2`[i] <- N_SDG_14_b_1_wide$`2021`[i]
  }
  else if (!(is.na(N_SDG_14_b_1_wide$`2022`[i]))){
    N_SDG_14_b_1_wide$`2020.2`[i] <- N_SDG_14_b_1_wide$`2022`[i]
  }
}

# Drop irrelevant columns
N_SDG_14_b_1_wide <- N_SDG_14_b_1_wide[,c(1:4,8)]
names(N_SDG_14_b_1_wide)[5] <- 2020

# Convert back from wide to long
N_SDG_14_b_1_long <- melt(N_SDG_14_b_1_wide, id.vars = c("Entity","Code","SDG"))
names(N_SDG_14_b_1_long)[4] <- "Year"

N_SDG_14_b_1 <- N_SDG_14_b_1_long

# length(unique(N_SDG_14_b_1$Code)) # unique country code = 150
# write.csv(N_SDG_14_b_1,"data/SDSN2/N_SDG_14_b_1.csv")

### All good to fill NAs ###


##################
### SDG_14_c_1 ###
##################

N_SDG_14_c_1_1 <- read.csv("data/SDSN2/14.c.1.1.ratification-and-accession-to-unclos.csv")
names(N_SDG_14_c_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_c_1_1 <- add_column(N_SDG_14_c_1_1, SDG = "14_c_1_1", .after = "Code")
N_SDG_14_c_1_1 <- subset(N_SDG_14_c_1_1, nchar(as.character(N_SDG_14_c_1_1$Code))==3)
N_SDG_14_c_1_1$Year <- 2020 # If year >= 2020, reset as 2020

# length(unique(N_SDG_14_c_1_1$Code)) # unique country code = 45
# write.csv(N_SDG_14_c_1_1,"data/SDSN2/N_SDG_14_c_1_1.csv")


N_SDG_14_c_1_2 <- read.csv("data/SDSN2/14.c.1.2.implementation-of-unclos.csv")
names(N_SDG_14_c_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_c_1_2 <- add_column(N_SDG_14_c_1_2, SDG = "14_c_1_2", .after = "Code")
N_SDG_14_c_1_2 <- subset(N_SDG_14_c_1_2, nchar(as.character(N_SDG_14_c_1_2$Code))==3)
N_SDG_14_c_1_2$Year <- 2020 # If year >= 2020, reset as 2020

# length(unique(N_SDG_14_c_1_2$Code)) # unique country code = 43
# write.csv(N_SDG_14_c_1_2,"data/SDSN2/N_SDG_14_c_1_2.csv")

# Integrate to SDG_14_c_1
N_SDG_14_c_1 <- rbind(N_SDG_14_c_1_1, N_SDG_14_c_1_2)
N_SDG_14_c_1 <- subset(N_SDG_14_c_1, nchar(as.character(N_SDG_14_c_1$Code))==3)
# length(unique(N_SDG_14_c_1$Code)) # unique country code = 45
# write.csv(N_SDG_14_c_1,"data/SDSN2/N_SDG_14_c_1.csv")

### All good to fill NAs ###


##################
# SDG_14_Combine #
##################

N_SDG_14_combraw <- rbind(N_SDG_14_1_1, N_SDG_14_2_1, N_SDG_14_5_1,
                          N_SDG_14_6_1, N_SDG_14_7_1, N_SDG_14_a_1,
                          N_SDG_14_b_1, N_SDG_14_c_1) 
names(N_SDG_14_combraw)[1] <- "Country"
names(N_SDG_14_combraw)[5] <- "Value"

# write.csv(N_SDG_14_combraw, "data/SDSN2/N_SDG_14_combraw.csv")

# N_SDG_14_combraw <- read.csv("data/SDSN2/N_SDG_14_combraw.csv")

N_SDG_14_comb_00_20 <- subset(N_SDG_14_combraw, N_SDG_14_combraw$Year %in% interest_year)
# write.csv(N_SDG_14_comb_00_20, "data/SDSN2/N_SDG_14_comb_00_20.csv")

# N_SDG_14_comb_00_20$Key <- paste(N_SDG_14_comb_00_20$Code,
#                                  N_SDG_14_comb_00_20$SDG, N_SDG_14_comb_00_20$Year,
#                                  sep="_")
# 
# 
# full_country_sdg_year$Key <- paste(full_country_sdg_year$Code,
#                                    full_country_sdg_year$SDG, full_country_sdg_year$Year,
#                                    sep="_")

# # The double for loops took too much computing capacity, thus inefficient;
# # the above data set was achieved through Python pandas dictionary, neatly!
# 
# 
# # N_SDG_14_comb_00_20 <- read.csv("data/SDSN/N_SDG_14_comb_00_20.csv")
# 
# ### The new dataset has been created from Python
# 
N_SDG_14_2000_2020 <- read.csv("data/SDSN2/update_full_country_sdg_year.csv")

N_SDG_14_2000_2020_clean <- N_SDG_14_2000_2020[,c(3:6,9)]
names(N_SDG_14_2000_2020_clean)[1] <- "Country"
names(N_SDG_14_2000_2020_clean)[5] <- "value"
# write.csv(N_SDG_14_2000_2020_clean, "data/SDSN2/SDG_14_all_unfill.csv")
# # readr::write_csv()


##################
# SDG_14_fill_NA #
##################

# load data
csv <- "data/SDSN2/SDG_14_all_unfill.csv"
unfill_SDG_14 <- readr::read_csv(csv)

## Indicator/sub_indicator that fits imputeTS interpolation method

# Continuous data suitable for imputation - sub/indicators
# 14.1.1.1
# 14.1.1.2
# 14.5.1
# 14.6.1
# 14.7.1
# 14.a.1
# 14.b.1
# 14.c.1.1
# 14.c.1.2

# ## Binary data NOT suitable for imputation - sub/indicators
# ## 14.2.1


# unique(unfill_SDG_14$SDG)
# create a suitable impuation list
SDG_Imputeable <- c("14_1_1_1", "14_1_1_2", "14_5_1", "14_6_1",
                    "14_7_1", "14_a_1", "14_b_1", "14_c_1_1", "14_c_1_2")

## data with continuous indicator
dat1 <- unfill_SDG_14 %>%
  dplyr::filter(SDG %in% SDG_Imputeable);

## data with binary indicator
dat2 <- unfill_SDG_14 %>%
  dplyr::filter(!SDG %in% SDG_Imputeable);

## Impute continuous data of sub/indicators
source("script/function_fill_na_continuous.R")
dat1_filled <- function_fill_na_continuous(dat1) ### add return() in function
dat1_filled <- dat1_filled[,c(2:5,8)]
names(dat1_filled)[5] <- "Value"
# write.csv(dat1_filled, "data/SDSN2/SDG_14_continuous_filled.csv")

## Manually fill binary data of sub/indicators
source("script/function_fill_na_binary.R")
dat2_filled <- function_fill_na_binary(dat2) ### add return() in function
dat2_filled <- dat2_filled[,c(2:5,7)]
# write.csv(dat2_filled, "data/SDSN2/SDG_14_binary_filled.csv")

SDG_14_all_filled <- rbind(dat1_filled, dat2_filled)
# write.csv(SDG_14_all_filled, "data/SDSN2/SDG_14_complete.csv")


##################
## SDG_14_score ##
##################

## Calculate SDG scores based on sub/indicators
SDG_14_all_filled <- read.csv("data/SDSN2/SDG_14_complete.csv")

# create a new column for SDG_main indicator
SDG_14_all_filled$SDG_main <- substr(SDG_14_all_filled$SDG,1,4)
# create a new column for SDG_sub indicator
SDG_14_all_filled$SDG_sub <- substr(SDG_14_all_filled$SDG,1,6)


# create empty data frames for later use
df1 <- data.frame()
df_sub_total <- data.frame()
df_main_total <- data.frame()
df_overall_total <- data.frame()
n <- length(unique(SDG_14_all_filled$Code)) ## Code - county iso code
uni_code <- unique(SDG_14_all_filled$Code) ## unique country list by code

# loop through each country and calculate their SDG scores by sub and main indicaotrs seperately
for (i in seq(1:n)) {
  print(i)
  ### loop each county Code
  print(uni_code[i])

  df1 <- subset(SDG_14_all_filled, SDG_14_all_filled$Code==uni_code[i])
  df_sub <- aggregate(Value~Country+Code+Year+SDG_sub, data=df1, mean, na.rm=TRUE)
  df_sub$SDG_main <- substr(df_sub$SDG_sub,1,4)
  df_main <- aggregate(Value~Country+Code+Year+SDG_main, data=df_sub, mean, na.rm=TRUE)
  df_main$SDG <- substr(df_main$SDG,1,2)
  df_overall <- aggregate(Value~Country+Code+Year+SDG, data=df_main, mean, na.rm=TRUE)

  df_sub_total <- rbind(df_sub_total, df_sub)
  df_main_total <- rbind(df_main_total, df_main)
  df_overall_total <- rbind(df_overall_total, df_overall)

  # return(df_sub_total)
  # return(df_main_total)
}

SDG_14_score_by_sub_indicator <- df_sub_total[,c(1:5)]
# write.csv(SDG_14_score_by_sub_indicator, "data/SDSN2/SDG_14_score_by_sub_indicator.csv")

SDG_14_score_by_main_indicator <- df_main_total[,c(1:5)]
# write.csv(SDG_14_score_by_main_indicator, "data/SDSN2/SDG_14_score_by_main_indicator.csv")

## SDG 14 overall score by country
SDG_14_score_overall <- df_overall_total
# write.csv(SDG_14_score_overall, "data/SDSN2/SDG_14_score_overall.csv")



## Test -----------------------------------------------------------------------

# create a subset for only ABW country from full data set
df_ABW <- subset(SDG_14_all_filled, SDG_14_all_filled$Code=="ABW")

# Calculate mean by SDG_sub indicator
df_sub <- aggregate(Value~Country+Code+Year+SDG_sub, data=df_ABW, mean, na.rm=TRUE)

# Create a list for SDG_main indicator only
df_sub$SDG_main <- substr(df_sub$SDG_sub,1,4)

# Calculate mean by SDG_main indicator
df_main <- aggregate(Value~Country+Code+Year+SDG_main, data=df_sub, mean, na.rm=TRUE)


#####################
### Visualization ###
#####################

### SDG 14 variation by each indicator with pool data ### ----------------------

### Unfilled data ###
N_SDG_14_comb_00_20 <- read.csv("data/SDSN2/N_SDG_14_comb_00_20.csv")
N_SDG_14_comb_00_20$Year <- as.factor(N_SDG_14_comb_00_20$Year)
# Idea 1 - Scatter point for SDG by indicators
ind_analysis <- aggregate(N_SDG_14_comb_00_20[,6],
                          list(N_SDG_14_comb_00_20$Year,N_SDG_14_comb_00_20$SDG),
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

plot1 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 14 Indicator Average Value Change over Years with Unfilled Data",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Indicator Value");

# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot1


### Filled data ###
N_SDG_14_2000_2020_all <- read.csv("data/SDSN2/SDG_14_complete.csv")
N_SDG_14_2000_2020_all$Year <- as.factor(N_SDG_14_2000_2020_all$Year)
# Idea 1 - Scatter point for SDG by indicators
ind_analysis2 <- aggregate(N_SDG_14_2000_2020_all[,6],
                           list(N_SDG_14_2000_2020_all$Year,N_SDG_14_2000_2020_all$SDG),
                           FUN = mean)
names(ind_analysis2)[1] <- "Year"
names(ind_analysis2)[2] <- "SDG"
names(ind_analysis2)[3] <- "Value"

plot2 <- ggplot(data = ind_analysis2, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 14 Indicator Average Value Change over Years with Imputed Data",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Indicator Value");

# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot2                                         ### Problem needs to look into!!!

### SDG 14 variation by each indicator with modified data ### ------------------

## By main indicator
SDG_14_score_by_main_indicator <- read.csv("data/SDSN2/SDG_14_score_by_main_indicator.csv")
# SDG_14_score_by_main_indicator$Year <- as.factor(SDG_14_score_by_main_indicator$Year)

# Idea 1 - Scatter point for SDG by indicators
ind_analysis <- aggregate(SDG_14_score_by_main_indicator$Value,
                          list(SDG_14_score_by_main_indicator$Year,SDG_14_score_by_main_indicator$SDG_main),
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

plot3 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 14 Main Indicator Average Value Change over Years",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Main Indicator Value");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot3

## By sub indicator
SDG_14_score_by_sub_indicator <- read.csv("data/SDSN2/SDG_14_score_by_sub_indicator.csv")
# SDG_14_score_by_sub_indicator$Year <- as.factor(SDG_14_score_by_sub_indicator$Year)

# Idea 1 - Scatter point for SDG by indicators
ind_analysis1 <- aggregate(SDG_14_score_by_sub_indicator$Value,
                           list(SDG_14_score_by_sub_indicator$Year,SDG_14_score_by_sub_indicator$SDG_sub),
                           FUN = mean)
names(ind_analysis1)[1] <- "Year"
names(ind_analysis1)[2] <- "SDG"
names(ind_analysis1)[3] <- "Value"

plot4 <- ggplot(data = ind_analysis1, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 14 Sub Indicator Average Value Change over Years",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Sub Indicator Value");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot4

### SDG 14 variation country ### -----------------------------------------------

# Idea 3 - boxplot

SDG_14_score_overall <- read.csv("data/SDSN2/SDG_14_score_overall.csv")

SDG_14_score_overall$Year <- as.factor(SDG_14_score_overall$Year)
plot5 <- ggplot(data = SDG_14_score_overall) +
  stat_boxplot(mapping=aes(x=Year, y=Value),
               na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=Year, y=Value),
               na.rm = TRUE) +
  # annotate(geom = "text",
  #          )
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Score Change over Years",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Score");
plot5


# Idea 4 - violin plot
plot6 <- ggplot(data = SDG_14_score_overall) +
  geom_violin(mapping=aes(x=Year, y=Value),
              na.rm = TRUE) +
  # geom_smooth(method="lm") +
  theme_bw() +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Value Change over Years",
       subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Score");
plot6

# Idea 5 - SDG score line graph with SD as shaded area

ind_analysis3 <- aggregate(SDG_14_score_overall$Value, 
                           list(SDG_14_score_overall$Year,SDG_14_score_overall$SDG), 
                           FUN = mean)
names(ind_analysis3)[1] <- "Year"
names(ind_analysis3)[2] <- "SDG"
names(ind_analysis3)[3] <- "Value"

sd <- aggregate(SDG_14_score_overall$Value, 
                list(SDG_14_score_overall$Year,SDG_14_score_overall$SDG),
                FUN = sd)

ind_analysis3$sd <- sd$x
ind_analysis3$lower <- ind_analysis3$Value - 0.5*ind_analysis3$sd
ind_analysis3$upper <- ind_analysis3$Value + 0.5*ind_analysis3$sd


plot7 <- ggplot(data = ind_analysis3, aes(x = Year, y = Value)) +
  geom_ribbon(aes(ymin = lower,ymax = upper), alpha = 0.2, fill = "royalblue") +
  geom_line(color='royalblue') +
  # scale_fill_manual(values='seagreen', name="fill") + 
  geom_point(color='royalblue') +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1))+
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Score Change over Years",
       subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Score");
plot7




### SDG 14 map of all countries/regions ----------------------------------------

## Selected years of SDG scores for all countries, 2000, 2005, 2010, 2015, 2020
SDG_14_score_overall <- read.csv("data/SDSN2/SDG_14_score_overall.csv")
# SDG_14_score_overall_wide <- spread(SDG_14_score_overall, key = Year, value = Value)
SDG_14_score_overall_2000 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2000',]
SDG_14_score_overall_2005 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2005',]
SDG_14_score_overall_2010 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2010',]
SDG_14_score_overall_2015 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2015',]
SDG_14_score_overall_2020 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2020',]

# Create a data set of country's SDG change (slope of scores over years)
dat <- data.table(SDG_14_score_overall)
SDG_14_change <- dat[,as.list(coef(lm(Value~Year))), by=Code]
names(SDG_14_change)[3] <- 'Change'
# summary(SDG_14_change)

## shp ---

# head(shp)
shp <- ne_countries(scale = 'medium', type = 'countries', returnclass = 'sf') %>%
  dplyr::select(name, type, iso_a3, economy, income_grp) 

# Remove Antarctica
shp_nATA <- subset(shp, name != "Antarctica")
names(shp_nATA)[3] <- 'Code'

# Merge shp file with SDG score in 2000
shp_nATA_2000 <- merge(shp_nATA, SDG_14_score_overall_2000, by='Code')

plot8 <- ggplot(shp_nATA_2000) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2000") +
  scale_fill_distiller(palette='YlGnBu', direction = 1) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
plot8

# Merge shp file with SDG score in 2005
shp_nATA_2005 <- merge(shp_nATA, SDG_14_score_overall_2005, by='Code')

plot9 <- ggplot(shp_nATA_2005) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2005") +
  scale_fill_distiller(palette='YlGnBu', direction = 1) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
plot9

# Merge shp file with SDG score in 2010
shp_nATA_2010 <- merge(shp_nATA, SDG_14_score_overall_2010, by='Code')

plot10 <- ggplot(shp_nATA_2010) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2010") +
  scale_fill_distiller(palette='YlGnBu', direction = 1) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
plot10

# Merge shp file with SDG score in 2015
shp_nATA_2015 <- merge(shp_nATA, SDG_14_score_overall_2015, by='Code')

plot11 <- ggplot(shp_nATA_2015) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2015") +
  scale_fill_distiller(palette='YlGnBu', direction = 1) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
plot11


# Merge shp file with SDG score in 2020
shp_nATA_2020 <- merge(shp_nATA, SDG_14_score_overall_2020, by='Code')

plot12 <- ggplot(shp_nATA_2020) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2020") +
  scale_fill_distiller(palette='YlGnBu', direction = 1) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
plot12


# Merge shp file with SDG score change
shp_nATA_change <- merge(shp_nATA, SDG_14_change, by='Code')

plot13 <- ggplot(shp_nATA_change) +
  geom_sf(aes(fill=Change), size=0.1) + 
  ggtitle("Global SDG 14 change between 2000 and 2020") +
  scale_fill_distiller(palette='PuBu', direction = 1) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
plot13


# plot(shp['iso_a3'])
# # shp_iso_na <- shp %>%
# #   dplyr::filter(is.na(iso_a3))
# # shp_iso_na$name
# 
# # unique(shp$iso_a3)
# 
# shp %>%
#   ggplot() +
#   geom_sf(aes(fill = group), size = 0.1, color = 'gray50')
# 
# # diff <- setdiff(shp$iso_a3, SDG_14_score_overall$Code)
# # diff
# # 
# # shp$name[is.na(shp$iso_a3)]

