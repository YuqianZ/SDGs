
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
             "14_6_1" = NA, "14_7_1" = NA, "14_a_1" = NA, "14_b_1" = NA)#,
             #"14_c_1_1" = NA, "14_c_1_2" = NA) ## Due to data deficiency, 14_c_1 was removed in analysis

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

# Normalization
top_5 <- tail(sort(N_SDG_14_1_1_1$log.value),5)
bot_5 <- tail(sort(N_SDG_14_1_1_1$log.value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_1_1_1$norm_log.value <- (N_SDG_14_1_1_1$log.value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_1_1_1$norm_log.value[N_SDG_14_1_1_1$norm_log.value >1] <- 1
N_SDG_14_1_1_1$norm_log.value[N_SDG_14_1_1_1$norm_log.value <0] <- 0

# N_SDG_14_1_1_1$log.percent <- N_SDG_14_1_1_1$log.value/max(N_SDG_14_1_1_1$log.value)

N_SDG_14_1_1_1$norm_log.percent.reverse <- (1-N_SDG_14_1_1_1$norm_log.value)
N_SDG_14_1_1_1$value_norm <- N_SDG_14_1_1_1$norm_log.percent.reverse # higher the score, the better (less litter on beach)

N_SDG_14_1_1_1 <- N_SDG_14_1_1_1[,c(1:5,9)]

# length(unique(N_SDG_14_1_1_1$Code)) # unique country code = 141
# write.csv(N_SDG_14_1_1_1,"data/SDSN2/N_SDG_14_1_1_1.csv")

#### if NAs, keep NAs (OR assign the value of 1 to those countries!!!)

N_SDG_14_1_1_2 <- read.csv("data/SDSN2/14.1.1.2.chlorophyll-a-deviation-from-the-global-average.csv")
names(N_SDG_14_1_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_14_1_1_2 <- add_column(N_SDG_14_1_1_2, SDG = "14_1_1_2", .after = "Code")
N_SDG_14_1_1_2 <- subset(N_SDG_14_1_1_2, nchar(as.character(N_SDG_14_1_1_2$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_14_1_1_2$value),5)
bot_5 <- tail(sort(N_SDG_14_1_1_2$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_1_1_2$value_norm <- (N_SDG_14_1_1_2$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_1_1_2$value_norm[N_SDG_14_1_1_2$value_norm >1] <- 1
N_SDG_14_1_1_2$value_norm[N_SDG_14_1_1_2$value_norm <0] <- 0


N_SDG_14_1_1_2$value_norm <- 1 - N_SDG_14_1_1_2$value_norm # higher the score, the better (less % of chlorophyll)
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

N_SDG_14_2_1 <- subset(N_SDG_14_2_1, nchar(as.character(N_SDG_14_2_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_14_2_1$value),5)
bot_5 <- tail(sort(N_SDG_14_2_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_2_1$value_norm <- (N_SDG_14_2_1$value - lower) / (upper - lower)

# All value should be 1
N_SDG_14_2_1$value_norm <- 1


# Convert all values in 100%
# N_SDG_14_2_1$value <- N_SDG_14_2_1$value *100
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

N_SDG_14_5_1 <- subset(N_SDG_14_5_1, nchar(as.character(N_SDG_14_5_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_14_5_1$value),5)
bot_5 <- tail(sort(N_SDG_14_5_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_5_1$value_norm <- (N_SDG_14_5_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_5_1$value_norm[N_SDG_14_5_1$value_norm >1] <- 1
N_SDG_14_5_1$value_norm[N_SDG_14_5_1$value_norm <0] <- 0

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


# Normalization
top_5 <- tail(sort(N_SDG_14_6_1$value),5)
bot_5 <- tail(sort(N_SDG_14_6_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_6_1$value_norm <- (N_SDG_14_6_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_6_1$value_norm[N_SDG_14_6_1$value_norm >1] <- 1
N_SDG_14_6_1$value_norm[N_SDG_14_6_1$value_norm <0] <- 0

# N_SDG_14_6_1$value <- N_SDG_14_6_1$value/(max(N_SDG_14_6_1$value)) *100

N_SDG_14_6_1$value <- N_SDG_14_6_1$value_norm

N_SDG_14_6_1 <- N_SDG_14_6_1[,c(1:5)]

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

N_SDG_14_6_1$value_norm <- N_SDG_14_6_1$value


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

N_SDG_14_7_1 <- subset(N_SDG_14_7_1, nchar(as.character(N_SDG_14_7_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_14_7_1$value),5)
bot_5 <- tail(sort(N_SDG_14_7_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_7_1$value_norm <- (N_SDG_14_7_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_7_1$value_norm[N_SDG_14_7_1$value_norm >1] <- 1
N_SDG_14_7_1$value_norm[N_SDG_14_7_1$value_norm <0] <- 0

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

# Normalization
top_5 <- tail(sort(N_SDG_14_a_1$value),5)
bot_5 <- tail(sort(N_SDG_14_a_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_a_1$value_norm <- (N_SDG_14_a_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_a_1$value_norm[N_SDG_14_a_1$value_norm >1] <- 1
N_SDG_14_a_1$value_norm[N_SDG_14_a_1$value_norm <0] <- 0

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

# Normalization
top_5 <- tail(sort(N_SDG_14_b_1$value),5)
bot_5 <- tail(sort(N_SDG_14_b_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_14_b_1$value_norm <- (N_SDG_14_b_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_14_b_1$value_norm[N_SDG_14_b_1$value_norm >1] <- 1
N_SDG_14_b_1$value_norm[N_SDG_14_b_1$value_norm <0] <- 0

# N_SDG_14_b_1$value <- N_SDG_14_b_1$value/(max(N_SDG_14_b_1$value)) *100

N_SDG_14_b_1$value <- N_SDG_14_b_1$value_norm

N_SDG_14_b_1 <- N_SDG_14_b_1[,c(1:5)]

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

N_SDG_14_b_1$value_norm <- N_SDG_14_b_1$value

# length(unique(N_SDG_14_b_1$Code)) # unique country code = 150
# write.csv(N_SDG_14_b_1,"data/SDSN2/N_SDG_14_b_1.csv")

### All good to fill NAs ###


# ##################
# ### SDG_14_c_1 ### Removed in analysis, due to data deficiency
# ##################
# 
# N_SDG_14_c_1_1 <- read.csv("data/SDSN2/14.c.1.1.ratification-and-accession-to-unclos.csv")
# names(N_SDG_14_c_1_1)[4] <- "value"
# # Add SDG subgoals in a new column
# N_SDG_14_c_1_1 <- add_column(N_SDG_14_c_1_1, SDG = "14_c_1_1", .after = "Code")
# N_SDG_14_c_1_1 <- subset(N_SDG_14_c_1_1, nchar(as.character(N_SDG_14_c_1_1$Code))==3)
# N_SDG_14_c_1_1$Year <- 2020 # If year >= 2020, reset as 2020
# 
# # Normalization
# top_5 <- tail(sort(N_SDG_14_c_1_1$value),5)
# bot_5 <- tail(sort(N_SDG_14_c_1_1$value, decreasing = TRUE),5)
# upper <- mean(top_5)
# lower <- mean(bot_5)
# 
# N_SDG_14_c_1_1$value_norm <- (N_SDG_14_c_1_1$value - lower) / (upper - lower)
# 
# # If value >1, assign 1, if value <0, assign 0
# N_SDG_14_c_1_1$value_norm[N_SDG_14_c_1_1$value_norm >1] <- 1
# N_SDG_14_c_1_1$value_norm[N_SDG_14_c_1_1$value_norm <0] <- 0
# 
# # length(unique(N_SDG_14_c_1_1$Code)) # unique country code = 45
# # write.csv(N_SDG_14_c_1_1,"data/SDSN2/N_SDG_14_c_1_1.csv")
# 
# 
# N_SDG_14_c_1_2 <- read.csv("data/SDSN2/14.c.1.2.implementation-of-unclos.csv")
# names(N_SDG_14_c_1_2)[4] <- "value"
# # Add SDG subgoals in a new column
# N_SDG_14_c_1_2 <- add_column(N_SDG_14_c_1_2, SDG = "14_c_1_2", .after = "Code")
# N_SDG_14_c_1_2 <- subset(N_SDG_14_c_1_2, nchar(as.character(N_SDG_14_c_1_2$Code))==3)
# N_SDG_14_c_1_2$Year <- 2020 # If year >= 2020, reset as 2020
# 
# # Normalization
# top_5 <- tail(sort(N_SDG_14_c_1_2$value),5)
# bot_5 <- tail(sort(N_SDG_14_c_1_2$value, decreasing = TRUE),5)
# upper <- mean(top_5)
# lower <- mean(bot_5)
# 
# N_SDG_14_c_1_2$value_norm <- (N_SDG_14_c_1_2$value - lower) / (upper - lower)
# 
# # If value >1, assign 1, if value <0, assign 0
# N_SDG_14_c_1_2$value_norm[N_SDG_14_c_1_2$value_norm >1] <- 1
# N_SDG_14_c_1_2$value_norm[N_SDG_14_c_1_2$value_norm <0] <- 0
# 
# # length(unique(N_SDG_14_c_1_2$Code)) # unique country code = 43
# # write.csv(N_SDG_14_c_1_2,"data/SDSN2/N_SDG_14_c_1_2.csv")
# 
# # Integrate to SDG_14_c_1
# N_SDG_14_c_1 <- rbind(N_SDG_14_c_1_1, N_SDG_14_c_1_2)
# N_SDG_14_c_1 <- subset(N_SDG_14_c_1, nchar(as.character(N_SDG_14_c_1$Code))==3)
# # length(unique(N_SDG_14_c_1$Code)) # unique country code = 45
# # write.csv(N_SDG_14_c_1,"data/SDSN2/N_SDG_14_c_1.csv")
# 
# ### All good to fill NAs ###


##################
# SDG_14_Combine #
##################

N_SDG_14_combraw <- rbind(N_SDG_14_1_1, N_SDG_14_2_1, N_SDG_14_5_1,
                          N_SDG_14_6_1, N_SDG_14_7_1, N_SDG_14_a_1,
                          N_SDG_14_b_1)#, N_SDG_14_c_1) 
names(N_SDG_14_combraw)[1] <- "Country"
names(N_SDG_14_combraw)[5] <- "Value"
names(N_SDG_14_combraw)[6] <- "Value_norm_percent"

N_SDG_14_combraw$Value_norm_percent <- N_SDG_14_combraw$Value_norm_percent *100

# write.csv(N_SDG_14_combraw, "data/SDSN2/N_SDG_14_combraw_no14c.csv")

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

# ## Binary data NOT suitable for imputation - sub/indicators
# ## 14.2.1


# unique(unfill_SDG_14$SDG)
# create a suitable impuation list
SDG_Imputeable <- c("14_1_1_1", "14_1_1_2", "14_5_1", "14_6_1",
                    "14_7_1", "14_a_1", "14_b_1")#, "14_c_1_1", "14_c_1_2")

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


## Remove countries with more than 20% missing values ---

# create empty data frames for later use
df1 <- data.frame()
df_keep <- data.frame()
n <- length(unique(SDG_14_all_filled$Code)) ## Code - county iso code
uni_code <- unique(SDG_14_all_filled$Code) ## unique country list by code
missing_threshold  <- 0.5 # Can change the acceptance value between 0.1 - 0.5, the higher means allowing more missing value

# loop through each country and calculate their SDG scores by sub and main indicaotrs seperately
for (i in seq(1:n)) {
  print(i)
  ### loop each county Code
  print(uni_code[i])
  
  df1 <- subset(SDG_14_all_filled, SDG_14_all_filled$Code==uni_code[i])
  
  m <- nrow(df1)
  na_count <- 0
  
  for (j in seq(1:m)) {
    na_count <- na_count + is.na(df1$Value[j])
  }
  
  if (na_count <= missing_threshold * m) {
    df_keep <- rbind(df_keep, df1)
  } 
}

# SDG_14_all_filled_90 <- df_keep
# # length(unique(SDG_14_all_filled_90$Code)) # Country number: 12
# # write.csv(SDG_14_all_filled_90, "data/SDSN2/SDG_14_complete_90.csv")
# 
# SDG_14_all_filled_80 <- df_keep
# # length(unique(SDG_14_all_filled_80$Code)) # Country number: 47
# # write.csv(SDG_14_all_filled_80, "data/SDSN2/SDG_14_complete_80.csv")
# 
# SDG_14_all_filled_70 <- df_keep
# # length(unique(SDG_14_all_filled_70$Code)) # Country number: 82
# # write.csv(SDG_14_all_filled_70, "data/SDSN2/SDG_14_complete_70.csv")
# 
# SDG_14_all_filled_60 <- df_keep
# # length(unique(SDG_14_all_filled_60$Code)) # Country number: 113
# # write.csv(SDG_14_all_filled_60, "data/SDSN2/SDG_14_complete_60.csv")

SDG_14_all_filled_50 <- df_keep
# length(unique(SDG_14_all_filled_50$Code)) # Country number: 147
# write.csv(SDG_14_all_filled_50, "data/SDSN2/SDG_14_complete_50.csv")


##################
## SDG_14_score ##
##################

## Calculate SDG scores based on sub/indicators
SDG_14_all_filled <- read.csv("data/SDSN2/SDG_14_complete_50.csv")

# ## Look into Kazakhstan
# SDG_14_Kazakhstan_filled <- SDG_14_all_filled[SDG_14_all_filled$Country == "Kazakhstan",]
# # Here is the reason for the problem - Too many NAs!!! 

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
# write.csv(SDG_14_score_by_sub_indicator, "data/SDSN2/SDG_14_score_by_sub_indicator_50.csv")

SDG_14_score_by_main_indicator <- df_main_total[,c(1:5)]
# write.csv(SDG_14_score_by_main_indicator, "data/SDSN2/SDG_14_score_by_main_indicator_50.csv")

## SDG 14 overall score by country
SDG_14_score_overall <- df_overall_total
# write.csv(SDG_14_score_overall, "data/SDSN2/SDG_14_score_overall_50.csv")



## Test -----------------------------------------------------------------------

# # create a subset for only ABW country from full data set
# df_ABW <- subset(SDG_14_all_filled, SDG_14_all_filled$Code=="ABW")
# 
# # Calculate mean by SDG_sub indicator
# df_sub <- aggregate(Value~Country+Code+Year+SDG_sub, data=df_ABW, mean, na.rm=TRUE)
# 
# # Create a list for SDG_main indicator only
# df_sub$SDG_main <- substr(df_sub$SDG_sub,1,4)
# 
# # Calculate mean by SDG_main indicator
# df_main <- aggregate(Value~Country+Code+Year+SDG_main, data=df_sub, mean, na.rm=TRUE)


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

## By target
SDG_14_score_by_main_indicator <- read.csv("data/SDSN2/SDG_14_score_by_main_indicator_50.csv")
# SDG_14_score_by_main_indicator$Year <- as.factor(SDG_14_score_by_main_indicator$Year)

# Idea 1 - Scatter point for SDG by target
ind_analysis <- aggregate(SDG_14_score_by_main_indicator$Value,
                          list(SDG_14_score_by_main_indicator$Year,SDG_14_score_by_main_indicator$SDG_main),
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

# SDG_14_5 <- subset(ind_analysis, ind_analysis$SDG=="14_5")
# SDG_14_7 <- subset(ind_analysis, ind_analysis$SDG=="14_7")
# SDG_14_a <- subset(ind_analysis, ind_analysis$SDG=="14_a")
# SDG_14_1<- subset(ind_analysis, ind_analysis$SDG=="14_1")
# SDG_14_c<- subset(ind_analysis, ind_analysis$SDG=="14_c")


plot3 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size=2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  geom_line(size=1, aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8),
        panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust=0.5)) +
  labs(title = "SDG 14 Target Average Value Change Between 2000 and 2020",
       # subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Target Value",
       col = "SDG Target");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot3




## By indicator
SDG_14_score_by_sub_indicator <- read.csv("data/SDSN2/SDG_14_score_by_sub_indicator_50.csv")
# SDG_14_score_by_sub_indicator$Year <- as.factor(SDG_14_score_by_sub_indicator$Year)

# Idea 1 - Scatter point for SDG by indicators
ind_analysis1 <- aggregate(SDG_14_score_by_sub_indicator$Value,
                           list(SDG_14_score_by_sub_indicator$Year,SDG_14_score_by_sub_indicator$SDG_sub),
                           FUN = mean)
names(ind_analysis1)[1] <- "Year"
names(ind_analysis1)[2] <- "SDG"
names(ind_analysis1)[3] <- "Value"

plot4 <- ggplot(data = ind_analysis1, aes(x=Year, y=Value)) +
  geom_point(size=2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  geom_line(size=1, aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8),
        panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust=0.5)) +
  labs(title = "SDG 14 Indicator Average Value Change Between 2000 and 2020",
       # subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Indicator Value",
       col = "SDG Indicator");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot4

### SDG 14 variation country ### -----------------------------------------------

SDG_14_score_overall <- read.csv("data/SDSN2/SDG_14_score_overall_50.csv")

# # Idea 3 - boxplot
# 
# SDG_14_score_overall$Year <- as.factor(SDG_14_score_overall$Year)
# plot5 <- ggplot(data = SDG_14_score_overall) +
#   stat_boxplot(mapping=aes(x=Year, y=Value),
#                na.rm = TRUE,
#                geom = "errorbar",
#                width = 0.2) +
#   geom_boxplot(mapping=aes(x=Year, y=Value),
#                na.rm = TRUE) +
#   # annotate(geom = "text",
#   #          )
#   theme_bw() +
#   theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
#   # scale_x_discrete(labels = year) +
#   labs(title = "SDG 14 Score Change over Years",
#        subtitle = "249 Countries/Regions between 2000 and 2020",
#        x = "Year",
#        y = "SDG 14 Score");
# plot5
# 
# 
# # Idea 4 - violin plot
# plot6 <- ggplot(data = SDG_14_score_overall) +
#   geom_violin(mapping=aes(x=Year, y=Value),
#               na.rm = TRUE) +
#   # geom_smooth(method="lm") +
#   theme_bw() +
#   # scale_x_discrete(labels = year) +
#   labs(title = "SDG 14 Value Change over Years",
#        subtitle = "249 Countries between 2000 and 2020",
#        x = "Year",
#        y = "SDG 14 Score");
# plot6

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
  geom_line(size=1, color='royalblue') +
  # scale_fill_manual(values='seagreen', name="fill") + 
  geom_point(size=2, color='royalblue') +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1))+
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8),
        # axis.text = element_blank(),
        # axis.line = element_blank(),
        # axis.ticks = element_blank(),
        # panel.border = element_blank(),
        # panel.grid = element_blank(),
        # axis.title = element_blank(),
        panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust=0.5)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Score Between 2000 and 2020",
       # subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 14 Score");
plot7


## SDG score by categorized country 

# Country selection based on biodiversity hotspot and economy status --> from Chung and Liu's paper
Country_category <- read.csv("data/SDSN2/Country category.csv")
names(Country_category)[1] <- "Code"
Country_category <- Country_category[,c(1,3)]

SDG_14_score_overall_cat <- merge(SDG_14_score_overall, Country_category, by="Code")
SDG_14_score_overall_cat <- SDG_14_score_overall_cat[,c(1,3,7,4:6)]

# table(SDG_14_score_overall_cat$Group)/21
# length(unique(SDG_14_score_overall_cat$Code)) # Country number: 115

# Idea 6 - SDG score line graph with SD as shaded area

ind_analysis5 <- aggregate(SDG_14_score_overall_cat$Value, 
                           list(SDG_14_score_overall_cat$Year,SDG_14_score_overall_cat$SDG, 
                                SDG_14_score_overall_cat$Group), 
                           FUN = mean)
names(ind_analysis5)[1] <- "Year"
names(ind_analysis5)[2] <- "SDG"
names(ind_analysis5)[3] <- "Group"
names(ind_analysis5)[4] <- "Value"

# table(ind_analysis5$Group)

# Give meaningful names to categories
ind_analysis5$Group[ind_analysis5$Group=="HHC_H"] <- "High Hotspot (>50%), High Income"
ind_analysis5$Group[ind_analysis5$Group=="HHC_L"] <- "High Hotspot (>50%), Low Income"
ind_analysis5$Group[ind_analysis5$Group=="LHC_H"] <- "Low Hotspot (<50%), High Income"
ind_analysis5$Group[ind_analysis5$Group=="LHC_L"] <- "Low Hotspot (<50%), Low Income"
ind_analysis5$Group[ind_analysis5$Group=="NHC_H"] <- "No Hotspot (0%), High Income"
ind_analysis5$Group[ind_analysis5$Group=="NHC_L"] <- "No Hotspot (0%), Low Income"

# sd <- aggregate(SDG_14_score_overall_cat$Value, 
#                 list(SDG_14_score_overall_cat$Year,SDG_14_score_overall_cat$SDG, 
#                      SDG_14_score_overall_cat$Group), 
#                 FUN = sd)
# 
# ind_analysis5$sd <- sd$x
# ind_analysis5$lower <- ind_analysis5$Value - 0.5*ind_analysis5$sd
# ind_analysis5$upper <- ind_analysis5$Value + 0.5*ind_analysis5$sd

plot17 <- ggplot(data = ind_analysis5, aes(x = Year, y = Value)) +
  geom_point(aes(colour = factor(Group), shape = factor(Group)), size=2) +
  geom_line(aes(colour = factor(Group)), size=1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper, colour = factor(Group)), alpha = 0.2, fill = "royalblue") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  guides(color = guide_legend(title = "Country category"),
         shape = guide_legend(title = "Country category")) +
  scale_colour_manual(values = c('#1f78b4', '#a6cee3', '#33a02c', '#b2df8a','#e31a1c', '#fb9a99')) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8),
        # axis.text = element_blank(),
        # axis.line = element_blank(),
        # axis.ticks = element_blank(),
        # panel.border = element_blank(),
        panel.grid = element_blank(),
        # axis.title = element_blank(),
        # panel.background = element_rect(fill='#deebf7'),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Score Between 2000 and 2020",
       subtitle = "By biodiversity hotspot and income level",
       x = "Year",
       y = "SDG 14 Score");
plot17


### SDG 14 map of all countries/regions ----------------------------------------

## Selected years of SDG scores for all countries, 2000, 2005, 2010, 2015, 2020
SDG_14_score_overall <- read.csv("data/SDSN2/SDG_14_score_overall_50.csv")
# SDG_14_score_overall_wide <- spread(SDG_14_score_overall, key = Year, value = Value)
SDG_14_score_overall_2000 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2000',]
SDG_14_score_overall_2005 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2005',]
SDG_14_score_overall_2010 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2010',]
SDG_14_score_overall_2015 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2015',]
SDG_14_score_overall_2020 <- SDG_14_score_overall[SDG_14_score_overall$Year=='2020',]


# ## Look into Kazakhstan
# SDG_14_Kazakhstan <- SDG_14_score_overall[SDG_14_score_overall$Country == "Kazakhstan",]

# Create a data set of country's SDG change (slope of scores over years)
dat <- data.table(SDG_14_score_overall)
SDG_14_change <- dat[,as.list(coef(lm(Value~Year))), by=Code]
names(SDG_14_change)[3] <- 'Change_rate'
# summary(SDG_14_change)

## shp ---

# head(shp)
shp <- ne_countries(scale = 'medium', type = 'countries', returnclass = 'sf') %>%
  dplyr::select(name, type, iso_a3, economy, income_grp) 

# Remove Antarctica
shp_nATA <- subset(shp, name != "Antarctica")
names(shp_nATA)[3] <- 'Code'

# Merge shp file with SDG score in 2000
shp_nATA_2000 <- merge(shp_nATA, SDG_14_score_overall_2000, by='Code', all=TRUE)
# summary(shp_nATA_2000$Value)

# shp_nATA_2000_analysis <- shp_nATA_2000[,c(1,2,7,10)]
# 
# mean <- mean(shp_nATA_2000_analysis$Value)
# sd <- sd(shp_nATA_2000_analysis$Value)
# 
# high <- mean + sd
# low <- mean - sd
# 
# shp_nATA_2000_top5 <- unique(shp_nATA_2000_analysis$Country[shp_nATA_2000_analysis$Value >= (high+0.3*sd)])
# shp_nATA_2000_bottom5 <- unique(shp_nATA_2000_analysis$Country[shp_nATA_2000_analysis$Value <= (low-0*sd) &
#                                                                  shp_nATA_2000_analysis$Value != 0])
# 
# shp_nATA_2000_top5
# shp_nATA_2000_bottom5

# shp_nATA_2000$Value[shp_nATA_2000$name=="Mali"]

plot8 <- ggplot(shp_nATA_2000) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2000") +
  scale_fill_distiller(palette='YlGnBu', direction = 1, limits=c(0,80), na.value = 'grey') +
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
shp_nATA_2005 <- merge(shp_nATA, SDG_14_score_overall_2005, by='Code', all=TRUE)
# summary(shp_nATA_2005$Value)

plot9 <- ggplot(shp_nATA_2005) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2005") +
  scale_fill_distiller(palette='YlGnBu', direction = 1, limits=c(0,80), na.value = 'grey') +
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
shp_nATA_2010 <- merge(shp_nATA, SDG_14_score_overall_2010, by='Code', all=TRUE)
# summary(shp_nATA_2010$Value)

plot10 <- ggplot(shp_nATA_2010) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2010") +
  scale_fill_distiller(palette='YlGnBu', direction = 1, limits=c(0,80), na.value = 'grey') +
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
shp_nATA_2015 <- merge(shp_nATA, SDG_14_score_overall_2015, by='Code', all=TRUE)
# summary(shp_nATA_2015$Value)

plot11 <- ggplot(shp_nATA_2015) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2015") +
  scale_fill_distiller(palette='YlGnBu', direction = 1, limits=c(0,80), na.value = 'grey') +
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
shp_nATA_2020 <- merge(shp_nATA, SDG_14_score_overall_2020, by='Code', all=TRUE)
# summary(shp_nATA_2020$Value)

# shp_nATA_2020_analysis <- shp_nATA_2020[,c(1,2,7,10)]
# 
# mean2 <- mean(shp_nATA_2020_analysis$Value)
# sd2 <- sd(shp_nATA_2020_analysis$Value)
# 
# high2 <- mean2 + sd2
# low2 <- mean2 - sd2
# 
# shp_nATA_2020_top5 <- unique(shp_nATA_2020_analysis$Country[shp_nATA_2020_analysis$Value >= (high+0.5*sd)])
# shp_nATA_2020_bottom5 <- unique(shp_nATA_2020_analysis$Country[shp_nATA_2020_analysis$Value <= (low-0*sd) &
#                                                                  shp_nATA_2020_analysis$Value != 0])
# 
# shp_nATA_2020_top5
# shp_nATA_2020_bottom5

# shp_nATA_2020$Value[shp_nATA_2020$name=="Mali"]

plot12 <- ggplot(shp_nATA_2020) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 14 in 2020") +
  scale_fill_distiller(palette='YlGnBu', direction = 1, limits=c(0,80), na.value = 'grey') +
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
shp_nATA_change <- merge(shp_nATA, SDG_14_change, by='Code', all=TRUE)
# summary(shp_nATA_change$Change_rate)

# shp_nATA_change_analysis <- shp_nATA_change[, c(1,2,7)]
# summary(shp_nATA_change_analysis)
# 
# shp_nATA_ok_change <- unique(shp_nATA_change_analysis$name[shp_nATA_change_analysis$Change_rate > 0.5 &
#                                                                 shp_nATA_change_analysis$Change_rate < 1])
# shp_nATA_no_change <- unique(shp_nATA_change_analysis$name[shp_nATA_change_analysis$Change_rate <=0])
# 
# summary(shp_nATA_no_change)
# shp_nATA_no_change
# summary(shp_nATA_change_analysis)

plot13 <- ggplot(shp_nATA_change) +
  geom_sf(aes(fill=Change_rate), size=0.1) + 
  ggtitle("Global SDG 14 change between 2000 and 2020") +
  scale_fill_distiller(palette='RdBu', direction = 1, 
                       na.value = 'grey', name='Change rate per year') +
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

## change between 2000 and 2015
SDG_14_2000_2015 <- SDG_14_score_overall_2000
SDG_14_2000_2015$Value_change <- SDG_14_score_overall_2015$Value - SDG_14_score_overall_2000$Value

# Merge shp file with SDG score change
shp_nATA_change_2000_2015 <- merge(shp_nATA, SDG_14_2000_2015, by='Code', all=TRUE)
summary(shp_nATA_change_2000_2015$Value_change)

plot21 <- ggplot(shp_nATA_change_2000_2015) +
  geom_sf(aes(fill=Value_change), size=0.1) + 
  ggtitle("Global SDG 14 change between 2000 and 2015") +
  scale_fill_distiller(palette='RdBu', direction = 1, limits=c(-20,40),
                       name='Score change', na.value = 'grey') +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
# plot21

plot21.2 <- addSmallLegend(plot21)
plot21.2

# summary(SDG_14_2000_2015$Value_change)

## change between 2015 and 2020
SDG_14_2015_2020 <- SDG_14_score_overall_2015
SDG_14_2015_2020$Value_change <- SDG_14_score_overall_2020$Value - SDG_14_score_overall_2015$Value

# Merge shp file with SDG score change
shp_nATA_change_2015_2020 <- merge(shp_nATA, SDG_14_2015_2020, by='Code', all=TRUE)
summary(shp_nATA_change_2015_2020$Value_change)

plot22 <- ggplot(shp_nATA_change_2015_2020) +
  geom_sf(aes(fill=Value_change), size=0.1) + 
  ggtitle("Global SDG 14 change between 2015 and 2020") +
  scale_fill_distiller(palette='RdBu', direction = 1, limits=c(-20,40),
                       name='Score change', na.value = 'grey') +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5));
# plot22

plot22.2 <- addSmallLegend(plot22)
plot22.2

# summary(SDG_14_2015_2020$Value_change)

### Selection analysis ######

# Country selection based on biodiversity hotspot and economy status --> from Chung and Liu's paper
Country_category <- read.csv("data/SDSN2/Country category.csv")
names(Country_category)[1] <- "Code"
Country_category <- Country_category[,c(1,3)]

# Merge categories with countrylist
Country_full <- merge(countrylist, Country_category, by="Code", all=TRUE)

# Include only countreis in this study (n=147)
country_code_use <- unique(SDG_14_score_overall$Code)
Country_full_select <- Country_full[Country_full$Code %in% country_code_use,]
Country_full_select <- Country_full_select[,c(1,3)]
Country_full <- merge(countrylist,  Country_full_select, by="Code", all=TRUE)
# table(Country_full$Group)

# unique(Country_category$Group)
# Give meaningful names to categories
Country_full$Group[Country_full$Group=="HHC_H"] <- "High Hotspot (>50%), High Income (14)"
Country_full$Group[Country_full$Group=="HHC_L"] <- "High Hotspot (>50%), Low Income (40)"
Country_full$Group[Country_full$Group=="LHC_H"] <- "Low Hotspot (<50%), High Income (9)"
Country_full$Group[Country_full$Group=="LHC_L"] <- "Low Hotspot (<50%), Low Income (26)"
Country_full$Group[Country_full$Group=="NHC_H"] <- "No Hotspot (0%), High Income (18)"
Country_full$Group[Country_full$Group=="NHC_L"] <- "No Hotspot (0%), Low Income (8)"
Country_full$Group[is.na(Country_full$Group)] <- "Uncategorized - No Data (n=134)"

# Categorized country map
# Merge shp file with SDG score change
shp_nATA_cat <- merge(shp_nATA, Country_full, by='Code')

plot99 <- ggplot(shp_nATA_cat) +
  geom_sf(aes(fill=Group), size=0.1) + 
  ggtitle("Countries' Spatial Distribution by Biodiveristy Hotspot and Income Level for SDG 14") +
  # scale_fill_discrete(name='Country Category') +
  scale_fill_manual(values = c('#1f78b4', '#a6cee3', '#33a02c', '#b2df8a','#e31a1c', '#fb9a99','grey'),
                    name='Country Category') +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=-0.5));
plot99

# Change legend size function
addSmallLegend <- function(myPlot, pointSize = 10, textSize = 8, spaceLegend = 0.8) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize+2), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}


plot100 <- addSmallLegend(plot99)
plot100

# # Join country category with SDG 14 scores
# SDG_14_score_overall_all <- merge(SDG_14_score_overall, Country_category, by="Code", all=TRUE) 
# Non_category <- SDG_14_score_overall_all[is.na(SDG_14_score_overall_all$Group),]
# unique(Non_category$Code)
# unique(Non_category$Country)
# # Keep only countries with categorization
# SDG_14_score_overall_select <- SDG_14_score_overall_all[!is.na(SDG_14_score_overall_all$Group),]
# SDG_14_score_overall_select <- SDG_14_score_overall_select[,c(1,3,7,4:6)]
# 
# 
# SDG_14_selection_HHC_H <- SDG_14_score_overall_select[SDG_14_score_overall_select$Group=='HHC_H',]
# SDG_14_selection_HHC_L <- SDG_14_score_overall_select[SDG_14_score_overall_select$Group=='HHC_L',]
# SDG_14_selection_LHC_H <- SDG_14_score_overall_select[SDG_14_score_overall_select$Group=='LHC_H',]
# SDG_14_selection_LHC_L <- SDG_14_score_overall_select[SDG_14_score_overall_select$Group=='LHC_L',]
# SDG_14_selection_NHC_H <- SDG_14_score_overall_select[SDG_14_score_overall_select$Group=='NHC_H',]
# SDG_14_selection_NHC_L <- SDG_14_score_overall_select[SDG_14_score_overall_select$Group=='NHC_L',]
# 
# # Create a data set of HHC_H country's SDG change (slope of scores over years)
# dat1 <- data.table(SDG_14_selection_HHC_H)
# SDG_14_HHC_H_change <- dat1[,as.list(coef(lm(Value~Year))), by=Code]
# names(SDG_14_HHC_H_change)[3] <- 'Change_rate'
# # summary(SDG_14_change)
# 
# # Merge shp file with SDG score change
# shp_nATA_HHC_H_change <- merge(shp_nATA, SDG_14_HHC_H_change, by='Code')
# 
# # High hotspot, High Income
# plot14 <- ggplot(shp_nATA_HHC_H_change) +
#   geom_sf(aes(fill=Change_rate), size=0.1) + 
#   ggtitle("High Biodiversity and High Income Countries SDG 14 change between 2000 and 2020") +
#   scale_fill_distiller(palette='PuBu', direction = 1) +
#   theme(
#     axis.text = element_blank(),
#     axis.line = element_blank(),
#     axis.ticks = element_blank(),
#     panel.border = element_blank(),
#     panel.grid = element_blank(),
#     axis.title = element_blank(),
#     panel.background = element_rect(fill='white'),
#     plot.title = element_text(hjust=0.5));
# plot14
# 
# # Create a data set of HHC_L country's SDG change (slope of scores over years)
# dat2 <- data.table(SDG_14_selection_HHC_L)
# SDG_14_HHC_L_change <- dat2[,as.list(coef(lm(Value~Year))), by=Code]
# names(SDG_14_HHC_L_change)[3] <- 'Change_rate'
# # summary(SDG_14_change)
# 
# # Merge shp file with SDG score change
# shp_nATA_HHC_L_change <- merge(shp_nATA, SDG_14_HHC_L_change, by='Code')
# 
# # High hotspot, Low Income
# plot15 <- ggplot(shp_nATA_HHC_L_change) +
#   geom_sf(aes(fill=Change_rate), size=0.1) + 
#   ggtitle("High Biodiversity and Low Income Countries SDG 14 change between 2000 and 2020") +
#   scale_fill_distiller(palette='PuBu', direction = 1) +
#   theme(
#     axis.text = element_blank(),
#     axis.line = element_blank(),
#     axis.ticks = element_blank(),
#     panel.border = element_blank(),
#     panel.grid = element_blank(),
#     axis.title = element_blank(),
#     panel.background = element_rect(fill='white'),
#     plot.title = element_text(hjust=0.5));
# plot15

# ## Regression analysis
# 
# sdg_ind <- unique(SDG_14_all_filled$SDG)
# 
# SDG_14_reg <- SDG_14_score_overall[,c(2,3,4,6)]
# names(SDG_14_reg)[4] <- "SDG_14"
# 
# for (i in (1:length(sdg_ind))){
#   SDG_14_reg[,i+4] <- SDG_14_all_filled$Value[SDG_14_all_filled$SDG == sdg_ind[i]]
# }
# 
# SDG_14_lm <- lm(SDG_14 ~ V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14, 
#                 data = SDG_14_reg)
# summary(SDG_14_lm)
# 
# SDG_14_lm_did <- lm(SDG_14 ~ V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + 
#                       factor(Country), data = SDG_14_reg)
# summary(SDG_14_lm_did)

