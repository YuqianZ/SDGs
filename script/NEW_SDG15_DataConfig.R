
### Updated SDG 15 Data Configuration
### 2022-04-09
### Yuqian Zhang

# 
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir

setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG")
getwd()


source( file="script/reference.R" ); 


# Load data and cleaning --------------------------------------------------


# Create a country list for later use - list source: ISO 3166 - 3 digit ISO country/region code (#249)
df <- read.csv("data/SDSN/world_entity.csv")
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

# yearlist <- data.frame(matrix(ncol=1, nrow=21))
# colnames(yearlist) <- "Year"
# yearlist$Year <- interest_year
# 
# sdg <- c("15_1_1", "15_1_2_1", "15_1_2_2", "15_2_1_1", "15_2_1_2",
#          "15_2_1_3", "15_4_1",   "15_4_2",   "15_5_1",   "15_6_1_1",
#          "15_6_1_2", "15_6_1_3", "15_6_1_4", "15_6_1_5", "15_8_1_1",
#          "15_8_1_2", "15_9_1",   "15_9_2")
# sdglist <- data.frame(matrix(ncol=1, nrow=18))
# colnames(sdglist) <- "SDG"
# sdglist$SDG <- sdg


country_sdg <- countrylist %>%
  add_column("15_1_1" = NA, "15_1_2_1" = NA, "15_1_2_2" = NA, "15_2_1_1" = NA, 
             "15_2_1_2" = NA, "15_2_1_3" = NA, "15_4_1" = NA, "15_4_2" = NA,  
             "15_5_1" = NA, "15_6_1_1" = NA, "15_6_1_2" = NA, "15_6_1_3" = NA, 
             "15_6_1_4" = NA, "15_6_1_5" = NA, "15_8_1_1" = NA, "15_8_1_2" = NA, 
             "15_9_1" = NA, "15_9_2" = NA)

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
# write.csv(full_country_sdg_year, "data/SDSN/empty_full_country_sdg_year.csv")

##################
### SDG_15_1_1 ###
##################

N_SDG_15_1_1 <- read.csv("data/SDSN/15.1.1_forest-area-as-share-of-land-area.csv")
names(N_SDG_15_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_1_1 <- add_column(N_SDG_15_1_1, SDG = "15_1_1", .after = "Code")
N_SDG_15_1_1 <- subset(N_SDG_15_1_1, nchar(as.character(N_SDG_15_1_1$Code))==3)
# length(unique(N_SDG_15_1_1$Code)) # unique country code = 224
# write.csv(N_SDG_15_1_1,"data/SDSN/N_SDG_15_1_1.csv")

# N_SDG_15_1_1_sub <- subset(N_SDG_15_1_1, N_SDG_15_1_1$Year %in% empty_year$year)
# # write.csv(N_SDG_15_1_1_sub,"data/SDSN/N_SDG_15_1_1_yr.csv")
# 
# N_SDG_15_1_1_sub_f <- N_SDG_15_1_1_sub

### All good to fill NAs ###

##################
### SDG_15_1_2 ###
##################

N_SDG_15_1_2_1 <- read.csv("data/SDSN/15.1.2.1_proportion-of-important-sites-for-freshwater-biodiversity-covered-by-protected-areas.csv")
names(N_SDG_15_1_2_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_1_2_1 <- add_column(N_SDG_15_1_2_1, SDG = "15_1_2_1", .after = "Code")
# length(unique(N_SDG_15_1_2_1$Code)) # unique country code = 162
# write.csv(N_SDG_15_1_2_1,"data/SDSN/N_SDG_15_1_2_1.csv")
# 
# N_SDG_15_1_2_1_wide <- N_SDG_15_1_2_1 %>%
#   spread(Year, value)

### All good to fill NAs ###

N_SDG_15_1_2_2 <- read.csv("data/SDSN/15.1.2.2_protected-terrestrial-biodiversity-sites.csv")
names(N_SDG_15_1_2_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_1_2_2 <- add_column(N_SDG_15_1_2_2, SDG = "15_1_2_2", .after = "Code")
# length(unique(N_SDG_15_1_2_2$Code)) # unique country code = 237
# write.csv(N_SDG_15_1_2_2,"data/SDSN/N_SDG_15_1_2_2.csv")
# 
# N_SDG_15_1_2_2_wide <- N_SDG_15_1_2_2 %>%
#   spread(Year, value)

### All good to fill NAs ###

# Integrate to SDG_15_1_2
N_SDG_15_1_2 <- rbind(N_SDG_15_1_2_1, N_SDG_15_1_2_2)
N_SDG_15_1_2 <- subset(N_SDG_15_1_2, nchar(as.character(N_SDG_15_1_2$Code))==3)
# length(unique(N_SDG_15_1_2$Code)) # unique country code = 235
# write.csv(N_SDG_15_1_2,"data/SDSN/N_SDG_15_1_2.csv")

# N_SDG_15_1_2_sub <- subset(N_SDG_15_1_2, N_SDG_15_1_2$Year %in% empty_year$year)
# # write.csv(N_SDG_15_1_2_sub,"data/SDSN/N_SDG_15_1_2_yr.csv")
# 
# N_SDG_15_1_2_sub_f <- N_SDG_15_1_2_sub

### All good to fill NAs ###

##################
### SDG_15_2_1 ###
##################

N_SDG_15_2_1_1 <- read.csv("data/SDSN/15.2.1.1_forest-area-net-change-rate.csv")
names(N_SDG_15_2_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_2_1_1 <- add_column(N_SDG_15_2_1_1, SDG = "15_2_1_1", .after = "Code")
# length(unique(N_SDG_15_2_1_1$Code)) # unique country code = 231
# write.csv(N_SDG_15_2_1_1,"data/SDSN/N_SDG_15_2_1_1.csv")
# summary(N_SDG_15_2_1_1$value)

### Needs value conversion before filling NAs ###

### Thought: categorical -
### x<=-2, y=0
### -2<x<=0, y=1
### 0<x<=2, y=2
### 2<x<=4, y=3
### x>4, y=4

N_SDG_15_2_1_1$value_ed <- N_SDG_15_2_1_1$value
N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value <= -2] <- 0
N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > -2 & N_SDG_15_2_1_1$value <= 0] <- 1
N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > 0 & N_SDG_15_2_1_1$value <= 2] <- 2
N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > 2 & N_SDG_15_2_1_1$value <= 4] <- 3
N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > 4] <- 4

N_SDG_15_2_1_1$value <- N_SDG_15_2_1_1$value_ed
N_SDG_15_2_1_1 <- N_SDG_15_2_1_1[,1:5]
N_SDG_15_2_1_1$value <- N_SDG_15_2_1_1$value/(max(N_SDG_15_2_1_1$value)) *100

### All good to fill NAs ###


N_SDG_15_2_1_2 <- read.csv("data/SDSN/15.2.1.2_proportion-of-forest-area-within-legally-established-protected-areas.csv")
names(N_SDG_15_2_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_2_1_2 <- add_column(N_SDG_15_2_1_2, SDG = "15_2_1_2", .after = "Code")
# length(unique(N_SDG_15_2_1_2$Code)) # unique country code = 173
# write.csv(N_SDG_15_2_1_2,"data/SDSN/N_SDG_15_2_1_2.csv")
N_SDG_15_2_1_2$value_ed <- N_SDG_15_2_1_2$value
N_SDG_15_2_1_2$value_ed[N_SDG_15_2_1_2$value_ed > 100] <- 100 
N_SDG_15_2_1_2$value <- N_SDG_15_2_1_2$value_ed

N_SDG_15_2_1_2 <- N_SDG_15_2_1_2[,1:5]

### All good to fill NAs ###

N_SDG_15_2_1_3 <- read.csv("data/SDSN/15.2.1.3_proportion-of-forest-area-certified-under-an-independently-verified-certification-scheme.csv")
names(N_SDG_15_2_1_3)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_2_1_3 <- add_column(N_SDG_15_2_1_3, SDG = "15_2_1_3", .after = "Code")
N_SDG_15_2_1_3$log.value <- log(N_SDG_15_2_1_3$value) # log form makes more comparable, makes sense?
# hist(N_SDG_15_2_1_3$value) # Distribution of original value
# hist(N_SDG_15_2_1_3$log.value) # Distribution of log value
N_SDG_15_2_1_3[N_SDG_15_2_1_3 == -Inf] <- 0 # Convert all -Inf to O
N_SDG_15_2_1_3 <- N_SDG_15_2_1_3[c(1:4,6)]
names(N_SDG_15_2_1_3)[5] <- "value"
# summary(N_SDG_15_2_1_3) # Summary of transformed data set
# Lower bound is 0, upper bound is max(~19.892) now
N_SDG_15_2_1_3$value <- N_SDG_15_2_1_3$value/(max(N_SDG_15_2_1_3$value)) # Transform range between 0 and 1, makes sense?
N_SDG_15_2_1_3$value <- N_SDG_15_2_1_3$value * 100
##################### 15_2_1_3 needs further data transformation??????
# length(unique(N_SDG_15_2_1_3$Code)) # unique country code = 248
# write.csv(N_SDG_15_2_1_3,"data/SDSN/N_SDG_15_2_1_3.csv")

# Integrate to SDG_15_2_1
N_SDG_15_2_1 <- rbind(N_SDG_15_2_1_1, N_SDG_15_2_1_2, N_SDG_15_2_1_3)
N_SDG_15_2_1 <- subset(N_SDG_15_2_1, nchar(as.character(N_SDG_15_2_1$Code))==3)
# length(unique(N_SDG_15_2_1$Code)) # unique country code = 243
# write.csv(N_SDG_15_2_1,"data/SDSN/N_SDG_15_2_1.csv")

# N_SDG_15_2_1_sub <- subset(N_SDG_15_2_1, N_SDG_15_2_1$Year %in% empty_year$year)
# # write.csv(N_SDG_15_2_1_sub,"data/SDSN/N_SDG_15_2_1_yr.csv")
# 
# N_SDG_15_2_1_sub_wide <- N_SDG_15_2_1_sub %>%
#   spread(Year, value)
# 
# unique(colnames(N_SDG_15_2_1_sub_wide))
# N_SDG_15_2_1_sub_wide <- N_SDG_15_2_1_sub_wide %>%
#   add_column("2001" = NA, 
#              "2002" = NA, 
#              "2003" = NA, 
#              "2004" = NA, 
#              "2006" = NA, 
#              "2007" = NA, 
#              "2008" = NA, 
#              "2009" = NA, 
#              "2011" = NA, 
#              "2012" = NA, 
#              "2013" = NA, 
#              "2014" = NA)
# 
# N_SDG_15_2_1_sub_long <- melt(N_SDG_15_2_1_sub_wide, id.vars = c("Entity","Code", "SDG"))
# names(N_SDG_15_2_1_sub_long)[4] <- "Year"
# 
# N_SDG_15_2_1_sub_f <- N_SDG_15_2_1_sub_long

### All good to fill NAs ###


##################
### SDG_15_4_1 ###
##################

N_SDG_15_4_1 <- read.csv("data/SDSN/15.4.1_coverage-by-protected-areas-of-important-sites-for-mountain-biodiversity.csv")
names(N_SDG_15_4_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_4_1 <- add_column(N_SDG_15_4_1, SDG = "15_4_1", .after = "Code")
N_SDG_15_4_1 <- subset(N_SDG_15_4_1, nchar(as.character(N_SDG_15_4_1$Code))==3)
# length(unique(N_SDG_15_4_1$Code)) # unique country code = 181
# write.csv(N_SDG_15_4_1,"data/SDSN/N_SDG_15_4_1.csv")

### All good to fill NAs ###

##################
### SDG_15_4_2 ###
##################

N_SDG_15_4_2 <- read.csv("data/SDSN/15.4.2_mountain-green-cover-index.csv")
names(N_SDG_15_4_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_4_2 <- add_column(N_SDG_15_4_2, SDG = "15_4_2", .after = "Code")
N_SDG_15_4_2 <- subset(N_SDG_15_4_2, nchar(as.character(N_SDG_15_4_2$Code))==3)
# length(unique(N_SDG_15_4_2$Code)) # unique country code = 141
# write.csv(N_SDG_15_4_2,"data/SDSN/N_SDG_15_4_2.csv")

### All good to fill NAs ###

##################
### SDG_15_5_1 ###
##################

N_SDG_15_5_1 <- read.csv("data/SDSN/15.5.1_red-list-index.csv")
names(N_SDG_15_5_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_5_1 <- add_column(N_SDG_15_5_1, SDG = "15_5_1", .after = "Code")
N_SDG_15_5_1 <- subset(N_SDG_15_5_1, nchar(as.character(N_SDG_15_5_1$Code))==3)
# length(unique(N_SDG_15_5_1$Code)) # unique country code = 240
# write.csv(N_SDG_15_5_1,"data/SDSN/N_SDG_15_5_1.csv")

N_SDG_15_5_1$value <- N_SDG_15_5_1$value *100

### All good to fill NAs ###


##################
### SDG_15_6_1 ###
##################

N_SDG_15_6_1_1 <- read.csv("data/SDSN/15.6.1.1_countries-to-the-international-treaty-on-plant-genetic-resources.csv")
names(N_SDG_15_6_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_6_1_1 <- add_column(N_SDG_15_6_1_1, SDG = "15_6_1_1", .after = "Code")
# length(unique(N_SDG_15_6_1_1$Code)) # unique country code = 245
# write.csv(N_SDG_15_6_1_1,"data/SDSN/N_SDG_15_6_1_1.csv")

N_SDG_15_6_1_1 <- N_SDG_15_6_1_1[N_SDG_15_6_1_1$value<=1, ]
N_SDG_15_6_1_1$value <- N_SDG_15_6_1_1$value *100

### All good to fill NAs ###

N_SDG_15_6_1_2 <- read.csv("data/SDSN/15.6.1.2_countries-that-are-parties-to-the-nagoya-protocol.csv")
names(N_SDG_15_6_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_6_1_2 <- add_column(N_SDG_15_6_1_2, SDG = "15_6_1_2", .after = "Code")
# length(unique(N_SDG_15_6_1_2$Code)) # unique country code = 198
# write.csv(N_SDG_15_6_1_2,"data/SDSN/N_SDG_15_6_1_2.csv")

N_SDG_15_6_1_2 <- N_SDG_15_6_1_2[N_SDG_15_6_1_2$value<=1, ]
N_SDG_15_6_1_2$value <- N_SDG_15_6_1_2$value *100

### All good to fill NAs ###

Np_SDG_15_6_1_3 <- read.csv("data/SDSN/15.6.1.3_ER_CBD_ORSPGRFA.csv")

# Drop irrelevant columns and rename columns
Np_SDG_15_6_1_3c <- Np_SDG_15_6_1_3[,c(3,7,10:16)]
names(Np_SDG_15_6_1_3c)[1] <- "SDG"
names(Np_SDG_15_6_1_3c)[2] <- "Entity"
Np_SDG_15_6_1_3c$SDG <- "15_6_1_3"

# Clean data with melting from wide to long
Np_SDG_15_6_1_3clong <- melt(Np_SDG_15_6_1_3c, id.vars = c("Entity", "SDG"))

# Add Year to a new column name
names(Np_SDG_15_6_1_3clong)[names(Np_SDG_15_6_1_3clong)=="variable"] <- "Year"

# Clean column name for YEAR
Np_SDG_15_6_1_3clong$Year <- gsub("X", "", Np_SDG_15_6_1_3clong$Year)

# Add Country Code column
N_SDG_15_6_1_3 <- merge(countrylist, Np_SDG_15_6_1_3clong, by="Entity", all=TRUE)

N_SDG_15_6_1_3$Code[N_SDG_15_6_1_3$Entity == "United Republic of Tanzania"] <- "TZA"
N_SDG_15_6_1_3 <- na.omit(N_SDG_15_6_1_3)
# length(unique(N_SDG_15_6_1_3$Code)) # unique country code = 233
# write.csv(N_SDG_15_6_1_3,"data/SDSN/N_SDG_15_6_1_3.csv")

N_SDG_15_6_1_3$value <- N_SDG_15_6_1_3$value *100

### All good to fill NAs ###

N_SDG_15_6_1_4 <- read.csv("data/SDSN/15.6.1.4_countries-to-access-and-benefit-sharing-clearing-house.csv")
names(N_SDG_15_6_1_4)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_6_1_4 <- add_column(N_SDG_15_6_1_4, SDG = "15_6_1_4", .after = "Code")
# length(unique(N_SDG_15_6_1_4$Code)) # unique country code = 198
# write.csv(N_SDG_15_6_1_4,"data/SDSN/N_SDG_15_6_1_4.csv")

N_SDG_15_6_1_4 <- N_SDG_15_6_1_4[N_SDG_15_6_1_4$value<=1, ]
N_SDG_15_6_1_4$value <- N_SDG_15_6_1_4$value *100

### All good to fill NAs ###

Np_SDG_15_6_1_5 <- read.csv("data/SDSN/15.6.1.5_ER_CBD_SMTA.csv")
# Drop irrelevant columns and rename columns
Np_SDG_15_6_1_5c <- Np_SDG_15_6_1_5[,c(3,7,10:20)]
names(Np_SDG_15_6_1_5c)[1] <- "SDG"
names(Np_SDG_15_6_1_5c)[2] <- "Entity"
Np_SDG_15_6_1_5c$SDG <- "15_6_1_5"

# Clean data with melting from wide to long
Np_SDG_15_6_1_5clong <- melt(Np_SDG_15_6_1_5c, id.vars = c("Entity", "SDG"))

# Add Year to a new column name
names(Np_SDG_15_6_1_5clong)[names(Np_SDG_15_6_1_5clong)=="variable"] <- "Year"

# Clean column name for YEAR
Np_SDG_15_6_1_5clong$Year <- gsub("X", "", Np_SDG_15_6_1_5clong$Year)

# Add Country Code column
N_SDG_15_6_1_5 <- merge(countrylist, Np_SDG_15_6_1_5clong, by="Entity", all=TRUE)

N_SDG_15_6_1_5$Code[N_SDG_15_6_1_3$Entity == "Republic of Moldova"] <- "MDA"
N_SDG_15_6_1_5$Code[N_SDG_15_6_1_5$Entity == "United Republic of Tanzania"] <- "TZA"

N_SDG_15_6_1_5 <- na.omit(N_SDG_15_6_1_5)
N_SDG_15_6_1_5$log_value <- log(N_SDG_15_6_1_5$value)

N_SDG_15_6_1_5[N_SDG_15_6_1_5 == -Inf] <- 0 # Convert all -Inf to O
N_SDG_15_6_1_5 <- N_SDG_15_6_1_5[c(1:4,6)]
names(N_SDG_15_6_1_5)[5] <- "value"
# summary(N_SDG_15_6_1_5) # Summary of transformed data set

# Lower bound is 0, upper bound is max(~9.298) now
N_SDG_15_6_1_5$value <- N_SDG_15_6_1_5$value/(max(N_SDG_15_6_1_5$value)) *100 # Transform range between 0 and 100%, makes sense?

##################### 15_6_1_5 needs further data transformation??????

# length(unique(N_SDG_15_6_1_5$Code)) # unique country code = 233
# write.csv(N_SDG_15_6_1_5,"data/SDSN/N_SDG_15_6_1_5.csv")


### All good to fill NAs ###


# Integrate to SDG_15_6_1
N_SDG_15_6_1 <- rbind(N_SDG_15_6_1_1, N_SDG_15_6_1_2,
                      N_SDG_15_6_1_3, N_SDG_15_6_1_4,
                      N_SDG_15_6_1_5)

N_SDG_15_6_1 <- subset(N_SDG_15_6_1, nchar(as.character(N_SDG_15_6_1$Code))==3)
# length(unique(N_SDG_15_6_1 $Code)) # unique country code = 245
# write.csv(N_SDG_15_6_1,"data/SDSN/N_SDG_15_6_1.csv")

### All good to fill NAs ###


##################
### SDG_15_8_1 ###
#################

N_SDG_15_8_1_1 <- read.csv("data/SDSN/15.8.1.1_national-biodiversity-strategy-align-with-aichi-target-9.csv")
names(N_SDG_15_8_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_8_1_1 <- add_column(N_SDG_15_8_1_1, SDG = "15_8_1_1", .after = "Code")
# length(unique(N_SDG_15_8_1_1$Code)) # unique country code = 195
# write.csv(N_SDG_15_8_1_1,"data/SDSN/N_SDG_15_8_1_1.csv")
N_SDG_15_8_1_1$value <- N_SDG_15_8_1_1$value *100

### All good to fill NAs ###

N_SDG_15_8_1_2 <- read.csv("data/SDSN/15.8.1.2_budget-to-manage-invasive-alien-species.csv")
names(N_SDG_15_8_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_8_1_2 <- add_column(N_SDG_15_8_1_2, SDG = "15_8_1_2", .after = "Code")
# length(unique(N_SDG_15_8_1_2$Code)) # unique country code = 153
# write.csv(N_SDG_15_8_1_2,"data/SDSN/N_SDG_15_8_1_2.csv")
N_SDG_15_8_1_2$value <- N_SDG_15_8_1_2$value *100

### All good to fill NAs ###

# Integrate to SDG_15_8_1
N_SDG_15_8_1 <- rbind(N_SDG_15_8_1_1, N_SDG_15_8_1_2)

N_SDG_15_8_1 <- subset(N_SDG_15_8_1, nchar(as.character(N_SDG_15_8_1$Code))==3)
# length(unique(N_SDG_15_8_1 $Code)) # unique country code = 198
# write.csv(N_SDG_15_8_1 ,"data/SDSN/N_SDG_15_8_1.csv")

### All good to fill NAs ###


##################
### SDG_15_9_1 ###
##################

# When to have national target for NBSAPs
Np_SDG_15_9_1 <- read.csv("data/SDSN/New_short_SDG_15_9_1.csv")
names(Np_SDG_15_9_1)[1] <- "Entity"
# # Drop irrelevant columns
# Np_SDG_15_9_1c = subset(Np_SDG_15_9_1, select = -c(X, CountryCode))
# Clean data with melting from wide to long
Np_SDG_15_9_1_long <- melt(Np_SDG_15_9_1, id.vars = c("Entity","Code"))
# Add Year to a new column name
names(Np_SDG_15_9_1_long)[names(Np_SDG_15_9_1_long)=="variable"] <- "Year"
# Clean the column of Year
Np_SDG_15_9_1_long$Year <- gsub("X", "", Np_SDG_15_9_1_long$Year)
# Add SDG subgoals in a new column
Np_SDG_15_9_1_sorted <- add_column(Np_SDG_15_9_1_long, SDG = "15_9_1", .after = "Code")
Np_SDG_15_9_1_clean <- Np_SDG_15_9_1_sorted[order(Np_SDG_15_9_1_sorted$Entity, Np_SDG_15_9_1_sorted$SDG, Np_SDG_15_9_1_sorted$Year),]
# view(Np_SDG_15_9_1_clean)
N_SDG_15_9_1 <- Np_SDG_15_9_1_clean 
N_SDG_15_9_1$value[is.na(N_SDG_15_9_1$value)] <- 1 
N_SDG_15_9_1 <- subset(N_SDG_15_9_1, nchar(as.character(N_SDG_15_9_1$Code))==3)
# length(unique(N_SDG_15_9_1$Code)) # unique country code = 249
# write.csv(N_SDG_15_9_1,"data/SDSN/N_SDG_15_9_1.csv")
N_SDG_15_9_1$value <- N_SDG_15_9_1$value *100

### All good to fill NAs ###


##################
### SDG_15_9_2 ###
##################

N_SDG_15_9_2_1 <- read.csv("data/SDSN/15.9.2.1_countries-with-no-national-target-reflecting-aichi-biodiversity-target-2.csv")
names(N_SDG_15_9_2_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_9_2_1 <- add_column(N_SDG_15_9_2_1, SDG = "15_9_2_1", .after = "Code")
N_SDG_15_9_2_1 <- N_SDG_15_9_2_1[!(is.na(N_SDG_15_9_2_1$Code) | N_SDG_15_9_2_1$Code==""), ]
N_SDG_15_9_2_1 <- N_SDG_15_9_2_1[N_SDG_15_9_2_1$value==1, ]
N_SDG_15_9_2_1[5] <- "0" # recode value as 0


N_SDG_15_9_2_2 <- read.csv("data/SDSN/15.9.2.2_countries-with-no-progress-towards-their-national-target-reflecting-aichi-biodiversity-target-2.csv")
names(N_SDG_15_9_2_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_9_2_2 <- add_column(N_SDG_15_9_2_2, SDG = "15_9_2_2", .after = "Code")
N_SDG_15_9_2_2 <- N_SDG_15_9_2_2[!(is.na(N_SDG_15_9_2_2$Code) | N_SDG_15_9_2_2$Code==""), ]
N_SDG_15_9_2_2[5] <- "1" # recode value as 1

N_SDG_15_9_2_3 <- read.csv("data/SDSN/15.9.2.3_countries-progressing-towards-their-national-target-reflecting-aichi-biodiversity-target-2-at-an-insufficient-rate.csv")
names(N_SDG_15_9_2_3)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_9_2_3 <- add_column(N_SDG_15_9_2_3, SDG = "15_9_2_3", .after = "Code")
N_SDG_15_9_2_3 <- N_SDG_15_9_2_3[!(is.na(N_SDG_15_9_2_3$Code) | N_SDG_15_9_2_3$Code==""), ]
N_SDG_15_9_2_3 <- N_SDG_15_9_2_3[N_SDG_15_9_2_3$value==1, ]
N_SDG_15_9_2_3[5] <- "2" # recode value as 2

N_SDG_15_9_2_4 <- read.csv("data/SDSN/15.9.2.4_countries-on-track-to-their-national-target-reflecting-aichi-biodiversity-target-2.csv")
names(N_SDG_15_9_2_4)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_9_2_4 <- add_column(N_SDG_15_9_2_4, SDG = "15_9_2_4", .after = "Code")
N_SDG_15_9_2_4 <- N_SDG_15_9_2_4[!(is.na(N_SDG_15_9_2_4$Code) | N_SDG_15_9_2_4$Code==""), ]
N_SDG_15_9_2_4 <- N_SDG_15_9_2_4[N_SDG_15_9_2_4$value==1, ]
N_SDG_15_9_2_4[5] <- "3" # recode value as 3

N_SDG_15_9_2_5 <- read.csv("data/SDSN/15.9.2.5_countries-on-track-to-exceed-their-national-target-reflecting-aichi-biodiversity-target-2.csv")
names(N_SDG_15_9_2_5)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_9_2_5 <- add_column(N_SDG_15_9_2_5, SDG = "15_9_2_5", .after = "Code")
N_SDG_15_9_2_5 <- N_SDG_15_9_2_5[!(is.na(N_SDG_15_9_2_5$Code) | N_SDG_15_9_2_5$Code==""), ]
N_SDG_15_9_2_5[5] <- "4" # recode value as 4

# Merge SDG_15_9_2
df3 <- merge(merge(merge(merge(
  N_SDG_15_9_2_1,
  N_SDG_15_9_2_2, all = TRUE),
  N_SDG_15_9_2_3, all = TRUE),
  N_SDG_15_9_2_4, all = TRUE),
  N_SDG_15_9_2_5, all = TRUE)

N_SDG_15_9_2 <- merge(countrylist, df3, by="Code", all=TRUE)

N_SDG_15_9_2 <- subset(N_SDG_15_9_2, select= -Entity.y)
N_SDG_15_9_2[3] <-"15_9_2"
N_SDG_15_9_2[4] <- "2020"
names(N_SDG_15_9_2)[2] <- "Entity"
N_SDG_15_9_2 <- N_SDG_15_9_2[c(2,1,3:5)]

N_SDG_15_9_2 <- subset(N_SDG_15_9_2, nchar(as.character(N_SDG_15_9_2$Code))==3) # remove aggregate data, e.g.world
# length(unique(N_SDG_15_9_2$Code)) # unique country code = 249
# sum(is.na(N_SDG_15_9_2$value))

# Need to edit SDG 15.9.2 based on the submission of NBSAPs result from 15.9.1
N_SDG_15_9_2_1ed <- read.csv("data/SDSN/15.9.2.1 NBSAPs absence.csv") # Countries have no NBSAPs till 2021 - concluded from SDG 15.9.1 above

# unique(N_SDG_15_9_2$Code[is.na(N_SDG_15_9_2$value)])
# unique(N_SDG_15_9_2_1ed$Code)

# for (i in 1:nrow(N_SDG_15_9_2)) {
#   if (N_SDG_15_9_2$Code[is.na(N_SDG_15_9_2$value)] %in% N_SDG_15_9_2_1ed$Code)
#     {print(i)}
#     # N_SDG_15_9_2$value[i] <- 0
#   if (!(N_SDG_15_9_2$Code[is.na(N_SDG_15_9_2$value)] %in% N_SDG_15_9_2_1ed$Code))
#     # N_SDG_15_9_2$value[i] <- 1.5
#     {print(0)}
# }


for (i in 1:nrow(N_SDG_15_9_2)){
  if (is.na(N_SDG_15_9_2$value[i]))
  {
    if (N_SDG_15_9_2$Code[i] %in% N_SDG_15_9_2_1ed$Code)
      {N_SDG_15_9_2$value[i] <- 0}
    if (!(N_SDG_15_9_2$Code[i] %in% N_SDG_15_9_2_1ed$Code))
      {N_SDG_15_9_2$value[i] <- 1.5}
  }
}

# unique(N_SDG_15_9_2$value)
# write.csv(N_SDG_15_9_2,"data/SDSN/N_SDG_15_9_2.csv")

N_SDG_15_9_2 <- subset(N_SDG_15_9_2, nchar(as.character(N_SDG_15_9_2$Code))==3)
# length(unique(N_SDG_15_9_2$Code)) # unique country code = 249
# write.csv(N_SDG_15_9_2,"data/SDSN/N_SDG_15_9_2.csv")
N_SDG_15_9_2$value <- as.numeric(N_SDG_15_9_2$value)
N_SDG_15_9_2$value <- N_SDG_15_9_2$value/(max(N_SDG_15_9_2$value)) *100

# add 0 to the year of 2000 for all countries
# convert from long to wide
Np_SDG_15_9_2_wide <- spread(N_SDG_15_9_2, Year, value)
Np_SDG_15_9_2_wide$"2000" <- 0
Np_SDG_15_9_2_wide <- Np_SDG_15_9_2_wide[, c(1:3,5,4)]
#convert back from wide to long
Np_SDG_15_9_2_long <- melt(Np_SDG_15_9_2_wide, id.vars = c("Entity","Code","SDG"))
names(Np_SDG_15_9_2_long)[4] <- "Year"
names(Np_SDG_15_9_2_long)[5] <- "value"

N_SDG_15_9_2 <- Np_SDG_15_9_2_long

### All good to fill NAs ###

##################
### SDG_15_ab  ### Not used at the moment
##################
# 
# ### Think about the standardization and the positive/negative (giving or receiving) aids???
# 
# N_SDG_15_ab_1 <- read.csv("data/SDSN/15.ab.1_total-oda-for-biodiversity-by-donor.csv")
# names(N_SDG_15_ab_1)[4] <- "value"
# # Add SDG subgoals in a new column
# N_SDG_15_ab_1 <- add_column(N_SDG_15_ab_1, SDG = "SDG_15_ab_1", .after = "Code")
# N_SDG_15_ab_1 <- subset(N_SDG_15_ab_1, nchar(as.character(N_SDG_15_ab_1$Code))==3) # remove aggregate data, e.g.world
# # length(unique(N_SDG_15_ab_1$Code)) # unique country code = 29
# # write.csv(N_SDG_15_ab_1,"data/SDSN/N_SDG_15_ab_1.csv")
# 
# 
# N_SDG_15_ab_2 <- read.csv("data/SDSN/15.ab.2_total-oda-for-biodiversity-by-recipient.csv")
# names(N_SDG_15_ab_2)[4] <- "value"
# # Add SDG subgoals in a new column
# N_SDG_15_ab_2 <- add_column(N_SDG_15_ab_2, SDG = "SDG_15_ab_2", .after = "Code")
# N_SDG_15_ab_2 <- subset(N_SDG_15_ab_2, nchar(as.character(N_SDG_15_ab_2$Code))==3) # remove aggregate data, e.g.world
# # length(unique(N_SDG_15_ab_2$Code)) # unique country code = 153
# # write.csv(N_SDG_15_ab_2,"data/SDSN/N_SDG_15_ab_2.csv")
# 
# # Integrate to SDG_15_ab
# N_SDG_15_ab <- rbind(N_SDG_15_ab_1, N_SDG_15_ab_2)
# N_SDG_15_ab <- subset(N_SDG_15_ab, nchar(as.character(N_SDG_15_ab$Code))==3)
# # length(unique(N_SDG_15_ab$Code)) # unique country code = 181
# # write.csv(N_SDG_15_ab,"data/SDSN/N_SDG_15_ab.csv")
# 
# ### Need to standardize the data ### Aid/GDP???
# 

##################
# SDG_15_Combine #
##################

N_SDG_15_combraw <- rbind(N_SDG_15_1_1, N_SDG_15_1_2, N_SDG_15_2_1,
                          N_SDG_15_4_1, N_SDG_15_4_2, N_SDG_15_5_1,
                          N_SDG_15_6_1, N_SDG_15_8_1, N_SDG_15_9_1,
                          N_SDG_15_9_2) # Not include SDG_15_ab yet
# names(N_SDG_15_combraw)[1] <- "Country"
names(N_SDG_15_combraw)[5] <- "Value"
                      
# write.csv(N_SDG_15_combraw, "data/SDSN/N_SDG_15_combraw.csv")

# N_SDG_15_combraw <- read.csv("data/SDSN/N_SDG_15_combraw.csv")

N_SDG_15_comb_00_20 <- subset(N_SDG_15_combraw, N_SDG_15_combraw$Year %in% interest_year)
# write.csv(N_SDG_15_comb_00_20, "data/SDSN/N_SDG_15_comb_00_20.csv")

# N_SDG_15_comb_00_20$Key <- paste(N_SDG_15_comb_00_20$Entity, N_SDG_15_comb_00_20$Code,
#                                  N_SDG_15_comb_00_20$SDG, N_SDG_15_comb_00_20$Year, 
#                                  sep="_")
# 
# 
# full_country_sdg_year$Key <- paste(full_country_sdg_year$Entity, full_country_sdg_year$Code,
#                                    full_country_sdg_year$SDG, full_country_sdg_year$Year, 
#                                    sep="_")
# 
# empty_full_country_sdg_year <- full_country_sdg_year
# # full_country_sdg_year <- empty_full_country_sdg_year
# 
# for (i in 1:nrow(N_SDG_15_comb_00_20)) {
#   N_SDG_15_dict <- c(N_SDG_15_comb_00_20$Key[i]=N_SDG_15_comb_00_20$Value[i])
# }
# 
# 
# 
# SDG_15_value_key_dict <- dict(init_keys = c(N_SDG_15_comb_00_20$Key), init_values = c(N_SDG_15_comb_00_20$Value))
# 
# 
# SDG_15_value_key_map <- setNames(c(N_SDG_15_comb_00_20$Key), c(N_SDG_15_comb_00_20$Value))
# 
# SDG_15_value_key_dict["Afghanistan_AFG_15_1_1_2001"]
# 
# 
# 
# SDG_15_value_key_vector["Afghanistan_AFG_15_1_1_2000"]
# 
# N_SDG_15_dict <- c(N_SDG_15_comb_00_20$Key, N_SDG_15_comb_00_20$Value)
# view(N_SDG_15_dict)
# 
# 
# for (i in 1:nrow(full_country_sdg_year)){
#   for (j in 1:nrow(N_SDG_15_comb_00_20)){
#     # print(full_country_sdg_year$Key[i]);
#     # print(N_SDG_15_comb_00_20$Key[j]);
#     if (full_country_sdg_year$Key[i] == N_SDG_15_comb_00_20$Key[j])
#     {
#       # print("detected");
#       full_country_sdg_year$Value[i] <- N_SDG_15_comb_00_20$Value[j];
#     }
#   }
# }
#
# The double for loops took too much computing capacity, thus inefficient;
# the above data set was achieved through Python pandas dictionary, neatly!


# N_SDG_15_comb_00_20 <- read.csv("data/SDSN/N_SDG_15_comb_00_20.csv")

### The new dataset has been created from Python

N_SDG_15_2000_2020 <- read.csv("data/SDSN/update_full_country_sdg_year.csv")

N_SDG_15_2000_2020_clean <- N_SDG_15_2000_2020[,c(3:6,9)]
names(N_SDG_15_2000_2020_clean)[1] <- "Country"
names(N_SDG_15_2000_2020_clean)[5] <- "value"

# write.csv(N_SDG_15_2000_2020_clean, "data/SDSN/SDG_15_all_unfill.csv")
# readr::write_csv()


##################
# SDG_15_fill_NA #
##################

# load data
csv <- "data/SDSN/SDG_15_all_unfill.csv"
unfill_SDG_15 <- readr::read_csv(csv)

## Indicator/subindicator that fits imputeTS interpolation method

# Continuous data suitable for imputation - sub/indicators 
# 15.1.1
# 15.1.2.1
# 15.1.2.2
# 15.2.1.1
# 15.2.1.2
# 15.2.1.3
# 15.4.1
# 15.4.2
# 15.5.1
# 15.6.1.5 
# 15.9.2

## Binary data NOT suitable for imputation - sub/indicators
## 15.6.1.1
## 15.6.1.2
## 15.6.1.3
## 15.6.1.4
## 15.8.1.1
## 15.8.1.2
## 15.9.1

# unique(unfill_SDG_15$SDG)
# create a suitable impuation list
SDG_Imputeable <- c("15_1_1", "15_1_2_1", "15_1_2_2", "15_2_1_1", "15_2_1_2",
                       "15_2_1_3", "15_4_1","15_4_2", "15_5_1", "15_6_1_5", "15_9_2")

## data with continuous indicator
dat1 <- unfill_SDG_15 %>% 
  dplyr::filter(SDG %in% SDG_Imputeable);

## data with binary indicator
dat2 <- unfill_SDG_15 %>% 
  dplyr::filter(!SDG %in% SDG_Imputeable);

## Impute continuous data of sub/indicators
source("script/function_fill_na_continuous.R")
dat1_filled <- function_fill_na_continuous(dat1) ### add return() in function
dat1_filled <- dat1_filled[,c(2:5,8)]
names(dat1_filled)[5] <- "Value"
# write.csv(dat1_filled, "data/SDSN/SDG_15_continuous_filled.csv")

# Manually fill binary data of sub/indicators
source("script/function_fill_na_binary.R")
dat2_filled <- function_fill_na_binary(dat2) ### add return() in function
dat2_filled <- dat2_filled[,c(2:5,7)]
# write.csv(dat2_filled, "data/SDSN/SDG_15_binary_filled.csv")
 
SDG_15_all_filled <- rbind(dat1_filled, dat2_filled)
# write.csv(SDG_15_all_filled, "data/SDSN/SDG_15_complete.csv")


##################
## SDG_15_score ##
##################

## Calculate SDG scores based on sub/indicators
SDG_15_all_filled <- read.csv("data/SDSN/SDG_15_complete.csv")

# create a new column for SDG_main indicator
SDG_15_all_filled$SDG_main <- substr(SDG_15_all_filled$SDG,1,4)
# create a new column for SDG_sub indicator
SDG_15_all_filled$SDG_sub <- substr(SDG_15_all_filled$SDG,1,6)


# create empty data frames for later use
df1 <- data.frame()
df_sub_total <- data.frame()
df_main_total <- data.frame()
df_overall_total <- data.frame()
n <- length(unique(SDG_15_all_filled$Code)) ## Code - county iso code
uni_code <- unique(SDG_15_all_filled$Code) ## unique country list by code

# loop through each country and calculate their SDG scores by sub and main indicators seperately
for (i in seq(1:n)) {
    print(i) 
    ### loop each county Code
    print(uni_code[i])
    
    df1 <- subset(SDG_15_all_filled, SDG_15_all_filled$Code==uni_code[i])
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

SDG_15_score_by_sub_indicator <- df_sub_total[,c(1:5)]
# write.csv(SDG_15_score_by_sub_indicator, "data/SDSN/SDG_15_score_by_sub_indicator.csv")

SDG_15_score_by_main_indicator <- df_main_total[,c(1:5)]
# write.csv(SDG_15_score_by_main_indicator, "data/SDSN/SDG_15_score_by_main_indicator.csv")

## SDG 15 overall score by country
SDG_15_score_overall <- df_overall_total
# write.csv(SDG_15_score_overall, "data/SDSN/SDG_15_score_overall.csv")


# summary(SDG_15_score_by_sub_indicator)
# summary(SDG_15_score_by_main_indicator)
# summary(SDG_15_score_overall)


# ## Test -----------------------------------------------------------------------
# 
# # create a subset for only ABW country from full data set
# df_ABW <- subset(SDG_15_all_filled, SDG_15_all_filled$Code=="ABW") 
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

### SDG 15 variation by each indicator with pool data ### ----------------------

### Unfilled data ###
N_SDG_15_comb_00_20 <- read.csv("data/SDSN/N_SDG_15_comb_00_20.csv")
N_SDG_15_comb_00_20$Year <- as.factor(N_SDG_15_comb_00_20$Year)
# Idea 1 - Scatter point for SDG by indicators
ind_analysis <- aggregate(N_SDG_15_comb_00_20[,6], 
                          list(N_SDG_15_comb_00_20$Year,N_SDG_15_comb_00_20$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

plot1 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 Indicator Average Value Change over Years with Unfilled Data",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Indicator Value");

# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot1


### Filled data ###
N_SDG_15_2000_2020_all <- read.csv("data/SDSN/SDG_15_complete.csv")
N_SDG_15_2000_2020_all$Year <- as.factor(N_SDG_15_2000_2020_all$Year)
# Idea 1 - Scatter point for SDG by indicators
ind_analysis2 <- aggregate(N_SDG_15_2000_2020_all[,6], 
                          list(N_SDG_15_2000_2020_all$Year,N_SDG_15_2000_2020_all$SDG), 
                          FUN = mean)
names(ind_analysis2)[1] <- "Year"
names(ind_analysis2)[2] <- "SDG"
names(ind_analysis2)[3] <- "Value"

plot2 <- ggplot(data = ind_analysis2, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 Indicator Average Value Change over Years with Imputed Data",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Indicator Value");

# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot2

### SDG 15 variation by each indicator with modified data ### ------------------

## By main indicator
SDG_15_score_by_main_indicator <- read.csv("data/SDSN/SDG_15_score_by_main_indicator.csv")
# SDG_15_score_by_main_indicator$Year <- as.factor(SDG_15_score_by_main_indicator$Year)

# Idea 1 - Scatter point for SDG by indicators
ind_analysis <- aggregate(SDG_15_score_by_main_indicator$Value, 
                           list(SDG_15_score_by_main_indicator$Year,SDG_15_score_by_main_indicator$SDG_main), 
                           FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

plot3 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1))+
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 Main Indicator Average Value Change over Years",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Main Indicator Value");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot3

## By sub indicator
SDG_15_score_by_sub_indicator <- read.csv("data/SDSN/SDG_15_score_by_sub_indicator.csv")
# SDG_15_score_by_sub_indicator$Year <- as.factor(SDG_15_score_by_sub_indicator$Year)

# Idea 1 - Scatter point for SDG by indicators
ind_analysis1 <- aggregate(SDG_15_score_by_sub_indicator$Value, 
                          list(SDG_15_score_by_sub_indicator$Year,SDG_15_score_by_sub_indicator$SDG_sub), 
                          FUN = mean)
names(ind_analysis1)[1] <- "Year"
names(ind_analysis1)[2] <- "SDG"
names(ind_analysis1)[3] <- "Value"

plot4 <- ggplot(data = ind_analysis1, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1))+
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 Sub Indicator Average Value Change over Years",
       subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Sub Indicator Value");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot4


### SDG 15 variation country ### -----------------------------------------------

# Idea 3 - boxplot

SDG_15_score_overall <- read.csv("data/SDSN/SDG_15_score_overall.csv")

SDG_15_score_overall$Year <- as.factor(SDG_15_score_overall$Year)
plot5 <- ggplot(data = SDG_15_score_overall) +
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
  labs(title = "SDG 15 Score Change over Years",
       subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Score");
plot5


# Idea 4 - violin plot
plot6 <- ggplot(data = SDG_15_score_overall) +
  geom_violin(mapping=aes(x=Year, y=Value),
              na.rm = TRUE) +
  # geom_smooth(method="lm") + 
  theme_bw() +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Value Change over Years",
       subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Score");
plot6


# Idea 5 - SDG score line graph with SD as shaded area

ind_analysis3 <- aggregate(SDG_15_score_overall$Value, 
                           list(SDG_15_score_overall$Year,SDG_15_score_overall$SDG), 
                           FUN = mean)
names(ind_analysis3)[1] <- "Year"
names(ind_analysis3)[2] <- "SDG"
names(ind_analysis3)[3] <- "Value"

sd <- aggregate(SDG_15_score_overall$Value, 
                   list(SDG_15_score_overall$Year,SDG_15_score_overall$SDG),
                   FUN = sd)

ind_analysis3$sd <- sd$x
ind_analysis3$lower <- ind_analysis3$Value - 0.5*ind_analysis3$sd
ind_analysis3$upper <- ind_analysis3$Value + 0.5*ind_analysis3$sd


plot7 <- ggplot(data = ind_analysis3, aes(x = Year, y = Value)) +
  geom_ribbon(aes(ymin = lower,ymax = upper), alpha = 0.2, fill = "seagreen") +
  geom_line(color='seagreen') +
  # scale_fill_manual(values='seagreen', name="fill") + 
  geom_point(color='seagreen') +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1))+
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Score Change over Years",
       subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Score");
plot7



### SDG 15 map of all countries/regions ----------------------------------------

## Not in use, but may be useful ---

# world <- map_data("world")
# head(world)
# 
# worldplot <- ggplot() +
#   geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
#   coord_fixed(1.3)
# worldplot
# 
# URL <- "https://github.com/nvkelso/natural-earth-vector/raw/master/geojson/ne_50m_admin_0_countries.geojson.gz"
# fil <- basename(URL)
# 
# if (!file.exists(fil)) download.file(URL, fil)
# R.utils::gunzip(fil)
# world <- readOGR(dsn="ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
# 
# # remove antarctica
# world <- world[!world$iso_a3 %in% c("ATA"),]
# 
# world <- spTransform(world, CRS("+proj=wintri"))
# 
# dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
# pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)
# 
# names(countrylist)[1] <- 'region' 
# 
# world2 <- merge(x = world, y = countrylist, by = "region", all = TRUE)
# 
# 
# diff <- setdiff(world$region, countrylist$region)
# diff
# 
# map_country_name <- data.frame(unique(world$region))
# 
# unique(countrylist$region)

## Selected years of SDG scores for all countries, 2000, 2005, 2010, 2015, 2020
SDG_15_score_overall <- read.csv("data/SDSN/SDG_15_score_overall.csv")
# SDG_15_score_overall_wide <- spread(SDG_15_score_overall, key = Year, value = Value)
SDG_15_score_overall_2000 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2000',]
SDG_15_score_overall_2005 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2005',]
SDG_15_score_overall_2010 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2010',]
SDG_15_score_overall_2015 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2015',]
SDG_15_score_overall_2020 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2020',]

# Create a data set of country's SDG change (slope of scores over years)
dat <- data.table(SDG_15_score_overall)
SDG_15_change <- dat[,as.list(coef(lm(Value~Year))), by=Code]
names(SDG_15_change)[3] <- 'Change'
# summary(SDG_15_change)

## shp ---

# head(shp)
shp <- ne_countries(scale = 'medium', type = 'countries', returnclass = 'sf') %>%
  dplyr::select(name, type, iso_a3, economy, income_grp) 

# Remove Antarctica
shp_nATA <- subset(shp, name != "Antarctica")
names(shp_nATA)[3] <- 'Code'

# Merge shp file with SDG score in 2000
shp_nATA_2000 <- merge(shp_nATA, SDG_15_score_overall_2000, by='Code')

plot8 <- ggplot(shp_nATA_2000) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2000") +
  scale_fill_distiller(palette='YlGn', direction = 1) +
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
shp_nATA_2005 <- merge(shp_nATA, SDG_15_score_overall_2005, by='Code')

plot9 <- ggplot(shp_nATA_2005) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2005") +
  scale_fill_distiller(palette='YlGn', direction = 1) +
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
shp_nATA_2010 <- merge(shp_nATA, SDG_15_score_overall_2010, by='Code')

plot10 <- ggplot(shp_nATA_2010) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2010") +
  scale_fill_distiller(palette='YlGn', direction = 1) +
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
shp_nATA_2015 <- merge(shp_nATA, SDG_15_score_overall_2015, by='Code')

plot11 <- ggplot(shp_nATA_2015) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2015") +
  scale_fill_distiller(palette='YlGn', direction = 1) +
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
shp_nATA_2020 <- merge(shp_nATA, SDG_15_score_overall_2020, by='Code')

plot12 <- ggplot(shp_nATA_2020) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2020") +
  scale_fill_distiller(palette='YlGn', direction = 1) +
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
shp_nATA_change <- merge(shp_nATA, SDG_15_change, by='Code')

plot13 <- ggplot(shp_nATA_change) +
  geom_sf(aes(fill=Change), size=0.1) + 
  ggtitle("Global SDG 15 change between 2000 and 2020") +
  scale_fill_distiller(palette='RdYlGn', direction = 1) +
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
# # diff <- setdiff(shp$iso_a3, SDG_15_score_overall$Code)
# # diff
# # 
# # shp$name[is.na(shp$iso_a3)]

