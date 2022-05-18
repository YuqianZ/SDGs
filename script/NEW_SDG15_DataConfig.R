
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

# Normalization
top_5 <- tail(sort(N_SDG_15_1_1$value),5)
bot_5 <- tail(sort(N_SDG_15_1_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_1_1$value_norm <- (N_SDG_15_1_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_15_1_1$value_norm[N_SDG_15_1_1$value_norm >1] <- 1
N_SDG_15_1_1$value_norm[N_SDG_15_1_1$value_norm <0] <- 0

# length(unique(N_SDG_15_1_1$Code)) # unique country code = 224
# write.csv(N_SDG_15_1_1,"data/SDSN/N_SDG_15_1_1.csv")

### All good to fill NAs ###

##################
### SDG_15_1_2 ###
##################

N_SDG_15_1_2_1 <- read.csv("data/SDSN/15.1.2.1_proportion-of-important-sites-for-freshwater-biodiversity-covered-by-protected-areas.csv")
names(N_SDG_15_1_2_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_1_2_1 <- add_column(N_SDG_15_1_2_1, SDG = "15_1_2_1", .after = "Code")

N_SDG_15_1_2_1 <- subset(N_SDG_15_1_2_1, nchar(as.character(N_SDG_15_1_2_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_1_2_1$value),5)
bot_5 <- tail(sort(N_SDG_15_1_2_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_1_2_1$value_norm <- (N_SDG_15_1_2_1$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_15_1_2_1$value_norm[N_SDG_15_1_2_1$value_norm >1] <- 1
N_SDG_15_1_2_1$value_norm[N_SDG_15_1_2_1$value_norm <0] <- 0

# length(unique(N_SDG_15_1_2_1$Code)) # unique country code = 160
# write.csv(N_SDG_15_1_2_1,"data/SDSN/N_SDG_15_1_2_1.csv")
# 
# N_SDG_15_1_2_1_wide <- N_SDG_15_1_2_1 %>%
#   spread(Year, value)

### All good to fill NAs ###

N_SDG_15_1_2_2 <- read.csv("data/SDSN/15.1.2.2_protected-terrestrial-biodiversity-sites.csv")
names(N_SDG_15_1_2_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_1_2_2 <- add_column(N_SDG_15_1_2_2, SDG = "15_1_2_2", .after = "Code")

N_SDG_15_1_2_2 <- subset(N_SDG_15_1_2_2, nchar(as.character(N_SDG_15_1_2_2$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_1_2_2$value),5)
bot_5 <- tail(sort(N_SDG_15_1_2_2$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_1_2_2$value_norm <- (N_SDG_15_1_2_2$value - lower) / (upper - lower)

# If value >1, assign 1, if value <0, assign 0
N_SDG_15_1_2_2$value_norm[N_SDG_15_1_2_2$value_norm >1] <- 1
N_SDG_15_1_2_2$value_norm[N_SDG_15_1_2_2$value_norm <0] <- 0

# length(unique(N_SDG_15_1_2_2$Code)) # unique country code = 235
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


### All good to fill NAs ###

##################
### SDG_15_2_1 ###
##################

N_SDG_15_2_1_1 <- read.csv("data/SDSN/15.2.1.1_forest-area-net-change-rate.csv")
names(N_SDG_15_2_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_2_1_1 <- add_column(N_SDG_15_2_1_1, SDG = "15_2_1_1", .after = "Code")

N_SDG_15_2_1_1<- subset(N_SDG_15_2_1_1, nchar(as.character(N_SDG_15_2_1_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_2_1_1$value),5)
bot_5 <- tail(sort(N_SDG_15_2_1_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_2_1_1$value_norm <- (N_SDG_15_2_1_1$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_2_1_1$value_norm[N_SDG_15_2_1_1$value_norm >1] <- 1
N_SDG_15_2_1_1$value_norm[N_SDG_15_2_1_1$value_norm <0] <- 0

# length(unique(N_SDG_15_2_1_1$Code)) # unique country code = 227
# write.csv(N_SDG_15_2_1_1,"data/SDSN/N_SDG_15_2_1_1.csv")
# summary(N_SDG_15_2_1_1$value)


# ### Needs value conversion before filling NAs ###
# 
# ### Thought: categorical -
# ### x<=-2, y=0
# ### -2<x<=0, y=1
# ### 0<x<=2, y=2
# ### 2<x<=4, y=3
# ### x>4, y=4
# 
# N_SDG_15_2_1_1$value_ed <- N_SDG_15_2_1_1$value
# N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value <= -2] <- 0
# N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > -2 & N_SDG_15_2_1_1$value <= 0] <- 1
# N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > 0 & N_SDG_15_2_1_1$value <= 2] <- 2
# N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > 2 & N_SDG_15_2_1_1$value <= 4] <- 3
# N_SDG_15_2_1_1$value_ed[N_SDG_15_2_1_1$value > 4] <- 4
# 
# N_SDG_15_2_1_1$value <- N_SDG_15_2_1_1$value_ed
# N_SDG_15_2_1_1 <- N_SDG_15_2_1_1[,1:5]
# N_SDG_15_2_1_1$value <- N_SDG_15_2_1_1$value/(max(N_SDG_15_2_1_1$value)) *100

### All good to fill NAs ###


N_SDG_15_2_1_2 <- read.csv("data/SDSN/15.2.1.2_proportion-of-forest-area-within-legally-established-protected-areas.csv")
names(N_SDG_15_2_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_2_1_2 <- add_column(N_SDG_15_2_1_2, SDG = "15_2_1_2", .after = "Code")

N_SDG_15_2_1_2 <- subset(N_SDG_15_2_1_2, nchar(as.character(N_SDG_15_2_1_2$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_2_1_2$value),5)
bot_5 <- tail(sort(N_SDG_15_2_1_2$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_2_1_2$value_norm <- (N_SDG_15_2_1_2$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_2_1_2$value_norm[N_SDG_15_2_1_2$value_norm >1] <- 1
N_SDG_15_2_1_2$value_norm[N_SDG_15_2_1_2$value_norm <0] <- 0

# length(unique(N_SDG_15_2_1_2$Code)) # unique country code = 169
# write.csv(N_SDG_15_2_1_2,"data/SDSN/N_SDG_15_2_1_2.csv")

# N_SDG_15_2_1_2$value_ed <- N_SDG_15_2_1_2$value
# N_SDG_15_2_1_2$value_ed[N_SDG_15_2_1_2$value_ed > 100] <- 100 
# N_SDG_15_2_1_2$value <- N_SDG_15_2_1_2$value_ed
# 
# N_SDG_15_2_1_2 <- N_SDG_15_2_1_2[,1:5]

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

N_SDG_15_2_1_3 <- subset(N_SDG_15_2_1_3, nchar(as.character(N_SDG_15_2_1_3$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_2_1_3$value),5)
bot_5 <- tail(sort(N_SDG_15_2_1_3$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_2_1_3$value_norm <- (N_SDG_15_2_1_3$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_2_1_3$value_norm[N_SDG_15_2_1_3$value_norm >1] <- 1
N_SDG_15_2_1_3$value_norm[N_SDG_15_2_1_3$value_norm <0] <- 0

# N_SDG_15_2_1_3$value <- N_SDG_15_2_1_3$value/(max(N_SDG_15_2_1_3$value)) # Transform range between 0 and 1, makes sense?
# N_SDG_15_2_1_3$value <- N_SDG_15_2_1_3$value * 100

##################### 15_2_1_3 needs further data transformation??????

# length(unique(N_SDG_15_2_1_3$Code)) # unique country code = 243
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

# Normalization
top_5 <- tail(sort(N_SDG_15_4_1$value),5)
bot_5 <- tail(sort(N_SDG_15_4_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_4_1$value_norm <- (N_SDG_15_4_1$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_4_1$value_norm[N_SDG_15_4_1$value_norm >1] <- 1
N_SDG_15_4_1$value_norm[N_SDG_15_4_1$value_norm <0] <- 0

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

# Normalization
top_5 <- tail(sort(N_SDG_15_4_2$value),5)
bot_5 <- tail(sort(N_SDG_15_4_2$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_4_2$value_norm <- (N_SDG_15_4_2$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_4_2$value_norm[N_SDG_15_4_2$value_norm >1] <- 1
N_SDG_15_4_2$value_norm[N_SDG_15_4_2$value_norm <0] <- 0


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

# Normalization
top_5 <- tail(sort(N_SDG_15_5_1$value),5)
bot_5 <- tail(sort(N_SDG_15_5_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_5_1$value_norm <- (N_SDG_15_5_1$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_5_1$value_norm[N_SDG_15_5_1$value_norm >1] <- 1
N_SDG_15_5_1$value_norm[N_SDG_15_5_1$value_norm <0] <- 0

# length(unique(N_SDG_15_5_1$Code)) # unique country code = 240
# write.csv(N_SDG_15_5_1,"data/SDSN/N_SDG_15_5_1.csv")

# N_SDG_15_5_1$value <- N_SDG_15_5_1$value *100

### All good to fill NAs ###


##################
### SDG_15_6_1 ###
##################

N_SDG_15_6_1_1 <- read.csv("data/SDSN/15.6.1.1_countries-to-the-international-treaty-on-plant-genetic-resources.csv")
names(N_SDG_15_6_1_1)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_6_1_1 <- add_column(N_SDG_15_6_1_1, SDG = "15_6_1_1", .after = "Code")

N_SDG_15_6_1_1 <- subset(N_SDG_15_6_1_1, nchar(as.character(N_SDG_15_6_1_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_6_1_1$value),5)
bot_5 <- tail(sort(N_SDG_15_6_1_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_6_1_1$value_norm <- (N_SDG_15_6_1_1$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_6_1_1$value_norm[N_SDG_15_6_1_1$value_norm >1] <- 1
N_SDG_15_6_1_1$value_norm[N_SDG_15_6_1_1$value_norm <0] <- 0

# length(unique(N_SDG_15_6_1_1$Code)) # unique country code = 243
# write.csv(N_SDG_15_6_1_1,"data/SDSN/N_SDG_15_6_1_1.csv")

# N_SDG_15_6_1_1 <- N_SDG_15_6_1_1[N_SDG_15_6_1_1$value<=1, ]
# N_SDG_15_6_1_1$value <- N_SDG_15_6_1_1$value *100

### All good to fill NAs ###

N_SDG_15_6_1_2 <- read.csv("data/SDSN/15.6.1.2_countries-that-are-parties-to-the-nagoya-protocol.csv")
names(N_SDG_15_6_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_6_1_2 <- add_column(N_SDG_15_6_1_2, SDG = "15_6_1_2", .after = "Code")

N_SDG_15_6_1_2 <- subset(N_SDG_15_6_1_2, nchar(as.character(N_SDG_15_6_1_2$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_6_1_2$value),5)
bot_5 <- tail(sort(N_SDG_15_6_1_2$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_6_1_2$value_norm <- (N_SDG_15_6_1_2$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_6_1_2$value_norm[N_SDG_15_6_1_2$value_norm >1] <- 1
N_SDG_15_6_1_2$value_norm[N_SDG_15_6_1_2$value_norm <0] <- 0

# length(unique(N_SDG_15_6_1_2$Code)) # unique country code = 197
# write.csv(N_SDG_15_6_1_2,"data/SDSN/N_SDG_15_6_1_2.csv")

# N_SDG_15_6_1_2 <- N_SDG_15_6_1_2[N_SDG_15_6_1_2$value<=1, ]
# N_SDG_15_6_1_2$value <- N_SDG_15_6_1_2$value *100

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

N_SDG_15_6_1_3 <- subset(N_SDG_15_6_1_3, nchar(as.character(N_SDG_15_6_1_3$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_6_1_3$value),5)
bot_5 <- tail(sort(N_SDG_15_6_1_3$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_6_1_3$value_norm <- (N_SDG_15_6_1_3$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_6_1_3$value_norm[N_SDG_15_6_1_3$value_norm >1] <- 1
N_SDG_15_6_1_3$value_norm[N_SDG_15_6_1_3$value_norm <0] <- 0

# length(unique(N_SDG_15_6_1_3$Code)) # unique country code = 233
# write.csv(N_SDG_15_6_1_3,"data/SDSN/N_SDG_15_6_1_3.csv")
 
# N_SDG_15_6_1_3$value <- N_SDG_15_6_1_3$value *100

### All good to fill NAs ###

N_SDG_15_6_1_4 <- read.csv("data/SDSN/15.6.1.4_countries-to-access-and-benefit-sharing-clearing-house.csv")
names(N_SDG_15_6_1_4)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_6_1_4 <- add_column(N_SDG_15_6_1_4, SDG = "15_6_1_4", .after = "Code")

N_SDG_15_6_1_4 <- subset(N_SDG_15_6_1_4, nchar(as.character(N_SDG_15_6_1_4$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_6_1_4$value),5)
bot_5 <- tail(sort(N_SDG_15_6_1_4$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_6_1_4$value_norm <- (N_SDG_15_6_1_4$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_6_1_4$value_norm[N_SDG_15_6_1_4$value_norm >1] <- 1
N_SDG_15_6_1_4$value_norm[N_SDG_15_6_1_4$value_norm <0] <- 0

# length(unique(N_SDG_15_6_1_4$Code)) # unique country code = 197
# write.csv(N_SDG_15_6_1_4,"data/SDSN/N_SDG_15_6_1_4.csv")

# N_SDG_15_6_1_4 <- N_SDG_15_6_1_4[N_SDG_15_6_1_4$value<=1, ]
# N_SDG_15_6_1_4$value <- N_SDG_15_6_1_4$value *100

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

N_SDG_15_6_1_5 <- subset(N_SDG_15_6_1_5, nchar(as.character(N_SDG_15_6_1_5$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_6_1_5$value),5)
bot_5 <- tail(sort(N_SDG_15_6_1_5$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_6_1_5$value_norm <- (N_SDG_15_6_1_5$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_6_1_5$value_norm[N_SDG_15_6_1_5$value_norm >1] <- 1
N_SDG_15_6_1_5$value_norm[N_SDG_15_6_1_5$value_norm <0] <- 0

# # Lower bound is 0, upper bound is max(~9.298) now
# N_SDG_15_6_1_5$value <- N_SDG_15_6_1_5$value/(max(N_SDG_15_6_1_5$value)) *100 # Transform range between 0 and 100%, makes sense?
# 
# ##################### 15_6_1_5 needs further data transformation??????

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

N_SDG_15_8_1_1 <- subset(N_SDG_15_8_1_1, nchar(as.character(N_SDG_15_8_1_1$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_8_1_1$value),5)
bot_5 <- tail(sort(N_SDG_15_8_1_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_8_1_1$value_norm <- (N_SDG_15_8_1_1$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_8_1_1$value_norm[N_SDG_15_8_1_1$value_norm >1] <- 1
N_SDG_15_8_1_1$value_norm[N_SDG_15_8_1_1$value_norm <0] <- 0

# length(unique(N_SDG_15_8_1_1$Code)) # unique country code = 195
# write.csv(N_SDG_15_8_1_1,"data/SDSN/N_SDG_15_8_1_1.csv")

# N_SDG_15_8_1_1$value <- N_SDG_15_8_1_1$value *100

### All good to fill NAs ###

N_SDG_15_8_1_2 <- read.csv("data/SDSN/15.8.1.2_budget-to-manage-invasive-alien-species.csv")
names(N_SDG_15_8_1_2)[4] <- "value"
# Add SDG subgoals in a new column
N_SDG_15_8_1_2 <- add_column(N_SDG_15_8_1_2, SDG = "15_8_1_2", .after = "Code")

N_SDG_15_8_1_2 <- subset(N_SDG_15_8_1_2, nchar(as.character(N_SDG_15_8_1_2$Code))==3)

# Normalization
top_5 <- tail(sort(N_SDG_15_8_1_2$value),5)
bot_5 <- tail(sort(N_SDG_15_8_1_2$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_8_1_2$value_norm <- (N_SDG_15_8_1_2$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_8_1_2$value_norm[N_SDG_15_8_1_2$value_norm >1] <- 1
N_SDG_15_8_1_2$value_norm[N_SDG_15_8_1_2$value_norm <0] <- 0

length(unique(N_SDG_15_8_1_2$Code)) # unique country code = 153
write.csv(N_SDG_15_8_1_2,"data/SDSN/N_SDG_15_8_1_2.csv")

# N_SDG_15_8_1_2$value <- N_SDG_15_8_1_2$value *100

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

# Normalization
top_5 <- tail(sort(N_SDG_15_9_1$value),5)
bot_5 <- tail(sort(N_SDG_15_9_1$value, decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_9_1$value_norm <- (N_SDG_15_9_1$value - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_9_1$value_norm[N_SDG_15_9_1$value_norm >1] <- 1
N_SDG_15_9_1$value_norm[N_SDG_15_9_1$value_norm <0] <- 0

# length(unique(N_SDG_15_9_1$Code)) # unique country code = 249
# write.csv(N_SDG_15_9_1,"data/SDSN/N_SDG_15_9_1.csv")
# N_SDG_15_9_1$value <- N_SDG_15_9_1$value *100

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

# Normalization
top_5 <- tail(sort(as.numeric(N_SDG_15_9_2$value)),5)
bot_5 <- tail(sort(as.numeric(N_SDG_15_9_2$value), decreasing = TRUE),5)
upper <- mean(top_5)
lower <- mean(bot_5)

N_SDG_15_9_2$value_norm <- (as.numeric(N_SDG_15_9_2$value) - lower) / (upper - lower)

# If value >1, assign 1; if value <0, assign 0
N_SDG_15_9_2$value_norm[N_SDG_15_9_2$value_norm >1] <- 1
N_SDG_15_9_2$value_norm[N_SDG_15_9_2$value_norm <0] <- 0

# length(unique(N_SDG_15_9_2$Code)) # unique country code = 249
# write.csv(N_SDG_15_9_2,"data/SDSN/N_SDG_15_9_2.csv")

# N_SDG_15_9_2$value <- as.numeric(N_SDG_15_9_2$value)
# N_SDG_15_9_2$value <- N_SDG_15_9_2$value/(max(N_SDG_15_9_2$value)) *100

# add 0 to the year of 2000 for all countries
# convert from long to wide
Np_SDG_15_9_2_wide <- spread(N_SDG_15_9_2, Year, value_norm)
Np_SDG_15_9_2_wide$"2000" <- 0
Np_SDG_15_9_2_wide <- Np_SDG_15_9_2_wide[, c(1:4,6,5)]
#convert back from wide to long
Np_SDG_15_9_2_long <- melt(Np_SDG_15_9_2_wide, id.vars = c("Entity","Code","SDG","value"))
names(Np_SDG_15_9_2_long)[5] <- "Year"
names(Np_SDG_15_9_2_long)[6] <- "value_norm"

N_SDG_15_9_2 <- Np_SDG_15_9_2_long[, c(1:3,5,4,6)]

N_SDG_15_9_2$value[N_SDG_15_9_2$Year == 2000] <- 0
# write.csv(N_SDG_15_9_2,"data/SDSN/N_SDG_15_9_2_fill.csv")

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

names(N_SDG_15_combraw)[6] <- "Value_norm_percent"     
N_SDG_15_combraw$Value_norm_percent <- N_SDG_15_combraw$Value_norm_percent *100
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

# regulate the range between 0 and 100
dat1_filled$Value[dat1_filled$Value >100] <- 100
dat1_filled$Value[dat1_filled$Value <0] <- 0

# write.csv(dat1_filled, "data/SDSN/SDG_15_continuous_filled.csv")

# Manually fill binary data of sub/indicators
source("script/function_fill_na_binary.R")
dat2_filled <- function_fill_na_binary(dat2) ### add return() in function
dat2_filled <- dat2_filled[,c(2:5,7)]
# write.csv(dat2_filled, "data/SDSN/SDG_15_binary_filled.csv")
 
SDG_15_all_filled <- rbind(dat1_filled, dat2_filled)
# write.csv(SDG_15_all_filled, "data/SDSN/SDG_15_complete.csv")


## Remove countries with more than 20% missing values ---

# create empty data frames for later use
df1 <- data.frame()
df_keep <- data.frame()
n <- length(unique(SDG_15_all_filled$Code)) ## Code - county iso code
uni_code <- unique(SDG_15_all_filled$Code) ## unique country list by code
missing_threshold  <- 0.5 # Can change the acceptance value between 0.1 - 0.5, the higher means allowing more missing value

# loop through each country and calculate their SDG scores by sub and main indicaotrs seperately
for (i in seq(1:n)) {
  print(i)
  ### loop each county Code
  print(uni_code[i])
  
  df1 <- subset(SDG_15_all_filled, SDG_15_all_filled$Code==uni_code[i])
  
  m <- nrow(df1)
  na_count <- 0
  
  for (j in seq(1:m)) {
    na_count <- na_count + is.na(df1$Value[j])
  }
  
  if (na_count <= missing_threshold * m) {
    df_keep <- rbind(df_keep, df1)
  } 
}

# SDG_15_all_filled_90 <- df_keep
# length(unique(SDG_15_all_filled_90$Code)) # Country number: 153
# write.csv(SDG_15_all_filled_90, "data/SDSN/SDG_15_complete_90.csv")
# 
# SDG_15_all_filled_80 <- df_keep
# length(unique(SDG_15_all_filled_80$Code)) # Country number: 198
# write.csv(SDG_15_all_filled_80, "data/SDSN/SDG_15_complete_80.csv")
# 
# SDG_15_all_filled_70 <- df_keep
# length(unique(SDG_15_all_filled_70$Code)) # Country number: 232
# write.csv(SDG_15_all_filled_70, "data/SDSN/SDG_15_complete_70.csv")
# 
# SDG_15_all_filled_60 <- df_keep
# length(unique(SDG_15_all_filled_60$Code)) # Country number: 243
# write.csv(SDG_15_all_filled_60, "data/SDSN/SDG_15_complete_60.csv")

SDG_15_all_filled_50 <- df_keep
# length(unique(SDG_15_all_filled_50$Code)) # Country number: 246
# write.csv(SDG_15_all_filled_50, "data/SDSN/SDG_15_complete_50.csv")




##################
## SDG_15_score ##
##################

## Calculate SDG scores based on sub/indicators
SDG_15_all_filled <- read.csv("data/SDSN/SDG_15_complete_50.csv")

# unique(SDG_15_all_filled$SDG)
# 
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_1_1"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_1_2_1"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_1_2_2"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_2_1_1"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_2_1_2"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_2_1_3"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_4_1"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_4_2"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_5_1"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_6_1_5"])
# hist(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_9_2"])
# 
# ## Normalize again the imputable continuous SDG scores
#  
# # SDG_Imputeable
# # length(SDG_Imputeable)
# 
# for (i in 1:length(SDG_Imputeable)){
#   
#   top_5 <- tail(sort(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == SDG_Imputeable[i]]),5)
#   bot_5 <- tail(sort(SDG_15_all_filled$Value[SDG_15_all_filled$SDG == SDG_Imputeable[i]], decreasing = TRUE),5)
#   upper <- mean(top_5)
#   lower <- mean(bot_5)
#   
#   SDG_15_all_filled$Value_norm[SDG_15_all_filled$SDG == SDG_Imputeable[i]] <- 
#     (SDG_15_all_filled$Value[SDG_15_all_filled$SDG == SDG_Imputeable[i]] - lower) / (upper - lower)
#   
# }
# 
# # If value >1, assign 1; if value <0, assign 0
# SDG_15_all_filled$Value_norm[SDG_15_all_filled$Value_norm >1] <- 1
# SDG_15_all_filled$Value_norm[SDG_15_all_filled$Value_norm <0] <- 0
# 
# SDG_15_all_filled$Value <- SDG_15_all_filled$Value_norm *100
# SDG_15_all_filled <- SDG_15_all_filled[,c(1:6)]
# # write.csv(SDG_15_all_filled, "data/SDSN/SDG_15_complete_norm.csv")


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
# write.csv(SDG_15_score_by_sub_indicator, "data/SDSN/SDG_15_score_by_sub_indicator_50.csv")

SDG_15_score_by_main_indicator <- df_main_total[,c(1:5)]
# write.csv(SDG_15_score_by_main_indicator, "data/SDSN/SDG_15_score_by_main_indicator_50.csv")

## SDG 15 overall score by country
SDG_15_score_overall <- df_overall_total
# write.csv(SDG_15_score_overall, "data/SDSN/SDG_15_score_overall_50.csv")


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
SDG_15_score_by_main_indicator <- read.csv("data/SDSN/SDG_15_score_by_main_indicator_50.csv")
# SDG_15_score_by_main_indicator$Year <- as.factor(SDG_15_score_by_main_indicator$Year)

# Idea 1 - Scatter point for SDG by indicators
ind_analysis <- aggregate(SDG_15_score_by_main_indicator$Value, 
                           list(SDG_15_score_by_main_indicator$Year,SDG_15_score_by_main_indicator$SDG_main), 
                           FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

# SDG_15_5 <- subset(ind_analysis, ind_analysis$SDG=="15_5")
# SDG_15_1 <- subset(ind_analysis, ind_analysis$SDG=="15_1")
# SDG_15_2 <- subset(ind_analysis, ind_analysis$SDG=="15_2")
# SDG_15_4 <- subset(ind_analysis, ind_analysis$SDG=="15_4")
# SDG_15_6 <- subset(ind_analysis, ind_analysis$SDG=="15_6")
# SDG_15_8 <- subset(ind_analysis, ind_analysis$SDG=="15_8")
# SDG_15_9 <- subset(ind_analysis, ind_analysis$SDG=="15_9")

plot3 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size=2, aes(color=SDG)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1))+
  geom_line(size=1, aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 Target Average Value Change Between 2000 and 2020",
       # subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Target Value",
       col="SDG Target");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot3

## By sub indicator
SDG_15_score_by_sub_indicator <- read.csv("data/SDSN/SDG_15_score_by_sub_indicator_50.csv")
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
  geom_line(size=1, aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 Indicator Average Value Change Between 2000 and 2020",
       # subtitle = "249 Countries/Regions between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Indicator Value",
       col="SDG Indicator");
# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot4


### SDG 15 variation country ### -----------------------------------------------

# Idea 3 - boxplot

SDG_15_score_overall <- read.csv("data/SDSN/SDG_15_score_overall_50.csv")

# SDG_15_score_overall$Year <- as.factor(SDG_15_score_overall$Year)
# plot5 <- ggplot(data = SDG_15_score_overall) +
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
#   labs(title = "SDG 15 Score Change over Years",
#        subtitle = "249 Countries between 2000 and 2020",
#        x = "Year",
#        y = "SDG 15 Score");
# plot5
# 
# 
# # Idea 4 - violin plot
# plot6 <- ggplot(data = SDG_15_score_overall) +
#   geom_violin(mapping=aes(x=Year, y=Value),
#               na.rm = TRUE) +
#   # geom_smooth(method="lm") + 
#   theme_bw() +
#   # scale_x_discrete(labels = year) +
#   labs(title = "SDG 15 Value Change over Years",
#        subtitle = "249 Countries between 2000 and 2020",
#        x = "Year",
#        y = "SDG 15 Score");
# plot6


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

# head(ind_analysis3)
# summary(ind_analysis3)

plot7 <- ggplot(data = ind_analysis3, aes(x = Year, y = Value)) +
  geom_ribbon(aes(ymin = lower,ymax = upper), alpha = 0.2, fill = "seagreen") +
  geom_line(size=1, color='seagreen') +
  # scale_fill_manual(values='seagreen', name="fill") + 
  geom_point(size=2, color='seagreen') +
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
  labs(title = "SDG 15 Score Between 2000 and 2020",
       # subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG 15 Score");
plot7


## SDG score by categorized country 

# Country selection based on biodiversity hotspot and economy status --> from Chung and Liu's paper
Country_category <- read.csv("data/SDSN/Country category.csv")
names(Country_category)[1] <- "Code"
Country_category <- Country_category[,c(1,3)]

SDG_15_score_overall_cat <- merge(SDG_15_score_overall, Country_category, by="Code")
SDG_15_score_overall_cat <- SDG_15_score_overall_cat[,c(1,3,7,4:6)]

# table(SDG_15_score_overall_cat$Group)/21
# length(unique(SDG_15_score_overall_cat$Code)) # Country number: 157

# Idea 6 - SDG score line graph with SD as shaded area

ind_analysis5 <- aggregate(SDG_15_score_overall_cat$Value, 
                           list(SDG_15_score_overall_cat$Year,SDG_15_score_overall_cat$SDG, 
                                SDG_15_score_overall_cat$Group), 
                           FUN = mean)
names(ind_analysis5)[1] <- "Year"
names(ind_analysis5)[2] <- "SDG"
names(ind_analysis5)[3] <- "Group"
names(ind_analysis5)[4] <- "Value"

# Give meaningful names to categories
ind_analysis5$Group[ind_analysis5$Group=="HHC_H"] <- "High Hotspot (>50%), High Income"
ind_analysis5$Group[ind_analysis5$Group=="HHC_L"] <- "High Hotspot (>50%), Low Income"
ind_analysis5$Group[ind_analysis5$Group=="LHC_H"] <- "Low Hotspot (<50%), High Income"
ind_analysis5$Group[ind_analysis5$Group=="LHC_L"] <- "Low Hotspot (<50%), Low Income"
ind_analysis5$Group[ind_analysis5$Group=="NHC_H"] <- "No Hotspot (0%), High Income"
ind_analysis5$Group[ind_analysis5$Group=="NHC_L"] <- "No Hotspot (0%), Low Income"

# sd <- aggregate(SDG_15_score_overall_cat$Value, 
#                 list(SDG_15_score_overall_cat$Year,SDG_15_score_overall_cat$SDG, 
#                      SDG_15_score_overall_cat$Group), 
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
        # panel.background = element_rect(fill='#e5f5e0'),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Score Between 2000 and 2020",
       subtitle = "By biodiversity hotspot and income level",
       x = "Year",
       y = "SDG 15 Score");
plot17

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
SDG_15_score_overall <- read.csv("data/SDSN/SDG_15_score_overall_50.csv")
# SDG_15_score_overall_wide <- spread(SDG_15_score_overall, key = Year, value = Value)
SDG_15_score_overall_2000 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2000',]
SDG_15_score_overall_2005 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2005',]
SDG_15_score_overall_2010 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2010',]
SDG_15_score_overall_2015 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2015',]
SDG_15_score_overall_2020 <- SDG_15_score_overall[SDG_15_score_overall$Year=='2020',]

# Create a data set of country's SDG change (slope of scores over years)
dat <- data.table(SDG_15_score_overall)
SDG_15_change <- dat[,as.list(coef(lm(Value~Year))), by=Code]
names(SDG_15_change)[3] <- 'Change_rate'
# summary(SDG_15_change)

# summary(SDG_15_score_overall)

# SDG_15_score_change_abs <- SDG_15_score_overall_2000
# SDG_15_score_change_abs$change <- SDG_15_score_overall_2020$Value - SDG_15_score_overall_2000$Value
# names(SDG_15_score_change_abs)[6] <- "Value_before"
# SDG_15_score_change_abs$Value_after <- SDG_15_score_overall_2020$Value
# SDG_15_score_change_abs$change_percent <- SDG_15_score_change_abs$change / SDG_15_score_change_abs$Value_before
# 
# order.change <- order(SDG_15_score_change_abs$change_percent)
# SDG_15_score_change_abs$rank[order.change] <- 1:nrow(SDG_15_score_change_abs)
# 
# SDG_15_score_change_abs <- SDG_15_score_change_abs[!is.na(SDG_15_score_change_abs$change_percent),]
# 
# SDG_15_score_change_abs$rank <- 245 - SDG_15_score_change_abs$rank
# SDG_15_score_change_abs$change_percent <- SDG_15_score_change_abs$change_percent *100
# # Country SDG 15 increase by percentage and rankings


## shp ---

# head(shp)
shp <- ne_countries(scale = 'medium', type = 'countries', returnclass = 'sf') %>%
  dplyr::select(name, type, iso_a3, economy, income_grp) 

# Remove Antarctica
shp_nATA <- subset(shp, name != "Antarctica")
names(shp_nATA)[3] <- 'Code'

# Merge shp file with SDG score in 2000
shp_nATA_2000 <- merge(shp_nATA, SDG_15_score_overall_2000, by='Code', all=TRUE)
# summary(shp_nATA_2000$Value)

# shp_nATA_2000_analysis <- shp_nATA_2000[,c(1,2,7,10)]
# 
# mean <- mean(shp_nATA_2000_analysis$Value)
# sd <- sd(shp_nATA_2000_analysis$Value)
# 
# high <- mean + sd
# low <- mean - sd
# 
# shp_nATA_2000_top5 <- unique(shp_nATA_2000_analysis$Country[shp_nATA_2000_analysis$Value >= (high+0.5*sd)])
# shp_nATA_2000_bottom5 <- unique(shp_nATA_2000_analysis$Country[shp_nATA_2000_analysis$Value <= (low-0.5*sd) &
#                                                                  shp_nATA_2000_analysis$Value != 0])
# 
# shp_nATA_2000_top5
# shp_nATA_2000_bottom5
# 
# # Find the rank of several countries -
# # Biodiversity hotspots: Brazil, South Africa, Madagascar, Ecuador, Mexico,
# # United States, China, Philippines, Indonesia, India?
# 
# bio_hot_country <- c("BRA", "ZAF", "MDG", "ECU", "MEX", "USA", "CHN", "PHL", "IDN", "IND")
# 
# shp_nATA_2000_analysis$rank <- NA
# order.values <- order(shp_nATA_2000_analysis$Value)
# shp_nATA_2000_analysis$rank[order.values] <- 1:nrow(shp_nATA_2000_analysis)
# 
# Bio_hot_2000 <- shp_nATA_2000_analysis[shp_nATA_2000_analysis$Code %in% bio_hot_country,]
# #

plot8 <- ggplot(shp_nATA_2000) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2000") +
  scale_fill_distiller(palette='YlGn', direction = 1, limits=c(0,90), na.value = 'grey') +
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
shp_nATA_2005 <- merge(shp_nATA, SDG_15_score_overall_2005, by='Code', all=TRUE)
# summary(shp_nATA_2005$Value)

plot9 <- ggplot(shp_nATA_2005) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2005") +
  scale_fill_distiller(palette='YlGn', direction = 1, limits=c(0,90), na.value = 'grey') +
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
shp_nATA_2010 <- merge(shp_nATA, SDG_15_score_overall_2010, by='Code', all=TRUE)
# summary(shp_nATA_2010$Value)

plot10 <- ggplot(shp_nATA_2010) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2010") +
  scale_fill_distiller(palette='YlGn', direction = 1, limits=c(0,90), na.value = 'grey') +
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
shp_nATA_2015 <- merge(shp_nATA, SDG_15_score_overall_2015, by='Code', all=TRUE)
# summary(shp_nATA_2015$Value)

plot11 <- ggplot(shp_nATA_2015) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2015") +
  scale_fill_distiller(palette='YlGn', direction = 1, limits=c(0,90), na.value = 'grey') +
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
shp_nATA_2020 <- merge(shp_nATA, SDG_15_score_overall_2020, by='Code', all=TRUE)
# summary(shp_nATA_2020$Value)

# shp_nATA_2020_analysis <- shp_nATA_2020[,c(1,2,7,10)]
# 
# mean2 <- mean(shp_nATA_2020_analysis$Value)
# sd2 <- sd(shp_nATA_2020_analysis$Value)
# 
# high2 <- mean2 + sd2
# low2 <- mean2 - sd2
# 
# shp_nATA_2020_top5 <- unique(shp_nATA_2020_analysis$Country[shp_nATA_2020_analysis$Value >= (high2+0.5*sd2)])
# shp_nATA_2020_bottom5 <- unique(shp_nATA_2020_analysis$Country[shp_nATA_2020_analysis$Value <= (low2-0.5*sd2) &
#                                                                  shp_nATA_2020_analysis$Value != 0])
# 
# shp_nATA_2020_top5
# shp_nATA_2020_bottom5
# 
# # Find the rank of several countries - 
# # Biodiversity hotspots: Brazil, South Africa, Madagascar, Ecuador, Mexico, 
# # United States, China, Philippines, Indonesia, India?
# 
# bio_hot_country <- c("BRA", "ZAF", "MDG", "ECU", "MEX", "USA", "CHN", "PHL", "IDN", "IND")
# 
# shp_nATA_2020_analysis$rank <- NA
# order.values <- order(shp_nATA_2020_analysis$Value)
# shp_nATA_2020_analysis$rank[order.values] <- 1:nrow(shp_nATA_2020_analysis)
#   
# Bio_hot_2020 <- shp_nATA_2020_analysis[shp_nATA_2020_analysis$Code %in% bio_hot_country,]
# #

# # Compare 2000 and 2020 biodiversity hotspot countries' ranking
# Bio_compare_2000 <- Bio_hot_2000[,c(1,3,4,6)]
# names(Bio_compare_2000)[4] <- "Rank_2000"
# Bio_compare_2000$Rank_2000 <- 235 - Bio_compare_2000$Rank_2000
# 
# Bio_compare_2000$rank_value_2000 <- paste(Bio_compare_2000$Rank_2000, Bio_compare_2000$Value, sep="_")
# 
# bio_compare_2000 <- Bio_compare_2000[,c(1,2,6)]
# 
# 
# Bio_compare_2020 <- Bio_hot_2020[,c(1,3,4,6)]
# names(Bio_compare_2020)[4] <- "Rank_2020"
# Bio_compare_2020$Rank_2020 <- 235 - Bio_compare_2020$Rank_2020
# 
# Bio_compare_2020$rank_value_2020 <- paste(Bio_compare_2020$Rank_2020, Bio_compare_2020$Value, sep="_")
# 
# bio_compare_2020 <- Bio_compare_2020[,c(1,2,6)]
# 
# bio_compare_final <- cbind(bio_compare_2000, bio_compare_2020)
# bio_compare_final <- bio_compare_final[,c(1,2,3,6)]



# N_SDG_15_comb_00_20$Key <- paste(N_SDG_15_comb_00_20$Entity, N_SDG_15_comb_00_20$Code,
#                                  N_SDG_15_comb_00_20$SDG, N_SDG_15_comb_00_20$Year, 
#                                  sep="_")

plot12 <- ggplot(shp_nATA_2020) +
  geom_sf(aes(fill=Value), size=0.1) + 
  ggtitle("Global SDG 15 in 2020") +
  scale_fill_distiller(palette='YlGn', direction = 1, limits=c(0,90), na.value = 'grey') +
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
shp_nATA_change <- merge(shp_nATA, SDG_15_change, by='Code', all=TRUE)
# 
# shp_nATA_change_analysis <- shp_nATA_change[, c(1,2,7)]
# shp_nATA_no_change <- unique(shp_nATA_change_analysis$name[shp_nATA_change_analysis$Change_rate <=0])

# shp_nATA_no_change

# summary(shp_nATA_change$Change_rate)

plot13 <- ggplot(shp_nATA_change) +
  geom_sf(aes(fill=Change_rate), size=0.1) + 
  ggtitle("Global SDG 15 change between 2000 and 2020") +
  scale_fill_distiller(palette='RdYlGn', direction = 1, na.value = 'grey',
                       name='Change rate per year') +
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
SDG_15_2000_2015 <- SDG_15_score_overall_2000
SDG_15_2000_2015$Value_change <- SDG_15_score_overall_2015$Value - SDG_15_score_overall_2000$Value

# Merge shp file with SDG score change
shp_nATA_change_2000_2015 <- merge(shp_nATA, SDG_15_2000_2015, by='Code', all=TRUE)
# summary(shp_nATA_change_2000_2015$Value_change)

plot21 <- ggplot(shp_nATA_change_2000_2015) +
  geom_sf(aes(fill=Value_change), size=0.1) + 
  ggtitle("Global SDG 15 change between 2000 and 2015") +
  scale_fill_distiller(palette='RdYlGn', direction = 1, limits=c(-5,40),
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

# summary(SDG_15_2000_2015$Value_change)

## change between 2015 and 2020
SDG_15_2015_2020 <- SDG_15_score_overall_2015
SDG_15_2015_2020$Value_change <- SDG_15_score_overall_2020$Value - SDG_15_score_overall_2015$Value

# Merge shp file with SDG score change
shp_nATA_change_2015_2020 <- merge(shp_nATA, SDG_15_2015_2020, by='Code', all=TRUE)
# summary(shp_nATA_change_2015_2020$Value_change)

plot22 <- ggplot(shp_nATA_change_2015_2020) +
  geom_sf(aes(fill=Value_change), size=0.1) + 
  ggtitle("Global SDG 15 change between 2015 and 2020") +
  scale_fill_distiller(palette='RdYlGn', direction = 1, limits=c(-5,40),
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

# summary(SDG_15_2015_2020$Value_change)

# Change legend size function
addSmallLegend <- function(myPlot, pointSize = 10, textSize = 8, spaceLegend = 0.8) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize+2), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}


### Selection analysis ######

# Country selection based on biodiversity hotspot and economy status --> from Chung and Liu's paper
Country_category <- read.csv("data/SDSN/Country category.csv")
names(Country_category)[1] <- "Code"
Country_category <- Country_category[,c(1,3)]

# Merge categories with countrylist
Country_full <- merge(countrylist, Country_category, by="Code", all=TRUE)

# Include only countreis in this study (n=147)
country_code_use <- unique(SDG_15_score_overall$Code)
Country_full_select <- Country_full[Country_full$Code %in% country_code_use,]
Country_full_select <- Country_full_select[,c(1,3)]
Country_full <- merge(countrylist,  Country_full_select, by="Code", all=TRUE)
# table(Country_full$Group)

# unique(Country_category$Group)
# Give meaningful names to categories
Country_full$Group[Country_full$Group=="HHC_H"] <- "High Hotspot (>50%), High Income (14)"
Country_full$Group[Country_full$Group=="HHC_L"] <- "High Hotspot (>50%), Low Income (50)"
Country_full$Group[Country_full$Group=="LHC_H"] <- "Low Hotspot (<50%), High Income (10)"
Country_full$Group[Country_full$Group=="LHC_L"] <- "Low Hotspot (<50%), Low Income (40)"
Country_full$Group[Country_full$Group=="NHC_H"] <- "No Hotspot (0%), High Income (23)"
Country_full$Group[Country_full$Group=="NHC_L"] <- "No Hotspot (0%), Low Income (20)"
Country_full$Group[is.na(Country_full$Group)] <- "Uncategorized - No Data (n=92)"



# Categorized country map
# Merge shp file with SDG score change
shp_nATA_cat <- merge(shp_nATA, Country_full, by='Code')

plot99 <- ggplot(shp_nATA_cat) +
  geom_sf(aes(fill=Group), size=0.1) + 
  ggtitle("Countries' Spatial Distribution by Biodiveristy Hotspot and Income Level for SDG 15") +
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
# plot99

plot100 <- addSmallLegend(plot99)
plot100


# Arrange plots into one as output
# plot14 <- grid.arrange(plot8, plot9, plot10, plot11, plot12,
#                        nrow=3)
# 
# plot14
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


# country_N <- unique(SDG_15_all_filled$Code)
# length(country_N)
# unique(SDG_15_all_filled$SDG)
# 
# SDG_15_2_1_1 <- SDG_15_all_filled[SDG_15_all_filled$SDG == "15_2_1_1",]


# ## Regression analysis
# 
# # sdg_ind <- unique(SDG_15_all_filled$SDG)
# # 
# # SDG_15_reg <- SDG_15_score_overall[,c(2,3,4,6)]
# # names(SDG_15_reg)[4] <- "SDG_15"
# # 
# # for (i in (1:length(sdg_ind))){
# #   SDG_15_reg[,i+4] <- SDG_15_all_filled$Value[SDG_15_all_filled$SDG == sdg_ind[i]]
# # }
# # 
# # SDG_15_lm <- lm(SDG_15 ~ V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
# #                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = SDG_15_reg)
# # summary(SDG_15_lm)
# # 
# # SDG_15_lm_did <- lm(SDG_15 ~ V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
# #                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + factor(Country), data = SDG_15_reg)
# # summary(SDG_15_lm_did)
# 
# # SDG_15_reg$"15_1_1" <- SDG_15_all_filled$Value[SDG_15_all_filled$SDG == "15_1_1"]


