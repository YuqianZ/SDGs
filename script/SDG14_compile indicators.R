
## SDG 14 Indicator compilation
## 2021-02-15
## Yuqian Zhang


# Library deployment

source( file="script/reference.R" );

# Set working directory

setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG")
getwd()

# Import data SDG_14_1

SDG_14_1 <- read.csv("data/short_SDG_14_1.csv")

names(SDG_14_1)[2] <- "Country"
names(SDG_14_1)[3] <- "2000"
names(SDG_14_1)[4] <- "2001"
names(SDG_14_1)[5] <- "2002"
names(SDG_14_1)[6] <- "2003"
names(SDG_14_1)[7] <- "2004"
names(SDG_14_1)[8] <- "2005"
names(SDG_14_1)[9] <- "2006"
names(SDG_14_1)[10] <- "2007"
names(SDG_14_1)[11] <- "2008"
names(SDG_14_1)[12] <- "2009"
names(SDG_14_1)[13] <- "2010"
names(SDG_14_1)[14] <- "2011"
names(SDG_14_1)[15] <- "2012"
names(SDG_14_1)[16] <- "2013"
names(SDG_14_1)[17] <- "2014"
names(SDG_14_1)[18] <- "2015"
names(SDG_14_1)[19] <- "2016"
names(SDG_14_1)[20] <- "2017"
names(SDG_14_1)[21] <- "2018"
names(SDG_14_1)[22] <- "2019"


# Import data SDG_14_5

SDG_14_5 <- read.csv("data/short_SDG_14_5.csv")

names(SDG_14_5)[2] <- "Country"
names(SDG_14_5)[3] <- "2000"
names(SDG_14_5)[4] <- "2001"
names(SDG_14_5)[5] <- "2002"
names(SDG_14_5)[6] <- "2003"
names(SDG_14_5)[7] <- "2004"
names(SDG_14_5)[8] <- "2005"
names(SDG_14_5)[9] <- "2006"
names(SDG_14_5)[10] <- "2007"
names(SDG_14_5)[11] <- "2008"
names(SDG_14_5)[12] <- "2009"
names(SDG_14_5)[13] <- "2010"
names(SDG_14_5)[14] <- "2011"
names(SDG_14_5)[15] <- "2012"
names(SDG_14_5)[16] <- "2013"
names(SDG_14_5)[17] <- "2014"
names(SDG_14_5)[18] <- "2015"
names(SDG_14_5)[19] <- "2016"
names(SDG_14_5)[20] <- "2017"
names(SDG_14_5)[21] <- "2018"
names(SDG_14_5)[22] <- "2019"


## Find common countries from SDG_14_1 and SDG_14_5
L1 <- intersect(SDG_14_1$Country, SDG_14_5$Country)

# length(unique(SDG_14_1$Country))
# length(unique(SDG_14_5$Country))
# length(unique(SDG_14_6$Country))
# length(unique(SDG_14_7$Country))
# length(unique(SDG_14_b$Country))

# Import data SDG_14_6

SDG_14_6 <- read.csv("data/short_SDG_14_6.csv")

names(SDG_14_6)[2] <- "Country"
names(SDG_14_6)[3] <- "2000"
names(SDG_14_6)[4] <- "2001"
names(SDG_14_6)[5] <- "2002"
names(SDG_14_6)[6] <- "2003"
names(SDG_14_6)[7] <- "2004"
names(SDG_14_6)[8] <- "2005"
names(SDG_14_6)[9] <- "2006"
names(SDG_14_6)[10] <- "2007"
names(SDG_14_6)[11] <- "2008"
names(SDG_14_6)[12] <- "2009"
names(SDG_14_6)[13] <- "2010"
names(SDG_14_6)[14] <- "2011"
names(SDG_14_6)[15] <- "2012"
names(SDG_14_6)[16] <- "2013"
names(SDG_14_6)[17] <- "2014"
names(SDG_14_6)[18] <- "2015"
names(SDG_14_6)[19] <- "2016"
names(SDG_14_6)[20] <- "2017"
names(SDG_14_6)[21] <- "2018"
SDG_14_6$"2019" <- SDG_14_6$"2018"

## Find common countries from SDG_14_1 and SDG_14_5, SDG_14_6
L2 <- intersect(L1, SDG_14_6$Country)

# Import data SDG_14_7

SDG_14_7 <- read.csv("data/short_SDG_14_7.csv")

names(SDG_14_7)[2] <- "Country"
names(SDG_14_7)[3] <- "2000"
names(SDG_14_7)[4] <- "2001"
names(SDG_14_7)[5] <- "2002"
names(SDG_14_7)[6] <- "2003"
names(SDG_14_7)[7] <- "2004"
names(SDG_14_7)[8] <- "2005"
names(SDG_14_7)[9] <- "2006"
names(SDG_14_7)[10] <- "2007"
names(SDG_14_7)[11] <- "2008"
names(SDG_14_7)[12] <- "2009"
names(SDG_14_7)[13] <- "2010"
names(SDG_14_7)[14] <- "2011"
names(SDG_14_7)[15] <- "2012"
names(SDG_14_7)[16] <- "2013"
names(SDG_14_7)[17] <- "2014"
names(SDG_14_7)[18] <- "2015"
names(SDG_14_7)[19] <- "2016"
names(SDG_14_7)[20] <- "2017"
SDG_14_7$"2018" <- length(SDG_14_7$Country)*NA
SDG_14_7$"2019" <- length(SDG_14_7$Country)*NA

## Find common countries from SDG_14_1 and SDG_14_5, SDG_14_6, SDG_14_7
L3 <- intersect(L2, SDG_14_7$Country)


# Import data SDG_14_b

SDG_14_b <- read.csv("data/short_SDG_14_b.csv")

names(SDG_14_b)[2] <- "Country"
names(SDG_14_b)[3] <- "2000"
names(SDG_14_b)[4] <- "2001"
names(SDG_14_b)[5] <- "2002"
names(SDG_14_b)[6] <- "2003"
names(SDG_14_b)[7] <- "2004"
names(SDG_14_b)[8] <- "2005"
names(SDG_14_b)[9] <- "2006"
names(SDG_14_b)[10] <- "2007"
names(SDG_14_b)[11] <- "2008"
names(SDG_14_b)[12] <- "2009"
names(SDG_14_b)[13] <- "2010"
names(SDG_14_b)[14] <- "2011"
names(SDG_14_b)[15] <- "2012"
names(SDG_14_b)[16] <- "2013"
names(SDG_14_b)[17] <- "2014"
names(SDG_14_b)[18] <- "2015"
names(SDG_14_b)[19] <- "2016"
names(SDG_14_b)[20] <- "2017"
names(SDG_14_b)[21] <- "2018"
SDG_14_b$"2019" <- SDG_14_b$"2018"

## Find common countries from SDG_14_1 and SDG_14_5, SDG_14_6, SDG_14_7, SDG_14_b
L4 <- intersect(L3, SDG_14_b$Country)


## Standardize each SDG 14 indicators in 100 percent, each indicator weighs 0.2;
## and combine the dataset
sub_SDG_14_1 <- subset(SDG_14_1, SDG_14_1$Country %in% L4)
sub_SDG_14_1$SDG <- "14.1"

sub_SDG_14_5 <- subset(SDG_14_5, SDG_14_5$Country %in% L4)
sub_SDG_14_5$SDG <- "14.5"

sub_SDG_14_6 <- subset(SDG_14_6, SDG_14_6$Country %in% L4)
sub_SDG_14_6$SDG <- "14.6"
sub_SDG_14_6$"2018"<- as.numeric(sub_SDG_14_6$"2018") *20
sub_SDG_14_6$"2019"<- as.numeric(sub_SDG_14_6$"2019") *20

sub_SDG_14_7 <- subset(SDG_14_7, SDG_14_7$Country %in% L4)
sub_SDG_14_7$SDG <- "14.7"

sub_SDG_14_b <- subset(SDG_14_b, SDG_14_b$Country %in% L4)
sub_SDG_14_b$SDG <- "14.b"
sub_SDG_14_b$"2018"<- as.numeric(sub_SDG_14_b$"2018") *20
sub_SDG_14_b$"2019"<- as.numeric(sub_SDG_14_b$"2019") *20

SDG_14_short <- rbind(sub_SDG_14_1,sub_SDG_14_5,sub_SDG_14_6,sub_SDG_14_7,sub_SDG_14_b)



## Convert to long format

SDG_14_long <- tidyr::gather(SDG_14_short, Year, Value, '2000':'2019', factor_key=TRUE)

SDG_14_clean <- SDG_14_long[order(SDG_14_long$Country, SDG_14_long$SDG, SDG_14_long$Year),]
# View(SDG_14_clean)

write.csv(SDG_14_clean, "data/SDG_14_combined_unFA.csv")


## Fill NAs and calculate sum values based on indicator weights

csv <- 'data/SDG_14_combined_unFA.csv'
df <- read.csv(csv, stringsAsFactors = F)

source("script/function_fill_na2.R")
function_fill_na_wb(df)

FA_SDG_14 <- read.csv("data/SDG_14_combined_unFA.csv_FILLNA_test1.csv")
view(FA_SDG_14)


SDG_14_clean_FA <- subset(FA_SDG_14, select = -c(X, Value, year, value))
names(SDG_14_clean_FA)[5] <- "Filled_Value"

write.csv(SDG_14_clean_FA, "data/SDG_14_combined_FA.csv")

SDG_14_Cal_2000_2019 <- SDG_14_clean_FA
SDG_14_Cal_2000_2019$Cal_Value <- as.numeric(SDG_14_Cal_2000_2019$Filled_Value)*0.2
SDG_14_Cal_2000_2019$Cal_Value[is.na(SDG_14_Cal_2000_2019$Cal_Value)] <- 0 

write.csv(SDG_14_Cal_2000_2019, "data/SDG_14_calfinal.csv")


## Calculation by country over years

value_sum <- aggregate(SDG_14_Cal_2000_2019$Cal_Value,
                       by = list(df$Country, df$Year),
                       FUN = sum)
colnames(value_sum) <- c("Country", "Year", "Value_sum")
view(value_sum)

SDG_14_sum <- value_sum
SDG_14_sum$SDG <- "14"
SDG_14_sum_2000_2019 <- SDG_14_sum[order(SDG_14_sum$Country, SDG_14_sum$SDG, SDG_14_sum$Year),]

write.csv(SDG_14_sum_2000_2019, "data/SDG_14_sum_final.csv")




## Visualization

SDG_14_sum_2000_2019 <- read.csv("data/SDG_14_sum_final.csv")
# Idea 1 - boxplot
SDG_14_sum_2000_2019$Year <- as.factor(SDG_14_sum_2000_2019$Year)
plot1 <- ggplot(data = SDG_14_sum_2000_2019) +
  stat_boxplot(mapping=aes(x=Year, y=Value_sum),
               na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=Year, y=Value_sum),
               na.rm = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Value Change over Years",
       subtitle = "160 Countries/regions between 2000 and 2019",
       x = "Year",
       y = "SDG 14 Value");
plot1

# Distribution of all country yearly SDG scores
SDG_2000 <- subset(SDG_14_sum_2000_2019, Year == "2000")
hist(SDG_2000$Value_sum)

SDG_2004 <- subset(SDG_14_sum_2000_2019, Year == "2004")
hist(SDG_2004$Value_sum)

SDG_2008 <- subset(SDG_14_sum_2000_2019, Year == "2008")
hist(SDG_2008$Value_sum)

SDG_2012 <- subset(SDG_14_sum_2000_2019, Year == "2012")
hist(SDG_2012$Value_sum)

SDG_2016 <- subset(SDG_14_sum_2000_2019, Year == "2016")
hist(SDG_2016$Value_sum)

SDG_2019 <- subset(SDG_14_sum_2000_2019, Year == "2019")
hist(SDG_2019$Value_sum)

data_sdg14_2019 <- SDG_2019[with(SDG_2019, order(Value_sum)),]
data_sdg14_2019[1:16,2]

summary(data_sdg14_2019)
Value_med <- median(data_sdg14_2019$Value_sum)
data_sdg14_2019$Value_cal <- abs(data_sdg14_2019$Value_sum - Value_med)

Value_med2 <- median(data_sdg14_2019$Value_cal)

MAD <- Value_med2 * 1.4826
lower <- Value_med - 2 * MAD
upper <- Value_med + 2 * MAD

data_sdg14_2019$deviate <- (data_sdg14_2019$Value_sum - Value_med) / MAD # error, because MAD = 0

bright14 <- data_sdg14_2019$Country[data_sdg14_2019$Value_sum > upper]
dark14 <- data_sdg14_2019$Country[data_sdg14_2019$Value_sum < lower]

bright14
dark14


# Idea 2 - violin plot
plot2 <- ggplot(data = SDG_14_sum_2000_2019) +
  geom_violin(mapping=aes(x=Year, y=Value_sum),
              na.rm = TRUE) +
  # geom_smooth(method="lm") + 
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Value Change over Years",
       subtitle = "160 Countries between 2000 and 2019",
       x = "Year",
       y = "SDG 14 Value");
plot2

# # Idea 3 - bar plot
# plot3 <-  ggplot(data = SDG_15_Cal_2000_2018,
#                  mapping = aes(x=year, y=Value_sum)) +
#   geom_bar(stat = "Country", color="black", fill="steelblue2");
# plot3
# 
# 
# 
# plot4 <- ggplot(data = SDG_15_Cal_2000_2018,
#                 aes(x=))
#   



# Visualization 2 - Scatter plot by SDG 15 indicator over years
# SDG_14_Cal_2000_2019 <- read.csv("data/SDG_14_calfinal.csv")
SDG_14_Cal_2000_2019$Year <- as.factor(SDG_14_Cal_2000_2019$Year)

# Idea 4 - Scatter point
ind_analysis <- aggregate(SDG_14_Cal_2000_2019[,6], 
                          list(SDG_14_Cal_2000_2019$Year,SDG_14_Cal_2000_2019$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

plot4 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 14 Indicator Average Values over Years",
       subtitle = "160 Countries between 2000 and 2019",
       x = "Year",
       y = "SDG 14 Indicator Value");

# stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot4


# Idea 5 - Box plot
plot5 <- ggplot(data = SDG_14_Cal_2000_2019) +
  # stat_boxplot(mapping=aes(x=year, y=value_cal),
  #              na.rm = TRUE,
  #              geom = "errorbar",
  #              width = 0.2) +
  geom_boxplot(mapping=aes(x=Year, y=Cal_Value),
               na.rm = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 14 Value Change over Years",
       subtitle = "160 Countries between 2000 and 2019",
       x = "Year",
       y = "SDG 14 Value");
plot5


## Bright and dark spot identification

SDG14_2019 <- subset(SDG_14_sum_2000_2019, SDG_14_sum_2000_2019$Year==2019)
  
mean <- mean(SDG14_2019$Value_sum)
sd <- sd(SDG14_2019$Value_sum)

high <- mean + sd
low <- mean - sd

bright_spot <- unique(SDG14_2019$Country[SDG14_2019$Value_sum >= high])
dark_spot <- unique(SDG14_2019$Country[SDG14_2019$Value_sum <= low])

very_bright_spot <- unique(SDG14_2019$Country[SDG14_2019$Value_sum >= (high+sd*0.5)])
very_dark_spot <- unique(SDG14_2019$Country[SDG14_2019$Value_sum <= (low-sd*0.4)])

bright_spot
dark_spot

very_bright_spot
very_dark_spot






# Visualization 3 - Scatter plot by SDG 15 indicator over years by country

# Idea 6 - Scatter point
# ind_analysis <- aggregate(SDG_15_NoCal_2000_2018[,10], 
#                           list(SDG_15_NoCal_2000_2018$Year,SDG_15_NoCal_2000_2018$SDG), 
#                           FUN = mean)
# names(ind_analysis)[1] <- "Year"
# names(ind_analysis)[2] <- "SDG"
# names(ind_analysis)[3] <- "Value"

# plot6 <- ggplot(data = SDG_15_Cal_2000_2018, aes(x=Year, y=Value_sum)) +
#   geom_point(size = 2, aes(color=SDG)) +
#   geom_line(aes(color=CountryCode)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
#   labs(title = "SDG 15 indicator Average Value Change over Years",
#        subtitle = "92 Countries between 2000 and 2018",
#        x = "Year",
#        y = "SDG 15 Indicator Value");
# 
# # stat_smooth(method = lm, se=FALSE, aes(color=SDG));
# plot6











#### Other Visualization ideas ##########
#####################################################################################

# 
# summary <- summarize (group_by(SDG_15_Cal_2000_2018, Country),
#                       maxSDG = max(Value_sum),
#                       minSDG = min(Value_sum),
#                       diffSDG = maxSDG - minSDG)
# which.max(summary$diffSDG)
# 
# 
# 
# # Idea 1
# plot1 <- ggplot(data = SDG_15_Cal_2000_2018, aes(x=Year, y=Value_sum, color=CountryCode))+
#   geom_line()
# plot1
# 
# 
# 
# # Idea 3
# plot3 <- ggplot(data = SDG_15_Cal_2000_2018) +
#   geom_point(mapping = aes(x=Year, y=Value_sum)) +
#   facet_wrap(~CountryCode, nrow = 10)
# plot3
# 
# # Idea 4 - map by change rate (slope)
# m <- plm(Value_sum~Year, data = SDG_15_Cal_2000_2018, 
#          index=c("CountryCode"), model="within")
# summary(m)
# fixef(m)
# # fixed_Country <- lm(Value_sum~Year + CountryCode - 1, data = SDG_15_Cal_2000_2018)
# # summary(fixed_Country)
# 
# 
# a <- signif(coef(m)[1], digits = 4)
# b <- signif(coef(m)[2], digits = 4)
# a
# b
# m
# 
# # Table - from Long to wide
# SDG_15_Cal_wide <- spread(SDG_15_Cal_2000_2018, Year, Value_sum)
# view(SDG_15_Cal_wide)
# 
# write.csv(SDG_15_Cal_wide,"SDG_15_Cal_2000-2018_wide.csv")