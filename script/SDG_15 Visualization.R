
### SDG 15 Visualization
### 2020-10-13
### Yuqian Zhang


path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
getwd()

library(dplyr)
library(tidyr)
library(ggplot2)


# Visualization 1 - Box plot by calculated SDG over years

SDG_15_Cal_2000_2018 <- read.csv("SDG_15_Cal_2000-2018.csv")
str(SDG_15_Cal_2000_2018)
unique(SDG_15_Cal_2000_2018$Year)

# Idea 1 - boxplot
SDG_15_Cal_2000_2018$year <- as.factor(SDG_15_Cal_2000_2018$Year)
plot1 <- ggplot(data = SDG_15_Cal_2000_2018) +
  stat_boxplot(mapping=aes(x=year, y=Value_sum),
               na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=year, y=Value_sum),
               na.rm = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Value Change over Years",
       subtitle = "92 Countries between 2000 and 2018",
       x = "Year",
       y = "SDG 15 Value");
plot1

# Idea 2 - violin plot
plot2 <- ggplot(data = SDG_15_Cal_2000_2018) +
  geom_violin(mapping=aes(x=year, y=Value_sum),
               na.rm = TRUE) +
  # geom_smooth(method="lm") + 
  theme_bw() +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Value Change over Years",
       subtitle = "92 Countries between 2000 and 2018",
       x = "Year",
       y = "SDG 15 Value");
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

SDG_15_NoCal_2000_2018 <- read.csv("SDG_15_NoCal_2000-2018.csv")
# str(SDG_15_NoCal_2000_2018)
SDG_15_NoCal_2000_2018$year <- as.factor(SDG_15_NoCal_2000_2018$Year)

# Idea 4 - Scatter point
ind_analysis <- aggregate(SDG_15_NoCal_2000_2018[,10], 
                          list(SDG_15_NoCal_2000_2018$Year,SDG_15_NoCal_2000_2018$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value"

plot4 <- ggplot(data = ind_analysis, aes(x=Year, y=Value)) +
  geom_point(size = 2, aes(color=SDG)) +
  geom_line(aes(color=SDG)) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  labs(title = "SDG 15 indicator Average Value Change over Years",
       subtitle = "92 Countries between 2000 and 2018",
       x = "Year",
       y = "SDG 15 Indicator Value");

  # stat_smooth(method = lm, se=FALSE, aes(color=SDG));
plot4


# Idea 5 - Box plot
plot5 <- ggplot(data = SDG_15_NoCal_2000_2018) +
  # stat_boxplot(mapping=aes(x=year, y=value_cal),
  #              na.rm = TRUE,
  #              geom = "errorbar",
  #              width = 0.2) +
  geom_boxplot(mapping=aes(x=year, y=value_cal),
               na.rm = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Value Change over Years",
       subtitle = "92 Countries between 2000 and 2018",
       x = "Year",
       y = "SDG 15 Value");
plot5


data(iris)
View(iris)
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(size = 3, aes(color=Species, shape=Species))





# Idea 1
plot1 <- ggplot(data = SDG_15_Cal_2000_2018, aes(x=Year, y=Value_sum, color=CountryCode))+
  geom_line()
plot1



# Idea 3
plot3 <- ggplot(data = SDG_15_Cal_2000_2018) +
  geom_point(mapping = aes(x=Year, y=Value_sum)) +
  facet_wrap(~CountryCode, nrow = 10)
plot3

# Idea 4 - map by change rate (slope)
m <- plm(Value_sum~Year, data = SDG_15_Cal_2000_2018, 
         index=c("CountryCode"), model="within")
summary(m)
fixef(m)
# fixed_Country <- lm(Value_sum~Year + CountryCode - 1, data = SDG_15_Cal_2000_2018)
# summary(fixed_Country)


a <- signif(coef(m)[1], digits = 4)
b <- signif(coef(m)[2], digits = 4)
a
b
m

# Table - from Long to wide
SDG_15_Cal_wide <- spread(SDG_15_Cal_2000_2018, Year, Value_sum)
view(SDG_15_Cal_wide)

write.csv(SDG_15_Cal_wide,"SDG_15_Cal_2000-2018_wide.csv")