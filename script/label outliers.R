
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# setwd(dir)
# getwd()


# source( file="script/reference.R" ); 

rm(list=ls());  

library(package=ggplot2);              # include all GGPlot2 functions
library(package=dplyr);                # include all dplyr functions
# library(package=tidyr);               # include all tidyr functions
# library(package=gridExtra);            # include all gridExtra functions
# library(plyr)
library(tibble)

# Visualization 1 - Box plot by calculated SDG over years

SDG_15_Cal_2000_2018 <- read.csv("data/SDG_15_Cal_2000-2018.csv")
str(SDG_15_Cal_2000_2018)
unique(SDG_15_Cal_2000_2018$Year)

# Idea 1 - boxplot

SDG_15_Cal_2000_2018$year <- as.factor(SDG_15_Cal_2000_2018$Year)

# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
# }
# 
# SDG_15_Cal_2000_2018 %>%
#   group_by(year) %>%
#   mutate(outlier = ifelse(is_outlier(Value_sum), Value_sum, as.numeric(NA))) %>%

plot1 <- ggplot(data = SDG_15_Cal_2000_2018) +
  stat_boxplot(mapping=aes(x=year, y=Value_sum),
               na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=year, y=Value_sum),
               na.rm = TRUE,
               outlier.color = "red") +
  # geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw() +
  theme(axis.text.x = element_text(color="black", size=9, angle=30, vjust=0.8)) +
  # scale_x_discrete(labels = year) +
  labs(title = "SDG 15 Value Change over Years",
       subtitle = "92 Countries between 2000 and 2018",
       x = "Year",
       y = "SDG 15 Value");
plot1
