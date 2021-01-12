
### SDG 15 Visualization
### 2020-10-13
### Yuqian Zhang


path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
getwd()




# Visualize the calculated SDG
# Idea 1
plot1 <- ggplot(data = SDG_15_Cal_2000_2018, aes(x=Year, y=Value_sum, color=CountryCode))+
  geom_line()
plot1

# Idea 2
plot2 <- ggplot(data = SDG_15_Cal_2000_2018, aes(x=Year, y=Value_sum, color=CountryCode))+
  geom_point()
plot2

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