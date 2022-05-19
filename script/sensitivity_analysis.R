
### Sensitivity Analysis
### 2022-05-17
### Yuqian Zhang

# 
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir

setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG")
getwd()


source( file="script/reference.R" ); 

### Conduct sensitivity analysis among accepting different levels of missing values

### SDG 15 ----

# With 50% missing values # Country number: 246
SDG_15_50 <- read.csv("data/SDSN/SDG_15_score_overall_50.csv")

ind_analysis <- aggregate(SDG_15_50$Value, 
                           list(SDG_15_50$Year,SDG_15_50$SDG), 
                           FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_50"

Compare_dat <- ind_analysis

# With 40% missing values # Country number: 243
SDG_15_60 <- read.csv("data/SDSN/SDG_15_score_overall_60.csv")

ind_analysis <- aggregate(SDG_15_60$Value, 
                          list(SDG_15_60$Year,SDG_15_60$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_60"

Compare_dat$Value_60 <- ind_analysis$Value_60

# # With 35% missing values # Country number: 241
# SDG_15_65 <- read.csv("data/SDSN/SDG_15_score_overall_65.csv")
# 
# ind_analysis <- aggregate(SDG_15_65$Value, 
#                           list(SDG_15_65$Year,SDG_15_65$SDG), 
#                           FUN = mean)
# names(ind_analysis)[1] <- "Year"
# names(ind_analysis)[2] <- "SDG"
# names(ind_analysis)[3] <- "Value_65"
# 
# Compare_dat$Value_65 <- ind_analysis$Value_65

# With 30% missing values # Country number: 232
SDG_15_70 <- read.csv("data/SDSN/SDG_15_score_overall_70.csv")

ind_analysis <- aggregate(SDG_15_70$Value, 
                          list(SDG_15_70$Year,SDG_15_70$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_70"

Compare_dat$Value_70 <- ind_analysis$Value_70

# With 20% missing values # Country number: 198
SDG_15_80 <- read.csv("data/SDSN/SDG_15_score_overall_80.csv")

ind_analysis <- aggregate(SDG_15_80$Value, 
                          list(SDG_15_80$Year,SDG_15_80$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_80"

Compare_dat$Value_80 <- ind_analysis$Value_80

# With 10% missing values # Country number: 153
SDG_15_90 <- read.csv("data/SDSN/SDG_15_score_overall_90.csv")

ind_analysis <- aggregate(SDG_15_90$Value, 
                          list(SDG_15_90$Year,SDG_15_90$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_90"

Compare_dat$Value_90 <- ind_analysis$Value_90

## One-way ANOVA across all groups
Compare_dat_long <- melt(Compare_dat, id.vars = c('Year', 'SDG'), variable.name = 'Acceptance')
anova <- aov(value~Acceptance, data = Compare_dat_long)
summary(anova)
## Results: F=1.386, P=0.244 <- Not statistically significant
## Conclusion: no significant difference among the groups!

# # Post-hoc testing: TukeyHSD - since the ANOVA result above is not significant, here is optional
# TukeyHSD(anova)

## So, it is okay to use 50% missing values data for final analysis <---!!!


# ## T-test between groups
# 
# # 50 vs 60
# res1 <- t.test(Compare_dat$Value_50, Compare_dat$Value_60)
# res1 # Not significant
# 
# # 60 vs 70
# res2 <- t.test(Compare_dat$Value_60, Compare_dat$Value_70)
# res2 # Not significant
# 
# # 70 vs 80
# res3 <- t.test(Compare_dat$Value_70, Compare_dat$Value_80)
# res3 # Not significant
# 
# # 80 vs 90
# res4 <- t.test(Compare_dat$Value_80, Compare_dat$Value_90)
# res4 # Not significant
# 
# # 50 vs 90
# res5 <- t.test(Compare_dat$Value_50, Compare_dat$Value_90)
# res5 # Significant*
# 
# # 60 vs 90
# res6 <- t.test(Compare_dat$Value_60, Compare_dat$Value_90)
# res6 # Significant*
# 
# # 70 vs 90
# res7 <- t.test(Compare_dat$Value_70, Compare_dat$Value_90)
# res7 # Not significant
# 
# # # 65 vs 90
# # res8 <- t.test(Compare_dat$Value_65, Compare_dat$Value_90)
# # res8 # Significant*
# 
# # 50 vs 70
# res9 <- t.test(Compare_dat$Value_50, Compare_dat$Value_70)
# res9 # Not significant
# 
# # 50 vs 80
# res10 <- t.test(Compare_dat$Value_50, Compare_dat$Value_80)
# res10 # Not significant



### SDG 14 ----

# Although there are 44 landlocked countries, the number is still way under (249 - max.countries.included)

# With 50% missing values # Country number included: 147
SDG_14_50 <- read.csv("data/SDSN2/SDG_14_score_overall_50.csv")

ind_analysis <- aggregate(SDG_14_50$Value, 
                          list(SDG_14_50$Year,SDG_14_50$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_50"

Compare_dat2 <- ind_analysis

# With 40% missing values # Country number: 113
SDG_14_60 <- read.csv("data/SDSN2/SDG_14_score_overall_60.csv")

ind_analysis <- aggregate(SDG_14_60$Value, 
                          list(SDG_14_60$Year,SDG_14_60$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_60"

Compare_dat2$Value_60 <- ind_analysis$Value_60

# With 37% missing values # Country number: 100
SDG_14_63 <- read.csv("data/SDSN2/SDG_14_score_overall_63.csv")

ind_analysis <- aggregate(SDG_14_63$Value, 
                          list(SDG_14_63$Year,SDG_14_63$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_63"

Compare_dat2$Value_63 <- ind_analysis$Value_63

# With 30% missing values # Country number: 82
SDG_14_70 <- read.csv("data/SDSN2/SDG_14_score_overall_70.csv")

ind_analysis <- aggregate(SDG_14_70$Value, 
                          list(SDG_14_70$Year,SDG_14_70$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_70"

Compare_dat2$Value_70 <- ind_analysis$Value_70

# With 20% missing values # Country number: 47
SDG_14_80 <- read.csv("data/SDSN2/SDG_14_score_overall_80.csv")

ind_analysis <- aggregate(SDG_14_80$Value, 
                          list(SDG_14_80$Year,SDG_14_80$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_80"

Compare_dat2$Value_80 <- ind_analysis$Value_80

# With 10% missing values # Country number: 12
SDG_14_90 <- read.csv("data/SDSN2/SDG_14_score_overall_90.csv")

ind_analysis <- aggregate(SDG_14_90$Value, 
                          list(SDG_14_90$Year,SDG_14_90$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_90"

Compare_dat2$Value_90 <- ind_analysis$Value_90

## One-way ANOVA across all groups
Compare_dat2_long <- melt(Compare_dat2, id.vars = c('Year', 'SDG'), variable.name = 'Acceptance')
anova2 <- aov(value~Acceptance, data = Compare_dat2_long)
summary(anova2)
## Results: F=36.08, P<0.01***
## Conclusion: There is a significant difference among the groups!

# Post-hoc testing: TukeyHSD - for detecting differences between two groups
TukeyHSD(anova2)
# TukeyHSD results:
#   Groups(1vs2)       diff      p adj
# Value_60-Value_50 2.0291769  0.0209283
# Value_63-Value_50 3.1709418  0.0000278
# Value_70-Value_50 4.3436968  0.0000000
# Value_80-Value_50 5.8568891  0.0000000
# Value_90-Value_50 6.2437307  0.0000000
# Value_63-Value_60 1.1417649  0.4674706
# Value_70-Value_60 2.3145199  0.0050185
# Value_80-Value_60 3.8277122  0.0000003
# Value_90-Value_60 4.2145537  0.0000000
# Value_70-Value_63 1.1727550  0.4365692
# Value_80-Value_63 2.6859473  0.0006129
# Value_90-Value_63 3.0727889  0.0000535
# Value_80-Value_70 1.5131923  0.1680205
# Value_90-Value_70 1.9000338  0.0375917
# Value_90-Value_80 0.3868415  0.9900564

# It looks there are significant differences between groups, to maximize the number of countries included,
# And take the minimum differences between groups, 
# the difference between 60 and 63, is NOT significant;
# the difference between 63 and 70, is NOT significant;
# the difference between 60 and 70, is significant***
## So, we choose the transitioning value of 63, which allows 37% missing values data for final analysis <---!!!


# ## T-test between groups
# # 50 vs 60
# res1 <- t.test(Compare_dat2$Value_50, Compare_dat2$Value_60)
# res1 # Significant***
# 
# # 60 vs 70
# res2 <- t.test(Compare_dat2$Value_60, Compare_dat2$Value_70)
# res2 # Significant***
# 
# # 70 vs 80
# res3 <- t.test(Compare_dat2$Value_70, Compare_dat2$Value_80)
# res3 # Significant**
# 
# # 80 vs 90
# res4 <- t.test(Compare_dat2$Value_80, Compare_dat2$Value_90)
# res4 # Not significant
# 
# # 50 vs 90 
# res5 <- t.test(Compare_dat2$Value_50, Compare_dat2$Value_90)
# res5 # Significant***
# 
# # 60 vs 90
# res6 <- t.test(Compare_dat2$Value_60, Compare_dat2$Value_90)
# res6 # Significant***
# 
# # 70 vs 90
# res7 <- t.test(Compare_dat2$Value_70, Compare_dat2$Value_90)
# res7 # Significant***
