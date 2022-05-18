
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

### SDG 15

# With 50% missing values
SDG_15_50 <- read.csv("data/SDSN/SDG_15_score_overall_50.csv")

ind_analysis <- aggregate(SDG_15_50$Value, 
                           list(SDG_15_50$Year,SDG_15_50$SDG), 
                           FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_50"

Compare_dat <- ind_analysis

# With 40% missing values
SDG_15_60 <- read.csv("data/SDSN/SDG_15_score_overall_60.csv")

ind_analysis <- aggregate(SDG_15_60$Value, 
                          list(SDG_15_60$Year,SDG_15_60$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_60"

Compare_dat$Value_60 <- ind_analysis$Value_60

# With 30% missing values
SDG_15_70 <- read.csv("data/SDSN/SDG_15_score_overall_70.csv")

ind_analysis <- aggregate(SDG_15_70$Value, 
                          list(SDG_15_70$Year,SDG_15_70$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_70"

Compare_dat$Value_70 <- ind_analysis$Value_70

# With 20% missing values
SDG_15_80 <- read.csv("data/SDSN/SDG_15_score_overall_80.csv")

ind_analysis <- aggregate(SDG_15_80$Value, 
                          list(SDG_15_80$Year,SDG_15_80$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_80"

Compare_dat$Value_80 <- ind_analysis$Value_80

# With 10% missing values
SDG_15_90 <- read.csv("data/SDSN/SDG_15_score_overall_90.csv")

ind_analysis <- aggregate(SDG_15_90$Value, 
                          list(SDG_15_90$Year,SDG_15_90$SDG), 
                          FUN = mean)
names(ind_analysis)[1] <- "Year"
names(ind_analysis)[2] <- "SDG"
names(ind_analysis)[3] <- "Value_90"

Compare_dat$Value_90 <- ind_analysis$Value_90


# ## Paired t-test between groups
# 
# # 50 vs 60
# res1 <- t.test(Compare_dat$Value_50, Compare_dat$Value_60)
# res1
# 
# # 60 vs 70
# res2 <- t.test(Compare_dat$Value_60, Compare_dat$Value_70)
# res2
# 
# # 70 vs 80
# res3 <- t.test(Compare_dat$Value_70, Compare_dat$Value_80)
# res3
# 
# # 80 vs 90
# res4 <- t.test(Compare_dat$Value_80, Compare_dat$Value_90)
# res4

## One-way ANOVA across all groups
Compare_dat_long <- melt(Compare_dat, id.vars = c('Year', 'SDG'), variable.name = 'Acceptance')
anova <- aov(value~Acceptance, data = Compare_dat_long)
summary(anova)
## Results: F=1.386, P=0.244
## Conclusion: no significant difference among the groups!


### SDG 14

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

# ## Paired t-test between groups
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
