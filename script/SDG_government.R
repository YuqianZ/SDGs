### SDGs Vs Government Behavior
### 2021-04-12
### Yuqian Zhang


source( file="script/reference.R" ); 

# Load data

SDG_15_Cal_2000_2018 <- read.csv("data/SDG_15_Cal_2000-2018.csv", check.names = FALSE)
corruption_control <- read.csv("data/Control_Corruption.csv", check.names = FALSE) 
rule_law <- read.csv("data/Rule_Law.csv", check.names = FALSE)
regul_quality <- read.csv("data/Regu_Quality.csv", check.names = FALSE)
gov_effective <- read.csv("data/Gov_Effectiveness.csv", check.names = FALSE)
poli_stability <- read.csv("data/Gov_Stability.csv", check.names = FALSE)
voice_accountability <- read.csv("data/Voice_accountability.csv", check.names = FALSE)


# Rename columns
SDG_15_Cal_2000_2018 <- SDG_15_Cal_2000_2018[-c(1)]
names(corruption_control)[1] <- "Country"
names(rule_law)[1] <- "Country"
names(regul_quality)[1] <- "Country"
names(gov_effective)[1] <- "Country"
names(poli_stability)[1] <- "Country"
names(voice_accountability)[1] <- "Country"


# Format value types and NAs

str(corruption_control)
head(corruption_control)

corruption_control[corruption_control==999] <- NA
rule_law[rule_law==999] <- NA
regul_quality[regul_quality==999] <- NA
gov_effective[gov_effective==999] <- NA
poli_stability[poli_stability==999] <- NA
voice_accountability[voice_accountability==999] <- NA


# Convert wide to long format

corruption_control_long <- gather(corruption_control, Year, Score, "1996":"2019", factor_key = TRUE)
rule_law_long <- gather(rule_law, Year, Score, "1996":"2019", factor_key = TRUE)
regul_quality_long <- gather(regul_quality, Year, Score, "1996":"2019", factor_key = TRUE)
gov_effective_long <- gather(gov_effective, Year, Score, "1996":"2019", factor_key = TRUE)
poli_stability_long <- gather(poli_stability, Year, Score, "1996":"2019", factor_key = TRUE)
voice_accountability_long <- gather(voice_accountability, Year, Score, "1996":"2019", factor_key = TRUE)


# Regress and Plot SDG vs. Government


# 1. SDG vs. Control of Corruption

SDG_corr <- merge(SDG_15_Cal_2000_2018, corruption_control_long, by=c("Country", "Year"))
names(SDG_corr)[7]<-"corru"
corruption.SDG15.mod1<-lm(Value_sum ~ corru, data=SDG_corr)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(corruption.SDG15.mod1)

#Interpret the model
anova(corruption.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(corruption.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot1 <- ggplot(SDG_corr, aes(x = corru, y = Value_sum)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Control of Corruption")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot1


# 2. SDG vs. Rule of Law

SDG_rule <- merge(SDG_15_Cal_2000_2018, rule_law_long, by=c("Country", "Year"))
names(SDG_rule)[7]<-"rule"
rule.SDG15.mod1<-lm(Value_sum ~ rule, data=SDG_rule)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(rule.SDG15.mod1)

#Interpret the model
anova(rule.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(rule.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot2 <- ggplot(SDG_rule, aes(x = rule, y = Value_sum)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Rule of Law")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot2


# 3. SDG vs. Regulatory quality

SDG_regq <- merge(SDG_15_Cal_2000_2018, regul_quality_long, by=c("Country", "Year"))
names(SDG_regq)[7]<-"regq"
regq.SDG15.mod1<-lm(Value_sum ~ regq, data=SDG_regq)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(regq.SDG15.mod1)

#Interpret the model
anova(regq.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(regq.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot3 <- ggplot(SDG_regq, aes(x = regq, y = Value_sum)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Regulatory quality")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot3


# 4. SDG vs. Government effectiveness

SDG_goveff <- merge(SDG_15_Cal_2000_2018, gov_effective_long, by=c("Country", "Year"))
names(SDG_goveff)[7]<-"goveff"
goveff.SDG15.mod1<-lm(Value_sum ~ goveff, data=SDG_goveff)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(goveff.SDG15.mod1)

#Interpret the model
anova(goveff.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(goveff.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot4 <- ggplot(SDG_goveff, aes(x = goveff, y = Value_sum)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Government effectiveness")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot4


# 5. SDG vs. Political stability

SDG_pols <- merge(SDG_15_Cal_2000_2018, poli_stability_long, by=c("Country", "Year"))
names(SDG_pols)[7]<-"pols"
pols.SDG15.mod1<-lm(Value_sum ~ pols, data=SDG_pols)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(pols.SDG15.mod1)

#Interpret the model
anova(pols.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(pols.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot5 <- ggplot(SDG_pols, aes(x = pols, y = Value_sum)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Political Stability")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot5


# 6. SDG vs. Voice and accountability

SDG_acc <- merge(SDG_15_Cal_2000_2018, voice_accountability_long, by=c("Country", "Year"))
names(SDG_acc)[7]<-"acc"
acc.SDG15.mod1<-lm(Value_sum ~ acc, data=SDG_acc)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(acc.SDG15.mod1)

#Interpret the model
anova(acc.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(acc.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot6 <- ggplot(SDG_acc, aes(x = acc, y = Value_sum)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Voice and Accountability")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot6


library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             nrow=3)


## Multiple linear regression of SDG vs. Government behavior
# https://www.statmethods.net/stats/regression.html

df1 <- merge(SDG_corr, SDG_rule, by=c("Country", "Year", "CountryCode", "Code", "Value_sum", "SDG"))
df2 <- merge(df1, SDG_regq, by=c("Country", "Year", "CountryCode", "Code", "Value_sum", "SDG"))
df3 <- merge(df2, SDG_goveff, by=c("Country", "Year", "CountryCode", "Code", "Value_sum", "SDG"))
df4 <- merge(df3, SDG_pols, by=c("Country", "Year", "CountryCode", "Code", "Value_sum", "SDG"))
full_data <- merge(df4, SDG_acc, by=c("Country", "Year", "CountryCode", "Code", "Value_sum", "SDG"))

colnames(full_data)

fit <- lm(Value_sum ~ corru + rule + regq + goveff + pols + acc, data=full_data)
summary(fit)

# diagnostic plots
par(mfrow=c(2,2))
plot(fit)

# Test for multicollinearity

# Pair-wise correlation
X <-full_data[,7:12]
library(GGally)
ggpairs(X)

# library(ppcor)
# pcor(X, method = "pearson")
# 
# vif(fit)
# 
# library(mctest)
# mctest(fit)
# 
omcdiag(fit)
# 
# head(X)
# str(X)
# str(full_data$Value_sum)
# 
# 
imcdiag(fit)


# Updated regression 1
fit1 <- lm(Value_sum ~  corru + rule + pols + acc, data=full_data)
summary(fit1)

# diagnostic plots
par(mfrow=c(2,2))
plot(fit1)


imcdiag(fit1)


fit2 <- lm(Value_sum ~  pols + acc, data=full_data)
summary(fit2)

# diagnostic plots
par(mfrow=c(2,2))
plot(fit2)

imcdiag(fit2)


# Plot
sub_data <- full_data[c(1,2,3,5,6,11,12)]
sub_data_long <- gather(sub_data, x_var, metric, pols:acc, factor_key = TRUE)

equation1=function(x){coef(fit2)[2]*x+coef(fit2)[1]}
equation2=function(x){coef(fit2)[2]*x+coef(fit2)[1]+coef(fit2)[3]}

sub_data_long$x_var <- factor(sub_data_long$x_var, levels = c("pols","acc"), labels = c("Political Stability", "Voice & Accountability"))

plot7 <- ggplot(sub_data_long, aes(x = metric, y = Value_sum, color=x_var)) +
  geom_point(size = 1, alpha=.5) +
  stat_function(fun=equation1,geom="line", size=1.5, color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",size=1.5, color=scales::hue_pal()(2)[2])+
  # geom_ribbon(alpha=0.2, linetype=0)+
  # geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  labs(color="Government Behavior") +
  xlab("Metric of Government Behavior")+
  ylab("SDG 15 Value Estimate") +
  theme_bw()
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot7



ggPredict(fit1, se=TRUE, interactive=TRUE)

