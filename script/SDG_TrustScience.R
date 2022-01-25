### SDGs Vs Trust in Science
### 2021-04-12
### Yuqian Zhang


source( file="script/reference.R" ); 


# Load data

SDG_15_Cal_2000_2018 <- read.csv("data/SDG_15_Cal_2000-2018.csv", check.names = FALSE)
Trust_Science <- read.csv("data/Country_trust_gdp.csv", check.names = FALSE) 


# Rename columns and format cells/NAs
SDG_15_Cal_2000_2018 <- SDG_15_Cal_2000_2018[-c(1)]
names(Trust_Science)[1] <- "Country"
names(Trust_Science)[2] <- "GDP_PPP_USD"
names(Trust_Science)[3] <- "Trust_score"

head(Trust_Science)
str(Trust_Science)

Trust_Science$Trust_score_perc <- 100*(Trust_Science$Trust_score)

Trust_Science$GDP_PPP_USD <- as.numeric(Trust_Science$GDP_PPP_USD)


# Subset SDG 2018 only

SDG_15_2018 <- subset(SDG_15_Cal_2000_2018, Year == 2018)



# Regress and Plot SDG vs. Trust Science

SDG_TrustScience <- merge(SDG_15_2018, Trust_Science, by=c("Country"))
TrustScience.SDG15.mod1<-lm(Value_sum ~ Trust_score, data=SDG_TrustScience)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(TrustScience.SDG15.mod1)

#Interpret the model
anova(TrustScience.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(TrustScience.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)


#Produce a figure that tells a story
plot_Trust1 <- ggplot(SDG_TrustScience, aes(x = Trust_score, y = Value_sum)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Extent of Trust in Science")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_Trust1


# Log form of SDG
TrustScience.SDG15.mod2<-lm(log(Value_sum) ~ Trust_score, data=SDG_TrustScience)
summary(TrustScience.SDG15.mod2)


#Produce a figure that tells a story
plot_Trust2 <- ggplot(SDG_TrustScience, aes(x = Trust_score, y = log(Value_sum))) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Extent of Trust in Science")+
  ylab("Log of SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_Trust2


# Multi regression

TrustScience.SDG15.mod3<-lm(Value_sum ~ Trust_score + GDP_PPP_USD, data=SDG_TrustScience)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(TrustScience.SDG15.mod3)

#Interpret the model
anova(TrustScience.SDG15.mod3)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(TrustScience.SDG15.mod3)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot_GDP <- ggplot(SDG_TrustScience, aes(x = GDP_PPP_USD, y = Value_sum)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("GDP per Person in USD")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_GDP


