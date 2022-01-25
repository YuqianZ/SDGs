### SDG 15 Vs Biofuel
### 2021-02-25
### Yuqian Zhang

# 
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# setwd(dir)
# getwd()


source( file="script/reference.R" ); 

SDG_15_Cal_2000_2018 <- read.csv("data/SDG_15_Cal_2000-2018.csv")
biofuel <- read.csv("data/biofuels-production-by-region.csv")

names(biofuel)[1] <- "Country"
names(biofuel)[4] <- "Biofuel_production" 

CombineData <- merge(SDG_15_Cal_2000_2018, biofuel, by=c("Country", "Year"))
# Data conversion to log form (add 0.01 to zero data cells)
CombineData$Biofuel_production_log <- log((CombineData$Biofuel_production+0.01))

#create a model for the relationship between biofuel production and SDG 15 value
#lm(response variable ~ explanatory variable, data)
biofuel.SDG15.mod1<-lm(Value_sum ~ Biofuel_production, data=CombineData)
biofuel.SDG15.mod2<-lm(Value_sum ~ Biofuel_production_log, data=CombineData)

#Check assumptions of normality
par(mfrow=c(2,2))
plot(biofuel.SDG15.mod1)

#Interpret the model
anova(biofuel.SDG15.mod1)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(biofuel.SDG15.mod1)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot1 <- ggplot(CombineData, aes(x = Biofuel_production, y = Value_sum)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Log of Biofuel Production - PJ Total")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot1


#Check assumptions of normality
par(mfrow=c(2,2))
plot(biofuel.SDG15.mod2)

#Interpret the model
anova(biofuel.SDG15.mod2)     #this produces the ANOVA table for your linear model (lists the sources of variation)
summary(biofuel.SDG15.mod2)   #the summary function gives your coefficient table (lists of your estimates (or betas), error, and associated t and p values)

#Produce a figure that tells a story
plot2 <- ggplot(CombineData, aes(x = Biofuel_production, y = Value_sum)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', alpha = .15)+ #alpha here is increasing transparency of CIs
  xlab("Log of Biofuel Production - PJ Total")+
  ylab("SDG 15 Value Estimate") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot2

unique(CombineData$Country)
