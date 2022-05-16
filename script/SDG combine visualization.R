
### All about visualization
### 2022-05-03
### Yuqian Zhang

# 
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir

setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG")
getwd()


source( file="script/reference.R" ); 

SDG_14_score_overall <- read.csv("data/SDSN2/SDG_14_score_overall.csv")
SDG_15_score_overall <- read.csv("data/SDSN/SDG_15_score_overall.csv")

ind_analysis14 <- aggregate(SDG_14_score_overall$Value, 
                           list(SDG_14_score_overall$Year,SDG_14_score_overall$SDG), 
                           FUN = mean)
names(ind_analysis14)[1] <- "Year"
names(ind_analysis14)[2] <- "SDG"
names(ind_analysis14)[3] <- "Value"

sd <- aggregate(SDG_14_score_overall$Value, 
                list(SDG_14_score_overall$Year,SDG_14_score_overall$SDG),
                FUN = sd)

ind_analysis14$sd <- sd$x
ind_analysis14$lower <- ind_analysis14$Value - 0.5*ind_analysis14$sd
ind_analysis14$upper <- ind_analysis14$Value + 0.5*ind_analysis14$sd




ind_analysis15 <- aggregate(SDG_15_score_overall$Value, 
                           list(SDG_15_score_overall$Year,SDG_15_score_overall$SDG), 
                           FUN = mean)
names(ind_analysis15)[1] <- "Year"
names(ind_analysis15)[2] <- "SDG"
names(ind_analysis15)[3] <- "Value"

sd2 <- aggregate(SDG_15_score_overall$Value, 
                list(SDG_15_score_overall$Year,SDG_15_score_overall$SDG),
                FUN = sd)

ind_analysis15$sd <- sd2$x
ind_analysis15$lower <- ind_analysis15$Value - 0.5*ind_analysis15$sd
ind_analysis15$upper <- ind_analysis15$Value + 0.5*ind_analysis15$sd

ind_analysis <- rbind(ind_analysis14, ind_analysis15)

# ind_analysis$SDG[ind_analysis$SDG == 14] <- "SDG.14"
# ind_analysis$SDG[ind_analysis$SDG == 15] <- "SDG.15"


plot <- ggplot(data = ind_analysis, aes(x = Year, y = Value)) +
  geom_ribbon(data=ind_analysis14, aes(ymin = lower,ymax = upper), alpha = 0.2, fill = "royalblue") +
  geom_ribbon(data=ind_analysis15, aes(ymin = lower,ymax = upper), alpha = 0.2, fill = "seagreen") +
  geom_line(data = ind_analysis, aes(colour = factor(SDG)), size=1) +
  # geom_smooth(aes(colour = factor(SDG)), method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  # scale_fill_manual(values='seagreen', name="fill") + 
  geom_point(data = ind_analysis, aes(colour = factor(SDG)), size=2) +
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
  scale_color_manual(values = c("royalblue", "seagreen")) +
  labs(title = "SDGs 14 and 15 Score Between 2000 and 2020",
       # subtitle = "249 Countries between 2000 and 2020",
       x = "Year",
       y = "SDG Score",
       color = "SDG");
plot
