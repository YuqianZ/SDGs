
### SDG 15 Bright and Dark spot identification
### 2021-01-30
### Yuqian Zhang

source( file="script/reference.R" ); 

SDG15_2018 <- read.csv("data/SDG_15_Cal_2018only.csv")

mean <- mean(SDG15_2018$Value_sum)
sd <- sd(SDG15_2018$Value_sum)

high <- mean + sd
low <- mean - sd

bright_spot <- unique(SDG15_2018$Country[SDG15_2018$Value_sum >= high])
dark_spot <- unique(SDG15_2018$Country[SDG15_2018$Value_sum <= low])

very_bright_spot <- unique(SDG15_2018$Country[SDG15_2018$Value_sum >= (high+sd*0.5)])
very_dark_spot <- unique(SDG15_2018$Country[SDG15_2018$Value_sum <= (low-sd*0.5)])

bright_spot
dark_spot

very_bright_spot
very_dark_spot
