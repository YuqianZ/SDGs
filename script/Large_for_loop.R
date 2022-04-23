
### FOR LOOP only
### 2022-04-18
### Yuqian Zhang


setwd("G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG")
getwd()


source( file="script/reference.R" ); 

### Read in data


N_SDG_15_comb_00_20 <- read.csv("data/SDSN/empty_full_country_sdg_year.csv")
N_SDG_15_comb_00_20 <- N_SDG_15_comb_00_20[, 2:6]


full_country_sdg_year <- read.csv("data/SDSN/empty_full_country_sdg_year.csv")
full_country_sdg_year <- full_country_sdg_year[, 2:6]

origin <- full_country_sdg_year[,1:5]

N_SDG_15_comb_00_20$Key <- paste(N_SDG_15_comb_00_20$Entity, N_SDG_15_comb_00_20$Code,
                                 N_SDG_15_comb_00_20$SDG, N_SDG_15_comb_00_20$Year, 
                                 sep="_")


full_country_sdg_year$Key <- paste(full_country_sdg_year$Entity, full_country_sdg_year$Code,
                                   full_country_sdg_year$SDG, full_country_sdg_year$Year, 
                                   sep="_")


for (i in 1:nrow(full_country_sdg_year)){
  for (j in 1:nrow(N_SDG_15_comb_00_20)){
    # print(full_country_sdg_year$Key[i]);
    # print(N_SDG_15_comb_00_20$Key[j]);
    if (full_country_sdg_year$Key[i] == N_SDG_15_comb_00_20$Key[j])
    {
      # print("detected");
      full_country_sdg_year$Value[i] <- N_SDG_15_comb_00_20$Value[j];
    }
  }
}

# pb <- txtProgressBar(min = 0, max = 100, style = 3)
# for(i in 1:100) {
#   Sys.sleep(0.1)
#   setTxtProgressBar(pb, i)
# }
# close(pb)