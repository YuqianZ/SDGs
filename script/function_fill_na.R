

### fill na function for wb data
library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)
library(ggplot2)

function_fill_na_wb <- function(df) {
  dfs <- data.frame()
  n <- length(unique(df$countrycode)) 
  
  for (i in seq(1:n)) {
    print(i) 
    ### loop each country code
    uni_ctycode <- unique(df$countrycode)
    print(uni_ctycode[i])
    
    df1 <- df %>%
      filter(countrycode == uni_ctycode[i]) %>% ## loop each nation
      # gather(key = 'year', value =  'value', -c(1:6)) %>%
      mutate(year =  Year) %>%
      # dplyr::mutate(year = year(as.Date(Year, format="%Y"))) #%>%
      mutate(value = as.numeric(value)) %>%
      arrange(year) # %>% ## order by date
    
    # m <- length(unique(df$SDG)) * length(unique(df$Year))
    # for (j in seq(1: m)) {
    #   print(j)
    #   print(df$SDG[j])
    # }

    ### if too many NA, then keep as it is - may drop this data; if not too much, then interpolation
    if (sum(is.na(df1$value)) > (length(df1$countrycode)-2)) {
      df1 <- df1 %>%
        mutate(value_ks = value)} ## 
    else {
      df1 <- df1 %>%
        mutate(value_ks = na_interpolation(x = value, option = 'stine'))}

    dfs <- rbind(dfs, df1)
    
    dfs1 <- dfs[order(dfs$Country, dfs$SDG),]
  }
  
  view(dfs)
  view(dfs1)
  
  
  ### plot and examine the interpolation
  # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
  plot <- dfs %>%
    ### randomly choose 20 countries and plot
    # filter(CountryCode %in% df$CountryCode[sample(1:3, size = 3, replace = T)]) %>%

    ggplot()+
    geom_line(aes(x = year, y = value),    color = 'blue', size = 6) +
    geom_line(aes(x = year, y = value_ks), color = 'red', size = 2) +
    facet_wrap(~countrycode, scales = 'free_y') +
    # xlim(1990, 2005) +
    # ylim(0, 7*10^5)+
    theme_bw()
  #

  
  ### update data and save to local
  # dfs_update <- dfs %>%
  #   select(-value) %>%
  #   spread(key = year, value = value_ks) %>%
  #   # left_join(x=ctr_eora, y = ., by=c('country_eora', 'CountryCode')) ### make sure the same order 
  # fname <- paste0(dir.output, iname, '_FILLNA.xlsx'); print(fname)
  # write.csv(x = dfs_update, file = fname, row.names = F)
  # write_xlsx(x = dfs_update, path = fname)
  
  ### here I just save the data, you can modify the above code and then save to the format you like
  fname <- paste0(#dir.output, 
                  csv, '_FILLNA_test_all.csv'); print(fname)
  write.csv(x = dfs1, file = fname, row.names = F)
  
  # return(plot)
  
}
