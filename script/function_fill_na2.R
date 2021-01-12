

### fill na function for wb data

library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)

function_fill_na_wb <- function(df) {
  dfs <- data.frame()
  
  # df <- test
  n <- length(unique(df$CountryCode)); n
  uni_ctylist <- unique(df$CountryCode)
  
  for (i in seq(1:n)) {
    # print("Country No.:")
    print(i) 
    ### loop each country code
    print(uni_ctylist[i]) 
    
    # i = 1
    dfi <- df %>% dplyr::filter(CountryCode == uni_ctylist[i])
    
    ### number of SDG indicators
    n2 <- length(unique(dfi$SDG)); n2
    # print("SDG No.:")
    unique(dfi$SDG)
    
    for (j in seq(1:n2)) {
      print(j)
      df1 <- dfi %>%
        dplyr::filter(SDG == unique(dfi$SDG)[j]) %>% ## loop each nation
        # gather(key = 'year', value =  'value', -c(1:6)) %>%
        dplyr::mutate(year =  Year) %>%
        # dplyr::mutate(year = year(as.Date(Year, format="%Y"))) #%>%
        dplyr::mutate(value = as.numeric(value)) %>%
        dplyr::arrange(year) # %>% ## order by date
      
      ### if too many NA, then keep as it is - may drop this data; if not too much, then interpolation
      if (sum(is.na(df1$value)) > (length(df1$CountryCode)-2)) {
        df1 <- df1 %>%
          dplyr::mutate(value_ks = value) #%>%
          # dplyr::arrange(SDG, CountryCode, year)
        } else {
          df1 <- df1 %>%
            dplyr::mutate(value_ks = na_interpolation(x = value, option = 'stine'))}
      
      dfs <- rbind(dfs, df1)
      
    }
  }
  
  view(dfs)
  ### plot and examine the interpolation
  # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
  plot <- dfs %>%
    ### randomly choose 20 countries and plot
    # filter(CountryCode %in% df$CountryCode[sample(1:3, size = 3, replace = T)]) %>% 
    
    ggplot()+
    geom_line(aes(x = year, y = value),    color = 'blue', size = 6) +
    geom_line(aes(x = year, y = value_ks), color = 'red',  size = 2) +
    facet_grid(SDG~CountryCode, scales = 'free_y') +
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
                  csv, '_FILLNA_test1.csv'); print(fname)
  write.csv(x = dfs, file = fname, row.names = F)
  
  return(plot)
  return(dfs)
  
}




# if (sum(is.na(df1$value)) > (length(df1$CountryCode)-2)) {
#   df1 <- df1 %>%
#     dplyr::mutate(value_ks = value) #%>%
#   # dplyr::arrange(SDG, CountryCode, year)
# } else {df1 <- df1 %>%
#   dplyr::mutate(value_ks = na_interpolation(x = value, option = 'stine'))}


