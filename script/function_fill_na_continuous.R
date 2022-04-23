

### fill na function for wb data
library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)
library(ggplot2)


function_fill_na_continuous <- function(df) {
  dfs <- data.frame()
  n <- length(unique(df$Code)) ## Code - county iso code
  
  ind_ls <- unique(df$SDG)
  
  for (i in seq(1:n)) {
    print(i) 
    ### loop each county Code
    uni_cty <- unique(df$Code)
    print(uni_cty[i])
    
    
    ## loop SDG indicators
    for(ind in ind_ls) {
      ## dfif: data for country i 's jth SDG
      dfij <- df %>%
        dplyr::filter(Code == uni_cty[i]) %>% ## loop each nation
        dplyr::filter(SDG  == ind) %>%        ## loop each SDG
        # gather(key = 'year', value =  'value', -c(1:6)) %>%
        dplyr::mutate(Year =  Year) %>%
        # dplyr::mutate(year = year(as.Date(Year, format="%Y"))) #%>%
        dplyr::mutate(Value = as.numeric(value)) %>%
        arrange(Year) # %>% ## order by date 
      
      

    
    
    # m <- length(unique(df$SDG)) * length(unique(df$Year))
    # for (j in seq(1: m)) {
    #   print(j)
    #   print(df$SDG[j])
    # }

    ### if too many NA, then keep as it is - may drop this data; if not too much, then interpolation
    if (sum(is.na(dfij$Value)) > (length(dfij$Code)-2)) {
      dfij <- dfij %>%
        mutate(value_ks = Value)} ## 
    else {
      dfij <- dfij %>%
        # group_by(SDG) %>%
        dplyr::mutate(value_ks = na_interpolation(x = Value, option = 'stine'))}

    
    ## combine process data from each country into one dataframe
    dfs <- rbind(dfs, dfij)
    
    dfs1 <- dfs[order(dfs$Code, dfs$SDG),]
  }
  }
  
  
  # view(dfs)
  # view(dfs1)
  
  
  # ### plot and examine the interpolation
  # # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
  # plot <- dfs %>%
  #   ### randomly choose 20 countries and plot
  #   # filter(CodeCode %in% df$CodeCode[sample(1:3, size = 3, replace = T)]) %>%
  # 
  #   ggplot()+
  #   geom_line(aes(x = Year, y = Value),    color = 'blue', size = 6) +
  #   geom_line(aes(x = Year, y = value_ks), color = 'red', size = 2) +
  #   facet_wrap(~Codecode, scales = 'free_y') +
  #   # xlim(1990, 2005) +
  #   # ylim(0, 7*10^5)+
  #   theme_bw()
  # #
  # 

  
  ### update data and save to local
  # dfs_update <- dfs %>%
  #   select(-value) %>%
  #   spread(key = year, value = value_ks) %>%
  #   # left_join(x=ctr_eora, y = ., by=c('Code_eora', 'CodeCode')) ### make sure the same order 
  # fname <- paste0(dir.output, iname, '_FILLNA.xlsx'); print(fname)
  # write.csv(x = dfs_update, file = fname, row.names = F)
  # write_xlsx(x = dfs_update, path = fname)
  
  ### here I just save the data, you can modify the above code and then save to the format you like
  fname <- paste0(#dir.output, 
                  csv, '_FILLNA_continuous.csv'); print(fname)
  write.csv(x = dfs1, file = fname, row.names = F)
  
  # return(plot)
  return(dfs1)
}
