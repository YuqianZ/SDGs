

### fill na function for wb data
library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)
library(ggplot2)


function_fill_na_binary <- function(df, yr_max = 2020) {
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
      
      
      
      dfij <- df %>%
        dplyr::filter(Code == uni_cty[i]) %>% ## loop each nation
        dplyr::filter(SDG  == ind) %>%        ## loop each SDG
        # gather(key = 'year', value =  'value', -c(1:6)) %>%
        dplyr::mutate(Year =  Year) %>%
        # dplyr::mutate(year = year(as.Date(Year, format="%Y"))) #%>%
        dplyr::mutate(Value = as.numeric(value)) %>%
        arrange(Year) # %>% ## order by date  
      
      head(dfij)
      
      find1 <- dfij %>%
        dplyr::filter(value == 100) %>%
        mutate(Year = as.numeric(as.character(Year))) %>%
        arrange(Year) 
      
      
      ## if there is a 1 in the data, get the `year`
      if (nrow(find1) > 0) {
        yr <- find1$Year[1] 
        
      } else {
        
        ## if no 1 was found, assign 2021 to yr
        yr <- yr_max + 1
      }
      
      
      ## after determine `yr`, impute data
      d_yr <- dfij %>%
        mutate(Value = ifelse(Year < yr, 0, 100))
      
      # view(d_yr)
      
      
      
      ## combine process data from each country into one dataframe
      dfs <- rbind(dfs, d_yr) %>% 
        arrange(Code, SDG)


    }
  }
  
  ### here I just save the data, you can modify the above code and then save to the format you like
  fname <- paste0(#dir.output, 
    csv, '_FILLNA_binary.csv'); print(fname)
  write.csv(x = dfs, file = fname, row.names = F)
  
  return(dfs)
  
}




## test ----------------------------------------------------------------------

# i = 1
# ind <- "15_6_1_2"
# df <- dat2
# uni_cty <- unique(df$Code)
# 
# dfij <- df %>%
#   dplyr::filter(Code == uni_cty[i]) %>% ## loop each nation
#   dplyr::filter(SDG  == ind) %>%        ## loop each SDG
#   # gather(key = 'year', value =  'value', -c(1:6)) %>%
#   dplyr::mutate(Year =  Year) %>%
#   # dplyr::mutate(year = year(as.Date(Year, format="%Y"))) #%>%
#   dplyr::mutate(Value = as.numeric(value)) %>%
#   arrange(Year) # %>% ## order by date  
# 
# head(dfij)
# 
# find1 <- dfij %>%
#   dplyr::filter(value == 100) %>%
#   mutate(Year = as.numeric(as.character(Year))) %>%
#   arrange(Year) 
# 
# 
# ## if there is a 1 in the data, get the `year`
# if (nrow(find1) > 0) {
#   yr <- find1$Year[1] 
#   
# } else {
#   
#   ## if no 1 was found, assign 2021 to yr
#   yr <- 2021
# }
# 
# 
# ## after determine `yr`, impute data
# d_yr <- dfij %>%
#   mutate(Value = ifelse(Year < yr, 0, 1))


