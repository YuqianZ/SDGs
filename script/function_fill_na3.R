

### fill na function for wb data

SDG_Imputeable <- c("15_1_1", "15_1_2_1", "15_1_2_2", "15_2_1_1", "15_2_1_2",
                    "15_2_1_3", "15_4_1","15_4_2", "15_5_1") 

function_fill_na_wb <- function(df) {
  dfs <- data.frame()
  n <- length(unique(df$Code)) 
  
  for (i in seq(1:n)) {
    print(i) 
    ### loop each Code
    uni_cty <- unique(df$Code)
    print(uni_cty[i])
    
    df1 <- df %>%
      filter(Code == uni_cty[i]) %>% ## loop each nation
      # gather(key = 'year', value =  'value', -c(1:6)) %>%
      mutate(Year =  Year) %>%
      # dplyr::mutate(year = year(as.Date(Year, format="%Y"))) #%>%
      mutate(Value = as.numeric(value)) %>%
      arrange(Year) # %>% ## order by date 
    
    ### if too many NA, then keep as it is - may drop this data; if not too much, then interpolation
    if (!(df1$SDG %in% SDG_Imputeable)) {
      df1 <- df1 %>%
        mutate(value_ks = Value)} ## 
    else {
      df1 <- df1 %>%
        group_by(SDG) %>%
        mutate(value_ks = na_interpolation(x = Value, option = 'stine'))}
    
    dfs <- rbind(dfs, df1)
    
    dfs1 <- dfs[order(dfs$Code, dfs$SDG),]
  }
  
  view(dfs)
  view(dfs1)
  
  
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
    csv, '_FILLNA_test3.csv'); print(fname)
  write.csv(x = dfs1, file = fname, row.names = F)
  
  # return(plot)
  
}
