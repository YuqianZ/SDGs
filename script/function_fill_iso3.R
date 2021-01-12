

### fill na country code

library(dplyr)

countryiso3 <- read.csv('CountryCode.csv')
View(countryiso3)

test <- dt %>% filter(Country == 'Afghanistan')

ctr <- test %>%
  distinct(Country, CountryCode) %>%
  filter(!is.na(CountryCode))

df2 <- test %>% 
  select(-CountryCode) %>%
  left_join(x = ., y = ctr, by = 'Country')