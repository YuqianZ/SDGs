rm(list=ls());                         # clear Console Window
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # include all GGPlot2 functions
library(package=dplyr);                # include all dplyr functions
library(package=tidyr);               # include all tidyr functions
# library(package=gridExtra);            # include all gridExtra functions
library(plyr)
library(tibble)
library(reshape2)
library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)
library(maps)

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(jsonlite)
library(RCurl)

# library(rworldmap)
# library(sp)

library(rnaturalearthdata)
library(rnaturalearth)
library(sf)

# library(Matrix)
# library(lme4)
library(data.table)