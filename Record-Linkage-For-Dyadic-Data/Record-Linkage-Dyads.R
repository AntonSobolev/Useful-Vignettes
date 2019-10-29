if (!require("pacman")) install.packages("pacman")
#install.packages("experiment")
library(pacman)
p_load(readr, dplyr,lubridate,magrittr,textreadr,data.table, tidyr, stringr)
p_load(countrycode,stringfix)
`%+%` <- function(x,y){paste0(x, y)}
len <- function(x){length(x)}

# Toy Data
d <- data.table(
      source = c(1,1,2,2,3,3),
      target = c(2,3,1,3,1,2),
      y = c(4,5,6,7,8,9) 
)
# Subset source - target columns
d.ids <- d %>% select(source, target)
# sort each row in alpabetic order 
d.ids.arranged <- t(apply(d.ids, 1, sort)) %>% data.table()
# Assign names to columns
names(d.ids.arranged) <- c("country1", "country2")
# Create a key. This key will be identical to all rows which refere to the same country-country dyad
d[,key:= d.ids.arranged$country1 %+% "+" %+% d.ids.arranged$country2]
# Calculate variables of interest

d <- d[,.(y = sum(y)), by = key]
