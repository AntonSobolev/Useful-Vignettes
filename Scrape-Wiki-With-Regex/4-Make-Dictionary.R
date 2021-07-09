if (!require("pacman")) install.packages("pacman")
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, xml2, plyr) # Packages for text mining
p_load(qdapRegex, textclean, stringfix)
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
path <- "0-Data/excess_mort_long.csv"
d.mortality <- fread(path)

unique.regions <- unique(d.mortality$reg_name)
unique.regions <- sort(unique.regions)

d.unique.regions <- data.table(regions_mortality = unique.regions)

dir.create("0-Data")
path <- "0-Data/" %+% "regions-mortality.csv"
fwrite(d.unique.regions, path, row.names = F, append = F, col.names = T, sep = ";")

