if (!require("pacman")) install.packages("pacman")
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, xml2, plyr) # Packages for text mining
p_load(qdapRegex, textclean, stringfix)
`%+%` <- function(x,y){ paste0(x, y)}
## --------------------------------------------------------------------
path <- "0-Data/excess_mort_long.csv"
d.mortality <- fread(path)
  d.mortality[,date:=as.character(date)]
path <- "0-Data/dictionary.csv"
d.dictionary <-fread(path)
path <- "0-Data/neighbours-clean.csv"
d.neighbours <-fread(path)
# Use dictionary
d.neighbours[,region_correct:=mapvalues(region,
                                        from = d.dictionary$regions_wiki,
                                        to = d.dictionary$regions_mortality)
             ]

d.neighbours[,neighbour_correct:=mapvalues(neigbour,
                                        from = d.dictionary$regions_wiki,
                                        to = d.dictionary$regions_mortality)
]
names(d.neighbours)
d.neighbours <- d.neighbours[,.(region = region_correct, 
                                neighbour = neighbour_correct)]

dates <- unique(d.mortality$date)
regions <- unique(d.neighbours$region)
d <- data.table()
for(date.i in dates){
  for(region.i in regions){
    print(date.i %+% region.i)
    neighbours.i <- d.neighbours[region == region.i,neighbour]
    mortality.avg.i <- d.mortality[date == date.i & reg_name %in% neighbours.i, excess_mort_p]
    mortality.avg.i <- mean(mortality.avg.i)
    mortality.actual.i <- d.mortality[date == date.i & reg_name == region.i, excess_mort_p]
    #mortality.actual.i <- d.mortality[reg_name == region.i, excess_mort_p]
    
    d.i = data.table(date = date.i,
                     reg_name = region.i,
                     excess_mort_p = mortality.actual.i,
                     excess_mort_p_instr = mortality.avg.i
                     )
    d <- list(d, d.i) %>% rbindlist()
  }
}


dir.create("0-Data")
path <- "0-Data/" %+% "excess_mort_long_with_instrument.csv"
fwrite(d, path, row.names = F, append = F, col.names = T, sep = ";")
