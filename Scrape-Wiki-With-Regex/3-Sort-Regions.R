if (!require("pacman")) install.packages("pacman")
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, xml2, plyr) # Packages for text mining
p_load(qdapRegex, textclean, stringfix)
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
path <- "0-Data/neighbours.csv"
d <- fread(path)
d <- unique(d)
regions.of.interest <- unique(d$region)
regions.of.interest <- regions.of.interest[!"Край" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[!"Область" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[!"Город федерального значения" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[!"Автономная область" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[!"Автономный округ" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[!"Республика Крым" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[!"Севастополь" %g% regions.of.interest]
regions.of.interest <- regions.of.interest[-80] # drop "Ненецкий автономный округ"

d <- d[region %in% regions.of.interest,]
d.wiki.dictionary <- unique(d[,.(region, region_wiki)])

d[,neigbour := mapvalues(neigbour_list,
                         from = d.wiki.dictionary$region_wiki,
                         to = d.wiki.dictionary$region)
  ]

d <- d[neigbour %in% regions.of.interest,]

# d.wiki.dictionary$region[!d.wiki.dictionary$region %in% unique(d$neigbour)]
# d[region == "Сахалинская область",]

d <- d[, .(region,neigbour)]

dir.create("0-Data")
path <- "0-Data/" %+% "neighbours-clean.csv"
fwrite(d, path, row.names = F, append = F, col.names = T, sep = ";")
# Create dictionary
regions.of.interest <- sort(regions.of.interest)
d.regions.of.interest <- data.table(regions_wiki = regions.of.interest)
path <- "0-Data/" %+% "regions-wiki.csv"
fwrite(d.regions.of.interest, path, row.names = F, append = F, col.names = T, sep = ";")
