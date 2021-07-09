if (!require("pacman")) install.packages("pacman")
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, xml2) # Packages for text mining
p_load(qdapRegex)
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
root.link <- "wikipedia-regions.html"
## ------------------------------------------------------------------------
# Get links from this page
lines.from.html <- readLines(root.link)
lines.from.html <- lines.from.html[281:370] # subset lines with regions 
links.from.lines <- c()
i = 1
for(line in lines.from.html){
  print(i)
  i = i + 1
potential.links <- str_extract_all(line, 'a href="(/wiki/.*?)" title')
  link <- potential.links[[1]][1]
  link <- gsub('a href="(/wiki/.*?)" title', "\\1", link)
  link <- gsub('/wiki/',"",link)
    link <- gsub('mw-redirect',"",link)
    link <- gsub('class=',"",link)
    link <- gsub('"| ',"",link)
  links.from.lines <- c(links.from.lines,link )
}

d.links.from.lines <- data.table(region_links = links.from.lines)

dir.create("0-Data")
path <- "0-Data/" %+% "links_to_regions.csv"
fwrite(d.links.from.lines, path, row.names = F, append = F, col.names = T, sep = ";")
