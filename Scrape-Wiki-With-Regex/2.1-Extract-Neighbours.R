if (!require("pacman")) install.packages("pacman")
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, xml2) # Packages for text mining
p_load(qdapRegex, textclean)
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
path <- "0-Data/links_to_regions.csv"
region.links <- fread(path)
region.links <- region.links$region_links
## ------------------------------------------------------------------------
# Get links from this page
#region.link.i = '%D0%90%D0%BB%D1%82%D0%B0%D0%B9%D1%81%D0%BA%D0%B8%D0%B9_%D0%BA%D1%80%D0%B0%D0%B9'
#  region.links[2]
d <- data.table()
i = 1
for(region.link.i in region.links){
  print(i)
  i = i + 1
  region.link <- "https://ru.wikipedia.org/wiki/" %+% region.link.i
  html_object <- read_html(region.link)
  region <- html_object %>% html_nodes("#firstHeading") %>% html_text()
  # Extract potential neighbors
  html_lines <- readLines(region.link)
  html_neigbours.1 <- str_subset(html_lines, "граничи")
  html_neigbours.2 <- str_subset(html_lines, "Граничи")
  html_neigbours <- paste(html_neigbours.1, html_neigbours.2, collapse = ",")
  if(length(html_neigbours) !=0){
  html_neigbours <- html_neigbours[1]
    html_neigbours <- str_extract_all(html_neigbours, 'a href="(/wiki/.*?)" title')
    html_neigbours <- lapply(X = html_neigbours, FUN = function(t) gsub(pattern = "a href", replacement = "", x = t, fixed = TRUE))
    html_neigbours <- lapply(X = html_neigbours, FUN = function(t) gsub(pattern = "/wiki/", replacement = "", x = t, fixed = TRUE))
    html_neigbours <- lapply(X = html_neigbours, FUN = function(t) gsub(pattern = '" title', replacement = "", x = t, fixed = TRUE))
    html_neigbours <- lapply(X = html_neigbours, FUN = function(t) gsub(pattern = '=\"', replacement = "", x = t, fixed = TRUE))
    # Actual regional neighbours: keep neighbour if it is in the initial list of regions
    html_neigbours <- html_neigbours[[1]] %>% as.vector()
    html_neigbours <- html_neigbours[html_neigbours %in% region.links]
    html_neigbours <- unique(html_neigbours)
    # html_neigbours <- paste0(html_neigbours, collapse = ",")
  }else{html_neigbours = NA}
    d.i <- data.table(region = region,
                      region_wiki = region.link.i,
                      neigbour_list = html_neigbours
                      )
    
    d <- list(d, d.i) %>% rbindlist()
}
dir.create("0-Data")
path <- "0-Data/" %+% "neighbours.csv"
fwrite(d, path, row.names = F, append = F, col.names = T, sep = ";")
