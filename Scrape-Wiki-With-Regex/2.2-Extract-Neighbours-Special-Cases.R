if (!require("pacman")) install.packages("pacman")
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, xml2) # Packages for text mining
p_load(qdapRegex, textclean)
`%+%` <- function(x,y){ paste0(x, y)}
## ------------------------------------------------------------------------
# Leningrad oblast
region.link.i <- c(
  "%D0%9B%D0%B5%D0%BD%D0%B8%D0%BD%D0%B3%D1%80%D0%B0%D0%B4%D1%81%D0%BA%D0%B0%D1%8F_%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C"
  )

  region.link <- "https://ru.wikipedia.org/wiki/" %+% region.link.i
  
  
  html_object <- read_html(region.link)
  region <- html_object %>% html_nodes("#firstHeading") %>% html_text()
  # Extract potential neighbors
  html_neigbours <- c("%D0%A0%D0%B5%D1%81%D0%BF%D1%83%D0%B1%D0%BB%D0%B8%D0%BA%D0%B0_%D0%9A%D0%B0%D1%80%D0%B5%D0%BB%D0%B8%D1%8F",
                      "%D0%92%D0%BE%D0%BB%D0%BE%D0%B3%D0%BE%D0%B4%D1%81%D0%BA%D0%B0%D1%8F_%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C",
                      "%D0%9D%D0%BE%D0%B2%D0%B3%D0%BE%D1%80%D0%BE%D0%B4%D1%81%D0%BA%D0%B0%D1%8F_%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C",
                      "%D0%9F%D1%81%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B0%D1%8F_%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C",
                      "%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3"
                      )
    d <- data.table(region = region,
                      region_wiki = region.link.i,
                      neigbour_list = html_neigbours
    )

dir.create("0-Data")
path <- "0-Data/" %+% "neighbours.csv"
fwrite(d, path, row.names = F, append = T, col.names = F, sep = ";")
