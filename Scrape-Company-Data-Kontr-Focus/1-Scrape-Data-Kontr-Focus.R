# In terminal run first
# cd /Users/sobol/Dropbox/current/0/218
# Run in terminal
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
# Address for Debugging
# https://focus.kontur.ru/search?region=&industry=&state=081077917&query=огрн+7709641590
# Rscript 1-Scrape-Data-Kontr-Focus.R

# Load Packages
if (!require("pacman")) install.packages("pacman")
require(pacman)
p_load(rvest) # Packages for scraping
p_load(data.table, dplyr, tidyr, stringr, methods, rowr) # Packages for text mining
#p_load(ggplot2, ggjoy) # Packages for graphic
p_load(qdapRegex)
require(RSelenium)

# Cleaning Functions
clean_up <- function (x){
  x <- gsub("[[:punct:]]"," ", x)
  x <- gsub("\r", "", x)
  x <- gsub("\n", "", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s+", "", x)
  x <- gsub("\\s+$", "", x)
  # x <- gsub("[ ]", "+", x)
  return(x)
}

clean_up2 <- function (x){
  x <- gsub(";"," ", x)
  return(x)
}
# Load data
ngo <- read.csv("companies-218.csv", stringsAsFactors = F, sep = ";") %>% data.table()
tryCatch({
# Remove already scraped companies
scraped.data <- read.csv("218-scrape-results.csv", stringsAsFactors = F,sep = "\t", header = F) %>% data.table()
names(scraped.data) <- c("company_real","region_real","company","region","zip","address","ogrn","inn")
scraped.data$id <- paste(scraped.data$company_real,scraped.data$region_real)
ngo$id <- paste(ngo$company_real,ngo$region_real)
ngo <- ngo[!ngo$id %in% scraped.data$id,]
rm(scraped.data)
ngo[,id:=NULL]
},error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # End of try catch


print(paste("We need to scrape:",nrow(ngo)))
i = 35
for (i in 145:nrow(ngo)){
  Sys.sleep(1)
  company_real <- ngo$company_real[i]
  region_real <- ngo$region_real[i]
  
  remDr <- remoteDriver(remoteServerAddr = "localhost" 
                        , port = 4445L
                        , browserName = "firefox"
  )
  
  company_real_for_url <-gsub(" ", "+", company_real)
  company_real_for_url <-gsub("\\s+$", "", company_real_for_url)
  region_real_for_url <-gsub(" ", "+", region_real)
  region_real_for_url <-gsub("\\s+$", "", region_real_for_url)
  
  url <- paste0('https://focus.kontur.ru/search?region=&industry=&state=081077917&query='
                ,company_real_for_url, "+", region_real_for_url)
  remDr$open(silent = TRUE)
  remDr$navigate(url)
  
  ogrn <- ""
  tryCatch({
    # Extract ogrn and date    
    ogrn_large <- remDr$findElements(using = 'css selector', ".ind1em+ .ind1em")
    ogrn_large <- unlist(lapply(ogrn_large, function(x){x$getElementText()}))
    # Extract OGRN
    ogrn_pattern <- "ОГРН: (.*?) – "
    ogrn <- regmatches(ogrn_large,regexec(ogrn_pattern,ogrn_large))
    ogrn <- sapply(ogrn, "[[", 2) 
    # Extract DATE
    date_pattern <- " . (.*)"
    date <- regmatches(ogrn_large,regexec(date_pattern,ogrn_large))
    date <- sapply(date, "[[", 2) 
    # Extract NAME
    company <- remDr$findElements(using = 'css selector', ".js-subject-link")
    company <- unlist(lapply(company, function(x){x$getElementText()})) %>% clean_up()
    # Extract INN    
    inn <- remDr$findElements(using = 'css selector', ".ind1em:nth-child(1)")
    inn <- unlist(lapply(inn, function(x){x$getElementText()}))
    inn_pattern <- "ИНН: (.*)"
    inn <- regmatches(inn,regexec(inn_pattern,inn))
    inn <- lapply(inn, `length<-`, max(lengths(inn)))
    inn <- sapply(inn, "[[", 2) 
    # Extract ADDRESS & REGION & ZIP    
    address <- remDr$findElements(using = 'css selector', ".noMargin.darkgreen")
    address <- unlist(lapply(address, function(x){x$getElementText()}))
    address <- clean_up2(address) # get rid of ";" in your address string
    address.update <- strsplit(address, ",")
    # Extract REGION    
    region <- sapply(address.update, "[[", 2) %>% clean_up()
    # Extract ZIP    
    zip <- sapply(address.update, "[[", 1) %>% clean_up()
    class(zip)
    # End of try catch
    print(paste(i, zip[[1]]))
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # End of try catch
  
  if(nchar(ogrn[[1]])!=0){
    d.new.line <- cbind.fill(company,region,zip,address,ogrn,inn,
                             fill=NA) %>% data.table() # fill empty elements of scraped data
    d.new.line <- cbind(company_real,region_real,d.new.line) # add real company and region to data
    
    names(d.new.line) <- c("company_real","region_real","company","region","zip","address","ogrn","inn")
    write.table(d.new.line, "218-scrape-results.csv", append = T, quote=F, row.names = FALSE, col.names = F, sep = "\t")
    print(d.new.line$zip[1])
    print("File is written")
  }
  
  remDr$close()
}