## ----setup, include=FALSE------------------------------------------------
if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, plyr, tidyr, stringfix) # utf8latex
p_load(tm, textreg, tidytext, parallel,pbmcapply, tidyverse, word2vec, qdapRegex)
#install.packages("word2vec")
`%+%` <- function(x,y){paste0(x, y)}
## ------------------------------------------------------------------------

# Load data
path <- "0-Data/" %+% "y1996.txt"
texts <- fread(path, stringsAsFactors = F, header = F, sep = ";")
# Process initial texts
names(texts) <- "text"
texts[,id:=1:nrow(texts)]

texts <- texts[!text %in% ""]
texts <- texts[nchar(text) > 50,]
texts[,text:=removeNumbers(text)]
texts[,text:=tolower(text)]
texts[,text:=removeWords(text, stopwords("en"))]
texts[,text:= stemDocument(text)]
# word2vec
texts.to.train <- texts$text
set.seed(123456789)
model <- word2vec(x = texts.to.train, type = "cbow", dim = 15, iter = 20)
# Prepare dictionary
dictionary <- texts %>% unnest_tokens(word, text) %>% data.table()
dictionary <- dictionary$word %>% unique()
dict_word <- dictionary[1]
for(dict_word in dictionary[1]){}
  potential.synonym <- predict(model, dict_word, type = "nearest", top_n = 5)


embedding <- as.matrix(model)
lookslike <- predict(model, c("bus", "toilet"), type = "nearest", top_n = 5)
lookslike






















# Load data
path <- "0-Data/" %+% "y1996.txt"
texts <- fread(path, stringsAsFactors = F, header = F, sep = ";")
texts[,id:=1:nrow(texts)]

names(texts) <- c("text", "id")

## ------------------------------------------------------------------------
# Save dictionary of words to use them at the stem completion step
d.dictionary <- texts %>% unnest_tokens(word, text) %>% data.table()


words <- d.dictionary$word %>% unique() 
words <- stemDocument(words)

dictionary.final <- data.table(word_initial = d.dictionary,
                               word_final = words
                               )




words


words <- stemCompletion(words, d.dictionary)
set.seed(123456789)
model <- word2vec(x = x, type = "cbow", dim = 15, iter = 20)
embedding <- as.matrix(model)
embedding <- predict(model, c("bus", "toilet"), type = "embedding")
lookslike <- predict(model, c("bus", "toilet"), type = "nearest", top_n = 5)
lookslike







# Clean texts
texts <- texts[!text %in% ""]
texts <- texts[nchar(text) > 50,]
texts[,text:=removeNumbers(text)]
texts[,text:=tolower(text)]
texts[,text:=removeWords(text, stopwords("en"))]
texts[,text:=stemDocument(text)]
## ------------------------------------------------------------------------
## Texts to tidy texts
glimpse(texts)
## ------------------------------------------------------------------------
d.tidy <- texts %>% unnest_tokens(word, text) %>% data.table()
d.tidy[,word:=stemCompletion(word,d.dictionary)]
d.tidy[,num:=1]

d.tidy <- d.tidy[,.(num=sum(num)),
                    by = .(word, id)]
names(d.tidy) <- c("word", "id", "n")

#' # Write results in file
#' # 1 Post_id to numeric - dictionary
## ------------------------------------------------------------------------

#' # Texts of Posts
## ------------------------------------------------------------------------
dir.create("0-Compiled-Data")
path <- "0-Compiled-Data/" %+% "tidy.csv"
fwrite(d.tidy, path, 
       append = F, quote = F, row.names = F, sep = ";")
#' 
## ------------------------------------------------------------------------
1+1

