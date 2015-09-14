# File-Name:       2. PreparacionCorpus.R
# Date:            2015-09-04                                
# Author:          Rubén Masó
# Email:           ruben@maso.es
# Purpose:         Create the Corpus for the following analysi. The Corpus are loaded from a dataframe and following apply
#                  diferent methods to clean
#                      * removePunctuation
#                      * removeNumbers
#                      * Convert to lowercase
#                      * Remove stopwords
#                      * Stem document
#                      * Stem Completion
# Data Used:       Data frame
# Packages Used:   tm, SnowballC, 
# Output File:     Corpus.Rdata
# Data Output:     Corpus

# Copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# **To start,** install the packages you need to mine text  
#      You only need to do this step once.  

#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust",
#            "cluster", "igraph", "fpc")  
#install.packages(Needed, dependencies=TRUE)  

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   

# If you get the following message:  
#       Update all/some/none? [a/s/n]:  
#   enter "a" and press return  
##########################################################################################

######
# Set working directory and load data
######

setwd('~/Documents/TextMining/spanish-PM-text-mining/')
load('./data/CleanData.Rdata')


##########################################################################################
#                                Start Your Analyses                                     #
##########################################################################################
# **Load the R package for text mining and then load your texts into R.**
library(tm)  

m <- list(content = "contents", heading = "title", author = "authors", language = "languages", datetimestamp="datetimestamp")
myReader <- readTabular(mapping = m)

docs <- VCorpus(DataframeSource(data.clean), readerControl = list(reader = myReader, language = "es"))


# docs <- Corpus(DirSource(cname, encoding = "UTF-8"), list(language = "es"))  
# docs[[1]]$content

## Preprocessing     
docs <- tm_map(docs, removePunctuation)   # *Removing punctuation:*   
docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*   
docs <- tm_map(docs, content_transformer(tolower))   # *Converting to lowercase:*   
docs <- tm_map(docs, removeWords, stopwords("spanish"))   # *Removing "stopwords"
docs <- tm_map(docs, removeWords, c("españa", "gobierno", "presidente", "muchas", "gracias"))
library(SnowballC)  
docs.original <- docs
docs <- tm_map(docs, stemDocument, language="es")   # *Removing common word endings* (e.g., "ing", "es")  

# stemCompletion_mod <- function(x,dict=corpuscopy) {
#   PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
# }
# 
# docs <- lapply(docs, stemCompletion_mod, dict=docs.original)

#docs <- tm_map(docs, stemCompletion, dictionary=docs.original, type="prevalent")
docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace  

save(docs, docs.original, file="./data/Corpus.Rdata")
## *This is the end of the preprocessing stage.*  


### Stage the Data     
dtm <- DocumentTermMatrix(docs)  
tdm <- TermDocumentMatrix(docs)  

### Explore your data     
freq <- colSums(as.matrix(dtm))  
length(freq)  
ord <- order(freq)  
m <- as.matrix(dtm)  
dim(m)  
write.csv(m, file="DocumentTermMatrix.csv")  
### FOCUS - on just the interesting stuff...  
#  Start by removing sparse terms:  
dtms <- removeSparseTerms(dtm, 0.5) # This makes a matrix that is 10% empty space, maximum.  
### Word Frequency  
head(table(freq), 20)  
# The above output is two rows of numbers. The top number is the frequency with which
# words appear and the bottom number reflects how many words appear that frequently.
#
tail(table(freq), 20)  
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))  
freq  
# The above matrix was created using a data transformation we made earlier.
# **An alternate view of term frequency:**  
# This will identify all terms that appear frequently (in this case, 50 or more times).  
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your data.
#
#
#  
### Plot Word Frequencies
# **Plot words that appear at least 50 times.**  
library(ggplot2)  
wf <- data.frame(word=names(freq), freq=freq)  
p <- ggplot(subset(wf, freq>50), aes(word, freq))   
p <- p + geom_bar(stat="identity")  
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))  
p  
# 
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.   
findAssocs(dtm, c("merkel"), corlimit=0.98) # specifying a correlation limit of 0.98  
#
# Change "question" & "analysi" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
#
### Word Clouds!  
# First load the package that makes word clouds in R.   
library(wordcloud)  
dtms <- removeSparseTerms(dtm, 0.25) # Prepare the data (max 15% empty space)  
freq <- colSums(as.matrix(dtms)) # Find word frequencies  
dark2 <- brewer.pal(6, "Dark2")  
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   

### Clustering by Term Similarity

### Hierarchal Clustering  
# dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)  
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward")  
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using  
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters  

### K-means clustering  
library(fpc)  
library(cluster) 
# dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)  
d <- dist(t(dtms), method="euclidian")  
kfit <- kmeans(d, 2)  
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  





BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)





rm(l.freq)
l.freq <- list()
for(i in unique(format(as.Date(unlist(meta(docs, "datetimestamp"))), "%Y"))){
  idx <- format(as.Date(unlist(meta(docs, "datetimestamp"))), "%Y") == i
  docx <- docs[idx]  
  
  dtm <- DocumentTermMatrix(docx)  
  dtms <- removeSparseTerms(dtm, 0.25) # Prepare the data (max 15% empty space)  
#  freq <- colSums(as.matrix(dtms))  
  
  l.freq[[i]] <- dtms
}

rm(b.freq)
b.freq <- list()
for(i in unique(format(as.Date(unlist(meta(docs, "datetimestamp"))), "%Y"))){
  idx <- format(as.Date(unlist(meta(docs, "datetimestamp"))), "%Y") == i
  docx <- docs[idx]  
  
  dtm <- DocumentTermMatrix(docx, control = list(tokenize = BigramTokenizer))
  b.freq[[i]] <- dtm
}




stop.words = c("señor señor", "segund lug", "prim lug", "dud algun", "punt vist", "sab si", "quier dec",
               "pued ser", "pued asegur", "señor rodriguez", "prim ministr", "señor rajoy", "deb ser", "va ser",
               "quer pregunt", "rodriguez zapater", "jimenez losant", "f jimenez", "quer sab", "primer vez",
               "cad dia", "cad vez", "respuest pregunt", "secretari general", "quier record", "c francin",
               "pregunt formul", "maner singul", "formul don", "l olmo", "dos pais", "pued dec", "pued hac", "c herrer",
               "form part", "ultim años", "tom decision", "cuatr años", "prim dia", "gust sab", "consej ministr",
               "grup parlamentari", "part popular", "president español", "part popul", "deb dec", "an pastor",
               "usted sab", "part ahi", "señor señores", "perez rubalc", "deb hac", "cualqui cas", "si usted",
               "pep buen", "garc campoy", "c garc", "pregunt si", "i gabilond", "se si", "p señor", "buen tard",
               "garc abadill")
for(idtm in names(b.freq) ){
  dtms <- removeSparseTerms(b.freq[[idtm]], 0.80) # Prepare the data (max 15% empty space)  
  freq <- colSums(as.matrix(b.freq[[idtm]])) # Find word frequencies  
  freq <- freq[!names(freq) %in% stop.words]
  dark2 <- brewer.pal(6, "Dark2")  
  
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste("Año", idtm))
  wordcloud(names(freq), freq, max.words=20, rot.per=0.1, colors=dark2, random.order=FALSE)   
}


for(idtm in names(b.freq) ){
  dtms <- removeSparseTerms(b.freq[[idtm]], 0.80)# Prepare the data (max 15% empty space)  
  d <- as.matrix(dist(t(dtms), method="euclidian"))   # First calculate distance between words
  d <- as.dist(d[!colnames(d) %in% stop.words, !colnames(d) %in% stop.words])
  fit <- hclust(d=d, method="ward")  

  layout(matrix(c(1, 2), nrow=2), heights=c(1, 5))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste("Año", idtm))
  plot(fit, hang=-1, main="")
  groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using  
  rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters  
  
#  kfit <- kmeans(d, 5)  
#  clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, main="")  
}






