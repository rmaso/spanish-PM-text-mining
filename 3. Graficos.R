# File-Name:       3. Graficos.R
# Date:            2015-09-04                                
# Author:          Rubén Masó
# Email:           ruben@maso.es
# Purpose:         Create the diferent graphics for the analysis
#                      * 
# Data Used:       Data frame
# Packages Used:   
# Output File:     
# Data Output:     

# Copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# **To start,** install the packages you need to mine text  
#      You only need to do this step once.  

##########################################################################################

######
# Set working directory and load data
######

setwd('./Documents/TextMining/PresidenteGobierno/')
load('CleanData.Rdata')


##########################################################################################
#              Histograma con los lugares de las comparecencias                          #
##########################################################################################
## set the levels in order we want
data.clean <- within(data.clean, 
                   place <- factor(place, levels=names(sort(table(place), decreasing=TRUE))))


ggplot(data.clean,aes(x=place))+
	geom_bar(binwidth=1)+
	scale_x_discrete(limits=head(names(sort(table(data.clean$place), decreasing=TRUE)),20)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))






##########################################################################################
#                                 Gráficos con bigrams                                   #
##########################################################################################
## Creación de bigrams

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)



b.freq <- list()
for(i in unique(format(as.Date(unlist(meta(docs, "datetimestamp"))), "%Y"))){
  idx <- format(as.Date(unlist(meta(docs, "datetimestamp"))), "%Y") == i
  docx <- docs[idx]  
  
  dtm <- DocumentTermMatrix(docx, control = list(tokenize = BigramTokenizer))
  b.freq[[i]] <- dtm
}



# Determinar bigrams sin valor añadido
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

# Creación de word cloud de Bigrams
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


# Creación de cluster de bigrams
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
}

