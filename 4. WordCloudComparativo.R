# File-Name:       4. WordCloudComparativo.R           
# Date:            2015-09-04                                
# Author:          Rubén Masó
# Email:           ruben@maso.es
# Purpose:         Create a word cloud to compare the most used words between Jose Luis Rodriguez Zapatero y Maria Rajoy Brey
# Data Used:       Data frame
# Packages Used:   tm, ggplot2
# Output File:     CleanData.Rdata
# Data Output:     Word Cloud comparativo entre las palabras más utilizadas por Zapatero y Rajoy

# Copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# Thanks to Drew Conway for the original code to create these magnificent word clouds


# Load libraries and data
library(tm)
library(ggplot2)

# Set working directory
setwd('~/Documents/TextMining/spanish-PM-text-mining/')

### Step 1: Load in text data, clean, and analyze overlapping terms
load("./data/Corpus.Rdata")
idxZapatero <- unlist(meta(docs, "author")) == "Jose Luis Rodriguez Zapatero"
idxRajoy <- unlist(meta(docs, "author")) == "Mariano Rajoy Brey"

# split corpus between two presidents
dataframe<-data.frame(zapatero=paste(unlist(sapply(docs[idxZapatero], `[`, "content")), collapse=" "),
                      rajoy=paste(unlist(sapply(docs[idxRajoy], `[`, "content")), collapse=" "),
                      stringsAsFactors=F)

speeches<-Corpus(VectorSource(dataframe))


# Get word counts
zapatero.wc<-length(unlist(strsplit(content(speeches[[1]]), " ")))
rajoy.wc<-length(unlist(strsplit(content(speeches[[2]]), " ")))

# Create a Term-Document matrix
add.stops=c("")
speech.control=list(stopwords=c(stopwords("spanish"),add.stops), removeNumbers=TRUE, removePunctuation=TRUE)
speeches.matrix<-TermDocumentMatrix(speeches, control=speech.control)

# Create data frame from matrix
speeches.df<-as.data.frame(inspect(speeches.matrix))
names(speeches.df) <- c("zapatero", "rajoy")
speeches.df<-subset(speeches.df, zapatero>0 & rajoy>0)
speeches.df[,c(1,2)] <- t(t(speeches.df[,c(1,2)])/colSums(speeches.df[,c(1,2)]))

speeches.df<-transform(speeches.df, freq.dif=round(zapatero-rajoy, 7))    

### Step 2: Create values for even y-axis spacing for each vertical
#           grouping of word freqeuncies

# Create separate data frames for each frequency type
zapatero.df<-subset(speeches.df, freq.dif>0)   # Said more often by zapatero
rajoy.df<-subset(speeches.df, freq.dif<0)   # Said more often by rajoy
equal.df<-subset(speeches.df, freq.dif==0)  # Said equally


zapatero.df <- head(zapatero.df[ order(-zapatero.df[,1]), ], 50)
zapatero.df <- transform(zapatero.df, freq.dif = round((freq.dif - min(freq.dif))/(2*max(freq.dif)), 1)+0.1 )

rajoy.df <- head(rajoy.df[ order(-rajoy.df[,2]), ], 50)
rajoy.df <- transform(rajoy.df, freq.dif = round((freq.dif + min(abs(freq.dif)))/(2*max(abs(freq.dif))), 1)-0.1 )

equal.df <- head(equal.df[ order(-equal.df[,1]-equal.df[,2]), ], 20)




# This function takes some number as spaces and returns a vertor
# of continuous values for even spacing centered around zero
optimal.spacing<-function(spaces) {
  if(spaces>1) {
    spacing<-1/spaces
    if(spaces%%2 > 0) {
      lim<-spacing*floor(spaces/2)
      return(seq(-lim,lim,spacing))
    }
    else {
      lim<-spacing*(spaces-1)
      return(seq(-lim,lim,spacing*2))
    }
  }
  else {
    return(0)
  }
}

# Get spacing for each frequency type
zapatero.spacing<-sapply(table(zapatero.df$freq.dif), function(x) optimal.spacing(x))
rajoy.spacing<-sapply(table(rajoy.df$freq.dif), function(x) optimal.spacing(x))
equal.spacing<-sapply(table(equal.df$freq.dif), function(x) optimal.spacing(x))

# Add spacing to data frames
zapatero.optim<-rep(0,nrow(zapatero.df))
for(n in names(zapatero.spacing)) {
  zapatero.optim[which(abs(zapatero.df$freq.dif-as.numeric(n))<0.01)]<-zapatero.spacing[[n]]
}
zapatero.df<-transform(zapatero.df, Spacing=zapatero.optim)

rajoy.optim<-rep(0,nrow(rajoy.df))
for(n in names(rajoy.spacing)) {
  rajoy.optim[which(abs(rajoy.df$freq.dif-as.numeric(n))<0.01)]<-rajoy.spacing[[n]]
}
rajoy.df<-transform(rajoy.df, Spacing=rajoy.optim)

equal.df$Spacing<-as.vector(equal.spacing)

### Step 3: Create visualization
comp.cloud <- ggplot(zapatero.df, aes(x=freq.dif, y=Spacing))+geom_text(aes(size=zapatero, label=row.names(zapatero.df), colour=freq.dif))+
  geom_text(data=rajoy.df, aes(x=freq.dif, y=Spacing, label=row.names(rajoy.df), size=rajoy, color=freq.dif))+
  geom_text(data=equal.df, aes(x=freq.dif, y=Spacing, label=row.names(equal.df), size=zapatero, color=freq.dif))+
  scale_size(range=c(3,11), name="Frecuencia")+scale_colour_gradient(low="darkred", high="darkblue", guide="none")+
  scale_x_continuous(breaks=c(min(rajoy.df$freq.dif),0,max(zapatero.df$freq.dif)),labels=c("Más dichas por Rajoy","Dichas igualitariamente","Más dichas por Zapatero"))+
  scale_y_continuous(breaks=c(0),labels=c(""))+xlab("")+ylab("")+
#   theme_bw()+
  labs(title="Comparativa Discursos Rajoy vs Zapatero")
comp.cloud

ggsave(plot=comp.cloud,filename="./graphics/wordcloud_comparativo_Zapatero_Rajoy.png",width=13,height=7)
