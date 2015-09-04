# File-Name:       0. Crawler.R           
# Date:            2015-09-04                                
# Author:          Rubén Masó
# Email:           ruben@maso.es
# Purpose:         Download the speeches of the Spanish prime minister from 2004 to 2015 on the website of the Goverment of Spain
# Data Used:       Downloaded data from:
#                       * http://www.lamoncloa.gob.es/presidente/intervenciones/Paginas/index.aspx
# Packages Used:   bitops, RCurl, stringr, XML
# Output File:     RawData.Rdata
# Data Output:     Dataframe containing all the speeches of the Spanish Prime Minister from 2004 to 2015

# Copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

#http://rstudio-pubs-static.s3.amazonaws.com/58718_f64b04d8fdcd4007a9d11ea859751628.html

# Load libraries
library(bitops)
library(RCurl)
library(stringr)
library(XML)

# Set working directory
setwd('./Documents/TextMining/PresidenteGobierno/')

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Extact value
extractValue<-function(object){
  if(length(object)>0){
    paste(xmlSApply(object[[1]], xmlValue), collapse="")
  }else{
    ''
  }
}

# myHttpheader is got from firefox-developers tools/network/get/type(html)/file(global)
myHttpheader<- c(
  "User-Agent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-US,en;q=0.5",
  "Connection"="keep-alive"
)
d2 =debugGatherer()

# make handle by getCurlHandle
cHandle2<- getCurlHandle(
  httpheader=myHttpheader,followlocation=1,
  debugfunction=d2$update,verbose=TRUE,cookiefile="cookie1.txt")

# La página web del gobierno http://www.lamoncloa.gob.es/presidente/intervenciones/Paginas/index.aspx
# permite el filtrado de las comparecencias del presidente del gobierno filtradas por mes
# Recorremos los diferentes meses desde el principio 04/2004 al último mes disponible 07/2015
for(d in seq(as.Date('2004-04-01'), as.Date('2015-07-01'), by='month')){
  cat(paste('Procesando ', format(as.Date(d, origin="1970-01-01"), "%m/%Y"), '\n', sep=''))
  
  # Construcción de la URL para el Año/Mes. Devuelve el listado de comparecencias del Presidente
  # para el Año/Mes indicado
  url<- paste("http://www.lamoncloa.gob.es/presidente/intervenciones/Paginas/index.aspx?mts=",format(as.Date(d, origin="1970-01-01"), "%Y"),format(as.Date(d, origin="1970-01-01"), "%m"),sep="")
  domain.info <- parse_url(url)
  page<-1
  
  
  ##scrap the 1st page
  # Se realiza dos veces la descarga de la página, ya que en ciertas ocasiones (que no he llegado a determinar)
  # la primera descarga no devolvia el listado con las comparecencias del persidente
  temp <- getURL(url, curl=cHandle2, verbose=TRUE)
  temp <- getURL(url, curl=cHandle2, verbose=TRUE)
  cont<-TRUE

  # Ciertos meses tienen varias páginas de comparecencias
  # Realizamos el bucle, mientras nos queden paginas por descargar
  while(cont){
    # Parseo del documento a uno tipo XML para poder utilizar el xPath para buscar los datos que
    # nos interesan
    doc<-htmlTreeParse(temp, useInternalNodes=TRUE)
    src <- xpathApply(xmlRoot(doc), "//ul[@class='buscadorAvanzadoResultados']//li")
    

    # Parseado de los datos importantes de la comparecencia
    #   Fecha
    #   Ciudad
    #   Sumario
    #   Título
    #   Enlace
    # y posterior descarga completa de la misma
    ldate<-vector();lplace<-vector();lsummary<-vector();ltitle<-vector();llink<-vector();lcontent<-vector()
    for (i in 1:length(src)) {
      date<-xpathApply(src[[i]], ".//p[@class='sumarioFecha']")
      place<-xpathApply(src[[i]], ".//p[@class='sumarioMinisterio']")
      summary<-xpathApply(src[[i]], ".//p[@class='intervencionesSumarioTitulo']")
      title<-xpathApply(src[[i]], ".//a", xmlGetAttr, "title")
      link<-xpathApply(src[[i]], ".//a", xmlGetAttr, "href")
      
      # Descarga completa de la comparecencia y paseado del contenido
      res<-htmlTreeParse(paste(domain.info$scheme,"://", domain.info$hostname, link[[1]], sep=""), isURL=TRUE, useInternalNodes=TRUE)
      content<-xpathApply(xmlRoot(res), "//div[@class='contenidoTexto']")
      
      
      ldate[i]<-extractValue(date)
      lplace[i]<-extractValue(place)
      lsummary[i]<-extractValue(summary)
      ltitle[i]<-title[[1]]
      llink[i]<-paste(domain.info$scheme,"://", domain.info$hostname, link[[1]], sep="")
      lcontent[i]<-extractValue(content)
    }
    
    # Guardado de los datos en el dataframe DATA, cada discurso en un elemento más del dataframe
    if(!exists("DATA")){
      DATA <- data.frame(date=ldate, place=lplace, summary=lsummary, title=ltitle, link=llink, content=lcontent)
    }else{
      tmp <- data.frame(date=ldate, place=lplace, summary=lsummary, title=ltitle, link=llink, content=lcontent)
      DATA<-rbind(DATA, tmp)
    }
    
    # Validar si estamos en la última página
    # Construcción de la URL para obtener la siguiente página
    npage <- xpathApply(xmlRoot(doc), "//div[@id='SelectorPaginaSiguiente']//a", xmlGetAttr, "onclick")
    if(length(npage)>0){  
      ss <- str_locate(npage[[1]], "\\(.*\\)")
      param <- strsplit(gsub("'", "", substr(npage[[1]], ss[1]+1, ss[2]-1)), ",")[[1]]
      
      temp<- getURL(
        paste(domain.info$scheme,"://", domain.info$hostname, 
              "/Apps/Common/AjaxPages/WCCEnlacesPagedServer.aspx?",
              "source=",trim(param[3]),
              "&p=",trim(param[1]),
              "&pt=",trim(param[2]),
              "&container=",trim(param[4]),
              "&guid=",trim(param[6]),
              "&emo=undefined&mts=",trim(param[7]), sep=""),
        curl=cHandle2,
        referer=url
      )
      
      page<-page+1
    }else{
      cont<-FALSE
    }  
  } # Fin while de existencia de nuevas paginas para el mes 
} # Fin for para los meses 


save(DATA, file = "RawData.Rdata")
