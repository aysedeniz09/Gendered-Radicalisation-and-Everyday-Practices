---
title: "everyday-gender-extr"
author: "adl"
date: "2022-10-29"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
```
### Supplementary Code for:
### "Gendered Radicalisation and “Everyday Practices": An Analysis of Extreme Right and Islamic State Women-Only Forums
#### Authors: [Yannick Veilleux-Lepage, PhD](https://www.yveilleuxlepage.com/new-page-1); [Alexandra Phelan, PhD](https://www.monash.edu/arts/gender-peace-security/our-people/key-researchers/alexandra-phelan); & [Ayse D. Lokmanoglu, PhD](https://www.adenizlok.com/)
#### <DOI>
Citation: 

The code below includes all the steps of the methodology, and the visualizations. 

For questions, or more information on the code please contact: 
Ayse D. Lokmanoglu\
ayse [dot] lokmanoglu [at] northwestern [dot] edu

## Code
1. Stormfront Data
Load the package
```{r}
options(stringsAsFactors = FALSE)
library(readr)
library(quanteda) 
library(topicmodels)
library(ldatuning) 
library(doParallel) 
library(LDAvis) 
library(wordcloud2) 
library(ggplot2) 
library(WriteXLS)
library(lda)
library(devtools)
library(quanteda)
library(topicmodels)
library(readxl)
library(dplyr)
library(tidyr)
library(tm)
library(tidyverse)
library(tidytext)
library(reshape2)
library(wordcloud)
library(formattable)
library(knitr)
library(kableExtra)
library(zoo)
library(lubridate)
library(htmlTable)
library(viridis)
library(viridisLite)
library(RColorBrewer)
```
Load the data *Due to the extremist content within the data, we will not be providing the data. Please contact the authors for any questions*
```{r}
Women_Forum_Posts
```
Preprocess the data
```{r}
Women_Forum_Posts$text<-Women_Forum_Posts$post_content
Women_Forum_Posts$date<-Women_Forum_Posts$post_datetime
Women_Forum_Posts$month<-substr(Women_Forum_Posts$date,1,7)
Women_Forum_Posts$year<-substr(Women_Forum_Posts$date,1,4)
Women_Forum_Posts$date<-as.Date(Women_Forum_Posts$date,"%Y-%m-%d")
Women_Forum_Posts$month<-as.yearmon(Women_Forum_Posts$month)
Women_Forum_Posts$year <- floor_date(Women_Forum_Posts$date, "year")
Women_Forum_Posts$index<-c(1:nrow(Women_Forum_Posts))
Women_Forum_Posts2<-Women_Forum_Posts[-c(which(Women_Forum_Posts$date=="1960-01-01")),]
mystopwords<-c(stopwords("en"),"amp","pic.twitter.com","http", "Quote", "quote","wn", LETTERS, letters)

Women_Forum_Posts2$text<-gsub("\\\\n"," ",Women_Forum_Posts2$text)
```
Start LDA
```{r}
mywomen <- corpus(Women_Forum_Posts2)
dfm_womennostem <- dfm(mywomen,tolower = TRUE, remove_punct = TRUE,remove_numbers=TRUE, 
                     remove = mystopwords, stem = FALSE,
                     remove_separators=TRUE, include_docvars=TRUE) 
docnames(dfm_womennostem)<-Women_Forum_Posts2$index

dfm_women2<-dfm_trim(dfm_womennostem, max_docfreq = 0.99, min_docfreq=0.001,docfreq_type="prop")
dtm_ldawomen <- convert(dfm_women2, to = "topicmodels")
```
Find optimal topic number
```{r}
mycores<-detectCores()-1

Sys.time()
result <- FindTopicsNumber(
  dtm_ldawomen,
  topics = seq(5,200,by=5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 3333), 
  mc.cores = mycores, 
  verbose = TRUE
)
Sys.time()

FindTopicsNumber_plot(result)  
save.image("Women Topic Models 4.13.19.RData")
```
Run LDA k=60
```{r}
k=60

dtm_ldawomen_60 <- LDA(dtm_ldawomen, 
                    k = 60, 
                    method = "Gibbs", 
                    control = list(verbose=500, 
                                   seed = 9898, 
                                   burnin = 1000,
                                   keep = 50,
                                   iter = 4000))
save.image("Women Topic Models 4.23.19.RData")

textcolnum=7

LDAwomenfit<-dtm_ldawomen_60
```
Extract top words
```{r}
mybeta<-data.frame(LDAwomenfit@beta)
colnames(mybeta)<-LDAwomenfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)

nwords=30

topwords <- mybeta[1:nwords,]
for (i in 1:LDAwomenfit@k) {
  tempframe <- mybeta[order(-mybeta[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}

rownames(topwords)<-c(1:nwords)

topwordswomen60<-data.frame(topwords)

nwords=1000

topicwords <- mybeta[1:nwords,]
for (i in 1:LDAwomenfit@k) {
  tempframe <- mybeta[order(-mybeta[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topicwords[,i]<-tempvec
}

rownames(topicwords)<-c(1:nwords)

topicwords60<-data.frame(topicwords)


WriteXLS("topwordswomen60", "Top Words Women 60.xlsx",
         SheetNames = NULL, perl="perl", 
         verbose = FALSE, Encoding = c("latin1"),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())
```
Extract top documents
```{r}
missing_docs<-setdiff(dfm_womennostem@Dimnames$docs,dtm_ldawomen_60@documents)

Women_short<-Women_Forum_Posts2
Women_short<-Women_short[-which( as.character(Women_short$index) %in% missing_docs),]
meta_theta_df<-cbind(Women_short[7],dtm_ldawomen_60@gamma)
ntext=40
toptexts2 <- mybeta[1:ntext,]

for (i in 1:LDAwomenfit@k) {
  tempframe2 <- meta_theta_df[order(-meta_theta_df[,i+1]),]
  tempframe2 <- tempframe2[1:ntext,]
  tempvec2<-as.vector(tempframe2[,1])
  toptexts2[,i]<-tempvec2
}

rownames(toptexts2)<-c(1:ntext)

toptexts2df<-data.frame(toptexts2)

WriteXLS("toptexts2df", "Top Texts k60 Gama.xlsx",
         SheetNames = NULL, perl="perl", 
         verbose = FALSE, Encoding = c("latin1"),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())
```
2. Women Dawah Data
Load the scraped data *data will not be provide publicly, please contact authors with any questions*
```{r}
WData
```
Preprocess the data
```{r}
mystopwords<-c(stopwords("en"),"amp","pic.twitter.com","http", "Quote", "quote","wn", LETTERS, letters)

colnames(WData)<-seq(1:ncol(WData))

WData$text<-gsub(mystopwords," ",WData$`1`)
WData$text<-gsub("Women Dawah"," ",WData$text)
WData$text<-removeNumbers(WData$text)
WData$index<-c(1:nrow(WData))
```
Start LDA and find the optimal topic number
```{r}
wcorpus<-corpus(WData)
dfm_w <- dfm(wcorpus,tolower = TRUE, remove_punct = TRUE,remove_numbers=TRUE, 
                       remove = mystopwords, stem = FALSE,
                       remove_separators=TRUE, include_docvars=TRUE)
docnames(dfm_w)<-WData$index
dtm_ldaw <- convert(dfm_w, to = "topicmodels")
mycores<-detectCores()-1
Sys.time()
result <- FindTopicsNumber(
  dtm_ldaw,
  topics = seq(5,200,by=5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 3333), 
  mc.cores = mycores, 
  verbose = TRUE
)
Sys.time()
FindTopicsNumber_plot(result)  
save.image("WD Topic Models 5.2.19.RData")
```
Run LDA for k=25
```{r}
k=25

dtm_lda_25 <- LDA(dtm_ldaw, 
                       k = 25, 
                       method = "Gibbs", 
                       control = list(verbose=500, 
                                      seed = 9898, 
                                      burnin = 1000,
                                      keep = 50,
                                      iter = 4000))
save.image("WD Topic Models 5.2.19.RData")
LDAwfit<-dtm_lda_35
```
Extract top words
```{r}
mybeta<-data.frame(LDAwfit@beta)
colnames(mybeta)<-LDAwfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)

nwords=30

topwords <- mybeta[1:nwords,]
for (i in 1:LDAwfit@k) {
  tempframe <- mybeta[order(-mybeta[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}

rownames(topwords)<-c(1:nwords)

topwordsw25<-data.frame(topwords)

WriteXLS("topwordsw25", "Top Words Dawah 25.xlsx",
         SheetNames = NULL, perl="perl",
         verbose = FALSE, Encoding = c("latin1"),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())
```
Extract top documents
```{r}
missing_docs<-setdiff(dfm_w@Dimnames$docs,dtm_lda_25@documents)

W_short<-WData
W_short<-W_short[-which( as.character(W_short$index) %in% missing_docs),]
meta_theta_df<-cbind(W_short[3],dtm_lda_25@gamma)
#colnames(meta_theta_df)
ntext=40

toptexts <- mybeta[1:ntext,]

for (i in 1:LDAwfit@k) {
  tempframe <- meta_theta_df[order(-meta_theta_df[,i+1]),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)

toptextsdf<-data.frame(toptexts)
WriteXLS("toptextsdf", "WD Top Texts k25 Gama 11.3.21.xlsx",
         SheetNames = NULL, perl="perl", 
         verbose = FALSE, Encoding = c("latin1"),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())

write.csv(toptextsdf, "WD Top Texts k35 Gama 11.3.21.csv")
```
