---
title: "everyday-gender-extr"
author: "adl"
date: "2022-10-29"
output: html_document
---
```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
knitr::opts_chunk$set(eval = FALSE)
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
Import libraries
```{r}
library(stringi) 
library(stringr)
library(qdap)
library(tm)
library(ggplot2)
library(lubridate)
library(irr)
library(quanteda)
library(ldatuning)
library(topicmodels)
library(textcat)
library(parallel)
library(RSQLite)
library(doParallel)
library(scales)
library(lsa)
library(igraph)
library(cld2) 
library(tidyverse)
library(tidytext)
library(dplyr)
library(rgexf)
library(openxlsx)
library(ggthemes)
```

### Stormfront Data

1. Load the data *Due to the extremist content within the data, we will not be providing the data. Please contact the authors for any questions*
```{r}
Women_Forum_Posts
```
2. Tokenize
```{r}
toks <- tokens(Women_Forum_Posts$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) %>%
  tokens_remove(c(stopwords("en"),
                  "amp",
                  "pic.twitter.com",
                  "http", 
                  "Quote", 
                  "quote",
                  "wn")) %>%
  tokens_select(min_nchar = 2)
```
3. Change it into a [document-feature matrix](https://quanteda.io/reference/dfm.html)
```{r}
dfm_counts<- dfm(toks) 
rm(toks) #remove unused files to save space
```
4. Match your dfm object with your original data frame through index
```{r}
docnames(dfm_counts)<-Women_Forum_Posts$index
```
5. Check for sparsity and trim accordingly
```{r}
sparsity(dfm_counts)
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.95, min_docfreq=0.05,docfreq_type="prop")
sparsity(dfm_counts2)
rm(dfm_counts) #remove for space
```
6. Convert dfm object to an LDA object
```{r}
dtm_lda <- convert(dfm_counts2, to = "topicmodels",docvars = dfm_counts2@docvars)
full_data<-dtm_lda
n <- nrow(full_data) #number of rows for cross-validation method
rm(dfm_counts2) #remove for space
```
7. Run the cross-validation, save the results. The method is from the [supplemental code](https://github.com/aysedeniz09/AJPH2020/blob/master/AJPH%20GITupload.R), citation: Walter, D., Ophir, Y., & Jamieson, K. H. (2020). Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015–2017. American Journal of Public Health, 110(5), 718–724. <https://doi.org/10.2105/AJPH.2019.305564>.
```{r}
print(Sys.time())
# create container for results
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
# set possible alpha and k values
candidate_alpha<- c(0.01, 0.05, 0.1, 0.2, 0.5) # candidates for alpha values
candidate_k <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
# run the 10-fold cross validation
for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  cluster <- makeCluster(detectCores(logical = TRUE) - 1) # We are leaving one Core spare. If number of corse on pc is 1, then -1 in this line should be removed.
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  folds <- 10
  splitfolds <- sample(1:folds, n, replace = TRUE)
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      control = list(alpha=eachalpha) )
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}
print ("DONE!")
print(Sys.time())
save(MainresultDF, file="Main_Results_DF_DATE.Rda")
```
8. Examine the output by visualizing
```{r}
MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

ggplot(MainresultDF)+geom_line(aes(x=k, y=mean(perplexity),color=myalpha))
ggplot(MainresultDF)+geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha))
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_smooth(se = TRUE, aes(x=k, y=perplexity,color=myalpha))


p<-ggplot(MainresultDF, aes(x = k, y = perplexity))
Alpha<-MainresultDF$myalpha
p+geom_point(aes(color=Alpha),size=0.1)+geom_smooth(se = FALSE, aes(color=Alpha))+
  ggtitle("5-fold cross-validation of topic modelling (5% of data)",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  scale_color_discrete(name = "Alpha Levels")+
  xlab("K (Number of Topics)")+
  ylab("Perplexity")
b<-ggplot(MainresultDF) +
  geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha)) +  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  xlab("Topics (k)")+
  ylab("Perplexity")+
  theme_wsj(color="white") +
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="top", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
```
9. Choose the alpha and test approximate topic numbers, in our case it was alpha=0.1 and k=60
```{r}
# Identify 2nd derivative max point on perplexity  
MainresultDF_MYALPHA<-MainresultDF[MainresultDF$myalpha==0.1,]
cars.spl <- with(MainresultDF_MYALPHA, smooth.spline(k, perplexity, df = 3))
plot(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)), type = "l",
     #main = "My title",
     #sub = "My subtitle",
     xlab = "Topics (k)",
     ylab = "Perplexity Second Derivative") + abline(v=60)
data<-data.frame(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)))

a<- ggplot(data,aes(x=x,y=y))+
  geom_line(color = "grey11")+
  geom_vline(xintercept=60) +
  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  ggtitle(" ")+
  xlab("Topics (k)")+
  ylab("Perplexity Second Derivative")+
  theme_wsj(color="white")+
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="bottom", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
ggpubr::ggarrange(b, a, ncol = 1, nrow = 2)
```
10. Run the topic model for the identified k and alpha
```{r}
Sys.time()
lda.60.1 <- LDA(full_data, 
                 k = 60, 
                 method = "Gibbs",
                 control = list(alpha=0.1,seed=9512))

save(lda.60.1, file="Stormfront_Women_lda_DATE.Rda")
Sys.time()
beepr::beep()

LDAfit<-lda.60.1 #copy the object it with a different name for backup
```
11. Extract top words from the topic model
```{r}
#mark text column
datacolnum=which( colnames(Women_Forum_Posts)=="text")

myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=100
topwords <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
topwords<-data.frame(topwords)
openxlsx::write.xlsx(topwords, file="Stormfront_Women_topwords_DATE.xlsx")
```
12. Extract top articles
```{r}
metadf<-mydata #your original dataframe
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)
toptexts<-data.frame(toptexts)
openxlsx::write.xlsx(toptexts, file="Stormfront_Women_topwords_toptexts_DATE.xlsx")
```
13. Save the complete file with topic loadings and all information
```{r}
meta_theta_df3<-cbind(metadf,LDAfit@gamma)
first<-which(colnames(meta_theta_df3)=="1")
last<-ncol(meta_theta_df3)
colnames(meta_theta_df3)[first:last] <- paste("X", colnames(meta_theta_df3[,c(first:last)]), sep = "_")
save(meta_theta_df3, file="Stormfront_Women_topwords_MetaThetaDF_DATE.Rda")
```

### Women Dawah Data
1. Load the scraped data *data will not be provide publicly, please contact authors with any questions*
```{r}
WData
```
2. Tokenize
```{r}
toks <- tokens(WData$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) %>%
  tokens_remove(c(stopwords("tr"),
                  "amp",
                  "pic.twitter.com",
                  "http", 
                  "Quote", 
                  "quote",
                  "wn")) %>%
  tokens_select(min_nchar = 2)
```
3. Change it into a [document-feature matrix](https://quanteda.io/reference/dfm.html)
```{r}
dfm_counts<- dfm(toks) 
rm(toks) #remove unused files to save space
```
4. Match your dfm object with your original data frame through index
```{r}
docnames(dfm_counts)<-WData$index
```
5. Check for sparsity and trim accordingly
```{r}
sparsity(dfm_counts)
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.95, min_docfreq=0.05,docfreq_type="prop")
sparsity(dfm_counts2)
rm(dfm_counts) #remove for space
```
6. Convert dfm object to an LDA object
```{r}
dtm_lda <- convert(dfm_counts2, to = "topicmodels",docvars = dfm_counts2@docvars)
full_data<-dtm_lda
n <- nrow(full_data) #number of rows for cross-validation method
rm(dfm_counts2) #remove for space
```
7. Run the cross-validation, save the results. The method is from the [supplemental code](https://github.com/aysedeniz09/AJPH2020/blob/master/AJPH%20GITupload.R), citation: Walter, D., Ophir, Y., & Jamieson, K. H. (2020). Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015–2017. American Journal of Public Health, 110(5), 718–724. <https://doi.org/10.2105/AJPH.2019.305564>.
```{r}
print(Sys.time())
# create container for results
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
# set possible alpha and k values
candidate_alpha<- c(0.01, 0.05, 0.1, 0.2, 0.5) # candidates for alpha values
candidate_k <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
# run the 10-fold cross validation
for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  cluster <- makeCluster(detectCores(logical = TRUE) - 1) # We are leaving one Core spare. If number of corse on pc is 1, then -1 in this line should be removed.
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  folds <- 10
  splitfolds <- sample(1:folds, n, replace = TRUE)
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      control = list(alpha=eachalpha) )
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}
print ("DONE!")
print(Sys.time())
save(MainresultDF, file="Main_Results_DF_WD_DATE.Rda")
```
8. Examine the output by visualizing
```{r}
MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

ggplot(MainresultDF)+geom_line(aes(x=k, y=mean(perplexity),color=myalpha))
ggplot(MainresultDF)+geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha))
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_smooth(se = TRUE, aes(x=k, y=perplexity,color=myalpha))


p<-ggplot(MainresultDF, aes(x = k, y = perplexity))
Alpha<-MainresultDF$myalpha
p+geom_point(aes(color=Alpha),size=0.1)+geom_smooth(se = FALSE, aes(color=Alpha))+
  ggtitle("5-fold cross-validation of topic modelling (5% of data)",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  scale_color_discrete(name = "Alpha Levels")+
  xlab("K (Number of Topics)")+
  ylab("Perplexity")
b<-ggplot(MainresultDF) +
  geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha)) +  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  xlab("Topics (k)")+
  ylab("Perplexity")+
  theme_wsj(color="white") +
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="top", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
```
9. Choose the alpha and test approximate topic numbers, in our case it was alpha=0.1 and k=25
```{r}
# Identify 2nd derivative max point on perplexity  
MainresultDF_MYALPHA<-MainresultDF[MainresultDF$myalpha==0.1,]
cars.spl <- with(MainresultDF_MYALPHA, smooth.spline(k, perplexity, df = 3))
plot(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)), type = "l",
     #main = "My title",
     #sub = "My subtitle",
     xlab = "Topics (k)",
     ylab = "Perplexity Second Derivative") + abline(v=25)
data<-data.frame(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)))

a<- ggplot(data,aes(x=x,y=y))+
  geom_line(color = "grey11")+
  geom_vline(xintercept=25) +
  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  ggtitle(" ")+
  xlab("Topics (k)")+
  ylab("Perplexity Second Derivative")+
  theme_wsj(color="white")+
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="bottom", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
ggpubr::ggarrange(b, a, ncol = 1, nrow = 2)
```
10. Run the topic model for the identified k and alpha
```{r}
Sys.time()
lda.25.1 <- LDA(full_data, 
                 k = 25, 
                 method = "Gibbs",
                 control = list(alpha=0.1,seed=9512))

save(lda.25.1, file="WD_Women_lda_DATE.Rda")
Sys.time()
beepr::beep()

LDAfit<-lda.25.1 #copy the object it with a different name for backup
```
11. Extract top words from the topic model
```{r}
#mark text column
datacolnum=which( colnames(WData)=="text")

myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=100
topwords <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
topwords<-data.frame(topwords)
openxlsx::write.xlsx(topwords, file="WD_Women_topwords_DATE.xlsx")
```
12. Extract top articles
```{r}
metadf<-mydata #your original dataframe
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)
toptexts<-data.frame(toptexts)
openxlsx::write.xlsx(toptexts, file="WD_Women_topwords_toptexts_DATE.xlsx")
```
13. Save the complete file with topic loadings and all information
```{r}
meta_theta_df3<-cbind(metadf,LDAfit@gamma)
first<-which(colnames(meta_theta_df3)=="1")
last<-ncol(meta_theta_df3)
colnames(meta_theta_df3)[first:last] <- paste("X", colnames(meta_theta_df3[,c(first:last)]), sep = "_")
save(meta_theta_df3, file="WD_Women_topwords_MetaThetaDF_DATE.Rda")
```

