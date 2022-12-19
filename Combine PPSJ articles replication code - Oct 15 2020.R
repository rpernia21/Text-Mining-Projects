
#Combine PPSJ articles Replication####

rm(list=ls())

getwd()

setwd("~/R/PPSJ articles")

#install packages
library(tm)
library(NLP)
library(tidytext)
library(RColorBrewer)
library(readtext)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(lattice)
library(stringr)
library(dplyr) 
library(stopwords)

#combined articles
Combine <- readtext("Combine PPSJ articles/*.txt",
                    docvarsfrom = "filenames", 
                    docvarnames = NULL,
                    dvsep = "_")
str(Combine)
View(Combine)

#View data: select only 'text' column 
#CombineArt <-Combine$text
#View(CombineArt)

CombineArt2 <-data.frame(Combine$text, Combine$docvar1)
View(CombineArt2)

##data processing####
#Remove special characters
CombineArt2   <- gsub("[^0-9A-Za-z///' ]","'" , CombineArt2  ,ignore.case = TRUE)
CombineArt2   <- gsub("''","" , CombineArt2   ,ignore.case = TRUE)

#Create a corpus
CombineArt2_corpus <- iconv(CombineArt2, to = "UTF-8")
corpus <- Corpus(VectorSource(CombineArt2_corpus))

#Remove special characters again
#Post.MarcosM_corpus  <- gsub("[^0-9A-Za-z///' ]","'" , Post.MarcosM_corpus ,ignore.case = TRUE)
#Post.MarcosM_corpus<- gsub("''","" , Post.MarcosM_corpus ,ignore.case = TRUE)

# Data preprocessing: cleaning####
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

#create dictionary for "economy" and "economic"

economic.dictionary <-c("economic", "economy")
#corpus <- tm_map(corpus, stemCompletion = economic.dictionary)
stemCompletion(economic.dictionary, corpus, type = c("prevalent", "first", "longest",
                                                     "none", "random", "shortest"))

cleanset <-tm_map(corpus, removeWords, stopwords('English'))
cleanset <-tm_map(cleanset, removeWords, stopwords())

#List of all unnecessary words to be removed 
cleanset<-tm_map(cleanset,removeWords, c("ppsj","members","york","california",
                                         "journal","article","shall","also","one",
                                         "philippines","philippine","can","june",
                                         "may","university","san","december","however","must","politics","political","science",
                                         "diego","terms","use","must","politics",
                                         "first","like","states","san","new","even","downloaded",
                                         "two","well","ofthe","per","human","among","studies","study",
                                         "b","various","m","april","library","october","january",
                                         "march","february","quezon","given","office","used","countries",
                                         "nnfdownloaded", "issues", "three", "thenphilippines",
                                         "p", "s", "n", "us", "ofnthe","part", "many","number","within"))

cleanset.combine <- tm_map(cleanset, stripWhitespace)


#Process in quanteda####

corp_tm <- tm::VCorpus(tm::VectorSource(cleanset.combine))
corp_quanteda <- corpus(corp_tm)

require(quanteda)
options(width = 110)

corp_CombineArt <- corpus(corp_quanteda)
toks_CombineArt <- tokens(corp_CombineArt)

toks_nopunct <- tokens(toks_CombineArt, remove_punct = TRUE)
#print(toks_nopunct)

#kwic####

require(quanteda)
options(width = 110)

toks <- tokens(toks_CombineArt)

# for american
kwic_american <-kwic(toks, pattern = "americ*")
head(kwic_american, 10)

#kwic for word "citizen"
kw_CombineArt<- kwic(toks, pattern =  "citizen*")
head(kw_CombineArt, 50)
View(kw_CombineArt, 50)

#save into dataframe
citizen.csv <-data.frame(kw_CombineArt)

#output save in a csv table
write.table(citizen.csv, file="CombinePPSJcitizen.csv", sep=",")

#kw_CombineArt2 <- kwic(toks, pattern = c("citizen*", "government*"), window = 7)
#head(kw_CombineArt2, 10)


#phrase 
kw_CombineArt3 <- kwic(toks, pattern = phrase("citizen*"))
head(kw_CombineArt3, 50)
View(kw_CombineArt3, 50)


#kwic combine PPSJ articles####

require(quanteda)
options(width = 110)

toksCombineArticles <- tokens(toks_CombineArt)

#kwic : dictionary for "leaders"
kwic_leaders <-c("leader*", "president*", "elite*", "mayor*", "governor*", "politician*")

#kwic : dictionary for "leaders"
kw_Combine_art <- kwic(toksCombineArticles, pattern =  kwic_leaders)
head(kw_Combine_art, 50)


#kwic for "military"
kw_military <- kwic(toksCombineArticles, pattern = "military")
head(kw_military, 50)

#save into dataframe
leader.csv <-data.frame(kw_Combine_art)
View(leader.csv)

#output save in a csv table
write.table(leader.csv, file="CombinePPSJleader.csv", sep=",")


#save into dataframe
military.csv <-data.frame(kw_military)
View(military.csv)

#output save in a csv table
write.table(military.csv, file="CombinePPSJmilitary.csv", sep=",")


#run sentiment analysis####
#Sentiment analysis
library(sentimentr)
library(lexicon)

leader1 <- c(leader.csv$pre, leader.csv$post)
View(leader1)


leader.sent1 <-sentiment(leader1)
leader.sent1only <-leader.sent1$sentiment

#get_sentences(leader1)
leader.sent2 <-sentiment_by(leader1)
leader.sent2only <-leader.sent2$ave_sentiment

data1 <-data.frame(c(leader.csv,leader.sent2only))

#View(emotion(leader1))
#plot(emotion_by(leader1))

#plot(sentiment_by(leader1))
leader.sent3<-extract_sentiment_terms(leader1)
leader.negsent <-leader.sent3$negative
leader.neutralsent <-leader.sent3$neutral
leader.positivesent <-leader.sent3$positive
leader.positivesent_df <-data.frame(leader.positivesent)

leader.overall <-as.data.frame(c(leader.sent1only, leader.sent2only, leader.positivesent,
                              leader.negsent, leader.neutralsent))


#https://cran.r-project.org/web/packages/sentimentr/readme/README.html

#limit the number of words around the keyword, use window function

kw_military <- kwic(toksCombineArticles, pattern = phrase("military"))
head(kw_military, 50)
#for better view, use...
View(kw_military, 50)

#find multi-word expressions, separate words by whitespace and wrap the character vector by phrase().

kw_military <- kwic(toksCombineArticles, pattern = "military", window= 7)
head(kw_military, 50)


#kwic: dictionary for theory
kwic_theory <-c("theor*", "approach*", "method*", "analy*", "framework*")
kw_Combine_art_theory <-kwic(toksCombineArticles, pattern=kwic_theory)
head(kw_Combine_art_theory, 50)


#save into dataframe
theory.csv <-data.frame(kw_Combine_art_theory)
View(theory.csv)

#output save in a csv table####
write.table(leader.csv, file="CombinePPSJtheory.csv", sep=",")



#kw_CombineArt2 <- kwic(toks, pattern = c("citizen*", "government*"), window = 7)
#head(kw_CombineArt2, 10)

#phrase 
kw_CombineArt3 <- kwic(toks, pattern = phrase("citizen*"))
head(kw_CombineArt3, 10)



#dfm####

require(quanteda)
options(width = 110)

toks <- tokens(toks_CombineArt, remove_punct = TRUE)
dfmat_CombineArt <- dfm(toks_CombineArt)
print(dfmat_CombineArt)

dfmat_CombineArt <- toks_CombineArt %>% 
  tokens(remove_punct = TRUE) %>% 
  dfm()

ndoc(dfmat_CombineArt )
nfeat(dfmat_CombineArt )

head(docnames(dfmat_CombineArt), 20)
head(featnames(dfmat_CombineArt), 20)
head(rowSums(dfmat_CombineArt), 10)
head(colSums(dfmat_CombineArt), 10)

#most frequently occuring words#### 

topfeatures(dfmat_CombineArt, 10)
ppsj.top.words <-topfeatures(dfmat_CombineArt, 10)
#View(ppsj.top.words)


#output save in a csv table####
write.table(ppsj.top.words, file="ppsj.top.words.csv", sep=",")


#proportion/ weighting####

#dfmat_CombineArt_prop <- dfm_weight(dfmat_CombineArt, scheme  = "prop")
#View(dfmat_CombineArt_prop)

#Bar plot of mean values with 95% confidence intervals for mean####

#CombinePPSJtopics####

CombinePPSJtop_df <-topfeatures(dfmat_CombineArt, 10)

CombinePPSJtop_df <-data.frame(word = names(CombinePPSJtop_df), freq=CombinePPSJtop_df)
View(CombinePPSJtop_df )


#data visualization####
library(ggplot2)
ggplot2::ggplot(CombinePPSJtop_df, 
                aes(word, freq)) +
  stat_summary(fun.y=mean, geom="bar") +
  labs(title="Top 10 most occuring words in the combine PPSJ articles, 1974-2019", 
       x="Words", 
       y="Frequency") +
  stat_summary(fun.data=mean_cl_normal,
               geom="pointrange")



####finding correlation####

library(wordcloud)
library(RColorBrewer)
library(tm)
library(NLP)

#Creating the document term matrix for tokenization of corpus
a <-DocumentTermMatrix(cleanset.combine) #check the number of words appearing in rows


#finding frequent terms
findFreqTerms(a, lowfreq = 2000) #words that have been said at least 7 times (set the cut-off)
findFreqTerms(a, highfreq = 2000) #words that have been said below
#it can be stored in an object

#find associative terms
findAssocs(a,'government', .9) #the .5 is the cut-off percentage between 'will' and otehr words
#this means that we can associate these words together and create a narrative based on the association; to determine the mood or intention of the speaker
#another word
findAssocs(a,'state', .9)
#create meaningful discourse based on the pronouncements of the speaker-- on how to go about their policies/understand their policies and actions
#evaluate more words
findAssocs(a, c('government', 'development', 'state', 'local',
                'social', 'economic', 'policy', 'national',
                'power', 'people'), c(.9,.9,.9,.9,.9,.9,.9,.9,.9,.9)) #and figure out meaning

####construct a feature co-occurrence matrix (FCM)####

require(quanteda)
require(quanteda.corpora)

#install.packages("devtools") # get devtools to install quanteda.corpora
#devtools::install_github("quanteda/quanteda.corpora")

#library(devtools)
#devtools::install_github("quanteda/quanteda.corpora")
#require(quanteda) 
#require(quanteda.corpora)

#corp_news <- download('data_corpus_guardian')
#dfmat_news <- dfm(corp_news, remove = stopwords("en"), remove_punct = TRUE)
dfmat_news2 <- dfm(dfmat_CombineArt, remove = stopwords("en"), remove_punct = TRUE)

#dfmat_news <- dfm_remove(dfmat_news, pattern = c("*-time", "updated-*", "gmt", "bst", "|"))
dfmat_news2 <- dfm_remove(dfmat_CombineArt, pattern = c("*-time", "updated-*", "gmt", "bst", "|"))

#dfmat_news <- dfm_trim(dfmat_news, min_termfreq = 100)
dfmat_news2 <- dfm_trim(dfmat_CombineArt, min_termfreq = 100)

#topfeatures(dfmat_news)
topfeatures(dfmat_news2)
#nfeat(dfmat_news)
nfeat(dfmat_news2)
#fcmat_news <- fcm(dfmat_news)
fcmat_news2 <- fcm(dfmat_news2)
#dim(fcmat_news)
dim(fcmat_news2)

#topfeatures(fcmat_news)
topfeatures(fcmat_news2)

#feat <- names(topfeatures(fcmat_news, 50))
feat2 <- names(topfeatures(fcmat_news2, 50))

#fcmat_news_select <- fcm_select(fcmat_news, pattern = feat, selection = "keep")
fcmat_news_select2 <- fcm_select(fcmat_news2, pattern = feat2, selection = "keep")

#dim(fcmat_news_select)
dim(fcmat_news_select2)

#####network modelling####
#A FCM can be used to train word embedding models with the text2vec package, or to visualize a semantic network analysis with textplot_network().

#size <- log(colSums(dfm_select(dfmat_news, feat, selection = "keep")))
size2 <- log(colSums(dfm_select(dfmat_news2, feat2, selection = "keep")))


set.seed(144)
#textplot_network(fcmat_news_select, min_freq = 0.8, vertex_size = size / max(size) * 3)
textplot_network(fcmat_news_select2, min_freq = 0.8, vertex_size2 = size2 / max(size2) * 3)

View(size2)
#convert to dataframe and use igraph for visualization

topPPSJ_df <-data.frame(size2)
View(topPPSJ_df)


#output save in a csv table####
write.table(topPPSJ_df, file="topPPSJ_df.csv", sep=",")

#use igraph

library(statnet)
library(UserNetR)
library(igraph)
data(Moreno)