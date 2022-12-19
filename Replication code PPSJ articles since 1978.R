#Replication Code, PPSJ articles since 1978

#clean environment
rm(list=ls())

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

#Marcos Period####

#set directory
setwd("~/R/PPSJ articles")

#Data import
MarcosPer <- readtext("MLTime_all_txt/*.txt",
                         docvarsfrom = "filenames", 
                         docvarnames = NULL,
                         dvsep = "_")

#View data: select only 'text' column 
Marcos.Per <-MarcosPer$text

#Remove special characters
Marcos.Per <- gsub("[^0-9A-Za-z///' ]","'" , Marcos.Per ,ignore.case = TRUE)
Marcos.Per <- gsub("''","" , Marcos.Per ,ignore.case = TRUE)

#Create a corpus
Marcos.Per_corpus <- iconv(Marcos.Per, to = "UTF-8")
corpus2 <- Corpus(VectorSource(Marcos.Per_corpus))

#Remove special characters again
Marcos.Per_corpus  <- gsub("[^0-9A-Za-z///' ]","'" , Marcos.Per_corpus ,ignore.case = TRUE)
Marcos.Per_corpus<- gsub("''","" , Marcos.Per_corpus ,ignore.case = TRUE)

# Data preprocessing: cleaning
corpus.marcos <- tm_map(corpus2, tolower)
corpus.marcos <- tm_map(corpus.marcos, removePunctuation)
corpus.marcos <- tm_map(corpus.marcos, removeNumbers)
#corpus <- tm_map(corpus, stemDocument)
cleanset.marcos <-tm_map(corpus.marcos, removeWords, stopwords('English'))
cleanset.marcos <-tm_map(cleanset.marcos, removeWords, stopwords())

#List of all unnecessary words to be removed 
cleanset.marcos<-tm_map(cleanset.marcos,removeWords, c("ppsj","members","york","california",
                                         "journal","article","shall","also","one",
                                         "philippines","philippine","can","june",
                                         "may","university","san","december","however","must","politics","political","science",
                                         "diego","terms","use","must","politics",
                                         "first","like","states","san","new","even","downloaded",
                                         "two","well","ofthe","per","human","among","studies","study",
                                         "b","various","m","april","library","october","january",
                                         "march","february","quezon","given","office","used","countries",
                                         "pp","p","years","year","within","many","ldc","filipino",
                                         "still","cent"))

cleanset.marcos <- tm_map(cleanset.marcos, stripWhitespace)


cleanset.marcos

#Process in quanteda####

corp.marcos_tm <- tm::VCorpus(tm::VectorSource(cleanset.marcos))
corp_marcos.quanteda <- corpus(corp.marcos_tm)

require(quanteda)
options(width = 110)

corp_marcos.period <- corpus(corp_marcos.quanteda)
toks_marcos.period <- tokens(corp_marcos.period)

toks_nopunct <- tokens(toks_marcos.period, remove_punct = TRUE)
print(toks_nopunct)

#kwic####

require(quanteda)
options(width = 110)

toks2 <- tokens(toks_marcos.period)

#kwic for word "development"
kw_marcos.period <- kwic(toks2, pattern =  "development*")
head(kw_marcos.period, 10)

kw_marcos.period2 <- kwic(toks2, pattern = c("development*", "government*"))
head(kw_marcos.period2, 10)

kw_marcos.period3 <- kwic(toks2, pattern = c("development*", "government*"), window = 7)
head(kw_marcos.period3, 10)

#phrase
kw_marcos.period.natdev <- kwic(toks2, pattern = phrase("national development*"))
head(kw_marcos.period.natdev)

#interactive HTML table
View(kw_marcos.period.natdev)

#select tokens, remove unnecessary words####

require(quanteda)
options(width = 110)

toks2 <- tokens(toks_marcos.period)
toks2_nostop <- tokens_select(toks2, pattern = stopwords("en"), selection = "remove")
print(toks2_nostop)

#padding
toks2_nostop_pad <- tokens_remove(toks2, pattern = stopwords("en"), padding = TRUE)
print(toks2_nostop_pad)

toks2_natdev <- tokens_select(toks2, pattern = c("national*", "development*"), padding = TRUE)
print(toks2_natdev)

#window

toks2_natdev_window <- tokens_select(toks2, pattern = c("national*", "development*"), padding = TRUE, window = 5)
print(toks2_natdev_window)


#compound tokens

require(quanteda)
options(width = 110)

toks2 <- tokens(toks_marcos.period)

kw_multiword <- kwic(toks2, pattern = phrase(c("government*", "power*")))
head(kw_multiword, 10)

#preserve in bag of words

toks_comp2 <- tokens_compound(toks2, pattern = phrase(c("government*", "power*")))
kw_comp2 <- kwic(toks_comp2, pattern = c("government*", "power*"))
head(kw_comp2, 10)



#generate n-grams####

require(quanteda)
options(width = 110)

toks2 <- tokens(toks_marcos.period, remove_punct = TRUE)

toks2_ngram <- tokens_ngrams(toks2, n = 2:4)
head(toks2_ngram[[1]], 30)
tail(toks2_ngram[[1]], 30)
toks_skip <- tokens_ngrams(toks2, n = 2, skip = 1:2)
head(toks_skip[[1]], 30)

#toks_neg_bigram <- tokens_compound(toks, pattern = phrase("not *"))
#toks_neg_bigram_select <- tokens_select(toks_neg_bigram, pattern = phrase("not_*"))
#head(toks_neg_bigram_select[[1]], 30)


#dfm####

require(quanteda)
options(width = 110)

toks2_marcos.period <- tokens(toks_marcos.period, remove_punct = TRUE)
dfmat_marcos.period <- dfm(toks2_marcos.period)
print(dfmat_marcos.period)

dfmat_marcos.period <- toks2_marcos.period %>% 
  tokens(remove_punct = TRUE) %>% 
  dfm()

ndoc(dfmat_marcos.period)
nfeat(dfmat_marcos.period)

head(docnames(dfmat_marcos.period), 20)
head(featnames(dfmat_marcos.period), 20)
head(rowSums(dfmat_marcos.period), 10)
head(colSums(dfmat_marcos.period), 10)

#most frequently occuring words#### 
topfeatures(dfmat_marcos.period, 10)


#proportion/ weighting####

dfmat_marcos.period_prop <- dfm_weight(dfmat_marcos.period, scheme  = "prop")
print(dfmat_marcos.period_prop)
topfeatures(dfmat_marcos.period_prop)

#tfidf####

dfmat_marcos.period_tfidf <- dfm_tfidf(dfmat_marcos.period)
print(dfmat_marcos.period_tfidf)
topfeatures(dfmat_marcos.period_tfidf)

####construct a feature co-occurrence matrix (FCM)####

require(quanteda)
require(quanteda.corpora)

#install.packages("devtools") # get devtools to install quanteda.corpora
#devtools::install_github("quanteda/quanteda.corpora")

#library(devtools)
#devtools::install_github("quanteda/quanteda.corpora")
#require(quanteda) 
#require(quanteda.corpora)

dfmat_news1 <- dfm(dfmat_marcos.period, remove = stopwords("en"), remove_punct = TRUE)
dfmat_news1 <- dfm_remove(dfmat_marcos.period, pattern = c("*-time", "updated-*", "gmt", "bst", "|"))
dfmat_news1 <- dfm_trim(dfmat_marcos.period, min_termfreq = 100)

topfeatures(dfmat_news1)
nfeat(dfmat_news1)
fcmat_news1 <- fcm(dfmat_news1)
dim(fcmat_news1)

topfeatures(fcmat_news1)
feat1 <- names(topfeatures(fcmat_news1, 50))
fcmat_news_select1 <- fcm_select(fcmat_news1, pattern = feat1, selection = "keep")
dim(fcmat_news_select1)

#####network modelling####
size1 <- log(colSums(dfm_select(dfmat_news1, feat1, selection = "keep")))
set.seed(144)
textplot_network(fcmat_news_select1, min_freq = 0.8, vertex_size1 = size1 / max(size1) * 3)
textplot_network(fcmat_news_select1, min_freq = 0.7, vertex_size1 = size1 / max(size1) * 5)



####Post.Marcos####

#clean environment
rm(list=ls())

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
library(quanteda)

#set directory
setwd("~/R/PPSJ articles")

#Data import: all .txt files containing 199 observations
Post.MarcosM <- readtext("Post.Marcos_merge_text/*.txt",
                 docvarsfrom = "filenames", 
                 docvarnames = NULL,
                 dvsep = "_")

#View data: select only 'text' column 
Post.MarcosM <-Post.MarcosM$text

##data processing####
#Remove special characters
Post.MarcosM  <- gsub("[^0-9A-Za-z///' ]","'" , Post.MarcosM  ,ignore.case = TRUE)
Post.MarcosM  <- gsub("''","" , Post.MarcosM  ,ignore.case = TRUE)

#Create a corpus
Post.MarcosM_corpus <- iconv(Post.MarcosM, to = "UTF-8")
corpus <- Corpus(VectorSource(Post.MarcosM_corpus))

#Remove special characters again
Post.MarcosM_corpus  <- gsub("[^0-9A-Za-z///' ]","'" , Post.MarcosM_corpus ,ignore.case = TRUE)
Post.MarcosM_corpus<- gsub("''","" , Post.MarcosM_corpus ,ignore.case = TRUE)

# Data preprocessing: cleaning####
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, stemDocument)
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
                                         "march","february","quezon","given","office","used","countries"))

cleanset <- tm_map(cleanset, stripWhitespace)


#Process in quanteda####

corp_tm <- tm::VCorpus(tm::VectorSource(cleanset))
corp_quanteda <- corpus(corp_tm)

require(quanteda)
options(width = 110)

corp_ppsjmarcos <- corpus(corp_quanteda)
toks_ppsjmarcos <- tokens(corp_ppsjmarcos)

toks_nopunct <- tokens(toks_ppsjmarcos, remove_punct = TRUE)
print(toks_nopunct)

#kwic####

require(quanteda)
options(width = 110)

toks <- tokens(toks_ppsjmarcos)

#kwic for word "development"
kw_ppsjmarcos <- kwic(toks, pattern =  "development*")
head(kw_ppsjmarcos, 10)

kw_ppsjmarcos2 <- kwic(toks, pattern = c("development*", "government*"))
head(kw_ppsjmarcos2, 10)

kw_ppsjmarcos3 <- kwic(toks, pattern = c("development*", "government*"), window = 7)
head(kw_ppsjmarcos, 10)

#phrase
kw_natdev <- kwic(toks, pattern = phrase("national development*"))
head(kw_natdev)

#interactive HTML table
View(kw_natdev)

#select tokens, remove unnecessary words

require(quanteda)
options(width = 110)

toks <- tokens(toks_ppsjmarcos)
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")
print(toks_nostop)

#alternative
toks_nostop2 <- tokens_remove(toks, pattern = stopwords("en"))
print(toks_nostop2)

#padding
toks_nostop_pad <- tokens_remove(toks, pattern = stopwords("en"), padding = TRUE)
print(toks_nostop_pad)

toks_natdev <- tokens_select(toks, pattern = c("national*", "development*"), padding = TRUE)
print(toks_natdev)

#window

toks_natdev_window <- tokens_select(toks, pattern = c("national*", "development*"), padding = TRUE, window = 5)
print(toks_natdev_window)


#compound tokens

require(quanteda)
options(width = 110)

toks <- tokens(toks_ppsjmarcos)

kw_multiword <- kwic(toks, pattern = phrase(c("women*", "power*")))
head(kw_multiword, 10)

#preserve in bag of words

toks_comp <- tokens_compound(toks, pattern = phrase(c("women*", "power*")))
kw_comp <- kwic(toks_comp, pattern = c("women*", "power*"))
head(kw_comp, 10)


#look up dictionary

require(quanteda)
options(width = 110)
require(newsmap)

toks <- tokens(toks_ppsjmarcos)

dict_newsmap <- dictionary(file = "../../dictionary/newsmap.yml")
length(dict_newsmap)
names(dict_newsmap)
names(dict_newsmap[["AFRICA"]])
dict_newsmap[["AFRICA"]][["NORTH"]]

# use level of continents
toks_region <- tokens_lookup(toks, dictionary = dict_newsmap, levels = 1)
print(toks_region)

# use level of countries
toks_country <- tokens_lookup(toks, dictionary = dict_newsmap, levels = 3)
print(toks_country)

kwic(toks, dict_newsmap["AFRICA"])

#define your dictionary
dict <- dictionary(list(refugee = c("refugee*", "asylum*"),
                        worker = c("worker*", "employee*")))
print(dict)

dict_toks <- tokens_lookup(toks, dictionary = dict)
print(dict_toks)

dfm(dict_toks)



#generate n-grams####

require(quanteda)
options(width = 110)

toks <- tokens(toks_ppsjmarcos, remove_punct = TRUE)

toks_ngram <- tokens_ngrams(toks, n = 2:4)
head(toks_ngram[[1]], 30)

tail(toks_ngram[[1]], 30)

toks_skip <- tokens_ngrams(toks, n = 2, skip = 1:2)
head(toks_skip[[1]], 30)

toks_neg_bigram <- tokens_compound(toks, pattern = phrase("not *"))
toks_neg_bigram_select <- tokens_select(toks_neg_bigram, pattern = phrase("not_*"))
head(toks_neg_bigram_select[[1]], 30)


#dfm####

require(quanteda)
options(width = 110)

toks_ppsjmarcos <- tokens(toks_ppsjmarcos, remove_punct = TRUE)
dfmat_ppsjmarcos <- dfm(toks_ppsjmarcos)
print(dfmat_ppsjmarcos)

dfmat_ppsjmarcos <- toks_ppsjmarcos %>% 
  tokens(remove_punct = TRUE) %>% 
  dfm()

ndoc(dfmat_ppsjmarcos)
nfeat(dfmat_ppsjmarcos)

head(docnames(dfmat_ppsjmarcos), 20)
head(featnames(dfmat_ppsjmarcos), 20)
head(rowSums(dfmat_ppsjmarcos), 10)
head(colSums(dfmat_ppsjmarcos), 10)

#most frequently occuring words#### 

topfeatures(dfmat_ppsjmarcos, 10)
#ppsj.top.words <-topfeatures(dfmat_ppsjmarcos, 10)
#View(ppsj.top.words)

#proportion/ weighting####

dfmat_ppsjmarcos_prop <- dfm_weight(dfmat_ppsjmarcos, scheme  = "prop")
print(dfmat_ppsjmarcos_prop)

#tfidf####

dfmat_ppsjmarcos_tfidf <- dfm_tfidf(dfmat_ppsjmarcos)
print(dfmat_ppsjmarcos_tfidf)


###select features

require(quanteda)
options(width = 110)

dfmat_inaug <- dfm(data_corpus_inaugural, remove_punct = TRUE)
print(dfmat_inaug)

dfmat_inaug_nostop <- dfm_select(dfmat_inaug, pattern = stopwords("en"), selection = "remove")
print(dfmat_inaug_nostop)

dfmat_inaug_nostop <- dfm_remove(dfmat_inaug, pattern = stopwords("en"))
print(dfmat_inaug_nostop)

dfmat_inaug_long <- dfm_keep(dfmat_inaug, min_nchar = 5)
print(dfmat_inaug_long)

topfeatures(dfmat_inaug_long, 10)

dfmat_inaug_freq <- dfm_trim(dfmat_inaug, min_termfreq = 10)
print(dfmat_inaug_freq)

dfmat_inaug_docfreq <- dfm_trim(dfmat_inaug, max_docfreq = 0.1, docfreq_type = "prop")
print(dfmat_inaug_docfreq)

####look up dictionary

require(quanteda)
require(quanteda.textmodels)
options(width = 110)

dict_lg <- dictionary(file = "../../dictionary/laver-garry.cat", encoding = "UTF-8")

## Warning in stri_read_lines(path, encoding = encoding, fallback_encoding = "windows-1252"): `fallback_encoding`
## is no longer used and has been scheduled for removal

toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish)
print(dfmat_irish)

dfmat_irish_lg <- dfm_lookup(dfmat_irish, dictionary = dict_lg, levels = 1)
print(dfmat_irish_lg)

dfmat_irish_lg <- dfm(data_corpus_irishbudget2010, dictionary = dict_lg, remove_punct = TRUE)

####Group documents

require(quanteda)
options(width = 110)

dfmat_inaug <- dfm(data_corpus_inaugural)
print(dfmat_inaug)

head(colSums(dfmat_inaug), 10)
dfmat_party <- dfm_group(dfmat_inaug, groups = "Party")
print(dfmat_party)

head(colSums(dfmat_party), 10)
docvars(dfmat_party)
dfmat_party <- dfm(data_corpus_inaugural, groups = "Party")
ndoc(dfmat_party)

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
dfmat_news2 <- dfm(dfmat_ppsjmarcos, remove = stopwords("en"), remove_punct = TRUE)

#dfmat_news <- dfm_remove(dfmat_news, pattern = c("*-time", "updated-*", "gmt", "bst", "|"))
dfmat_news2 <- dfm_remove(dfmat_ppsjmarcos, pattern = c("*-time", "updated-*", "gmt", "bst", "|"))

#dfmat_news <- dfm_trim(dfmat_news, min_termfreq = 100)
dfmat_news2 <- dfm_trim(dfmat_ppsjmarcos, min_termfreq = 100)

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





#data visualization####
#marcos period
a<-topfeatures(dfmat_marcos.period, 10)

aa <-data.frame(word = names(a), freq=a)
View(aa)

#Bar plot of mean values
ggplot2::ggplot(aa, 
                aes(word, freq)) +
  stat_summary(fun.y=mean, geom="bar") +
  labs(title="Top 10 most occuring words in PPSJ during Martial Law era, 1978-1982", 
       x="Words", 
       y="Frequency")


#Bar plot of mean values with 95% confidence intervals for mean
ggplot2::ggplot(aa, 
                aes(word, freq)) +
  stat_summary(fun.y=mean, geom="bar") +
  labs(title="Top 10 most occuring words in the PPSJ during Martial Law era, 1978-1982", 
       x="Words", 
       y="Frequency") +
  stat_summary(fun.data=mean_cl_normal,
               geom="pointrange")

#post marcos

b <-topfeatures(dfmat_ppsjmarcos, 10)


bb <-data.frame(word = names(b), freq=b)
View(bb)

#Bar plot of mean values
ggplot2::ggplot(bb, 
                aes(word, freq)) +
  stat_summary(fun.y=mean, geom="bar") +
  labs(title="Top 10 most occuring words in the PPSJ after Martial Law era, 1989-2019", 
       x="Words", 
       y="Frequency")


#Bar plot of mean values with 95% confidence intervals for mean
ggplot2::ggplot(bb, 
                aes(word, freq)) +
  stat_summary(fun.y=mean, geom="bar") +
  labs(title="Top 10 most occuring words in the PPSJ after Martial Law era, 1989-2019", 
       x="Words", 
       y="Frequency") +
  stat_summary(fun.data=mean_cl_normal,
               geom="pointrange")


#Frequency count####

require(quanteda)
require(quanteda.corpora)
require(ggplot2)

corp_tweets <- download(url = "https://www.dropbox.com/s/846skn1i5elbnd2/data_corpus_sampletweets.rds?dl=1")

toks_tweets <- tokens(Post.MarcosM_corpus, remove_punct = TRUE) 
dfmat_tweets <- dfm(toks_tweets, select = "#*")
tstat_freq <- textstat_frequency(dfmat_tweets, n = 5, groups = "lang")
head(tstat_freq, 20)

dfmat_tweets %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

set.seed(132)
textplot_wordcloud(dfmat_tweets, max_words = 100)



dfmat_marcos.period %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# create document-level variable indicating whether tweet was in English or other language
corp_tweets$dummy_english <- factor(ifelse(corp_tweets$lang == "English", "English", "Not English"))

# create a grouped dfm and compare groups
dfmat_corp_language <- dfm(corp_tweets, select = "#*", groups = "dummy_english")

# create wor cloud
set.seed(132)
textplot_wordcloud(dfmat_corp_language, comparison = TRUE, max_words = 200)

#lexical diversity####

toks_inaug <- tokens(Post.MarcosM_corpus)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords("en"))
tstat_lexdiv <- textstat_lexdiv(dfmat_inaug)
tail(tstat_lexdiv, 5)

#View(dfmat_inaug)

plot(tstat_lexdiv$TTR, type = "l", xaxt = "n", xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = dfmat_inaug$President)

#document/feature similarity

require(quanteda)

toks_inaug <- tokens(Post.MarcosM_corpus)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords("en"))
tstat_dist <- as.dist(textstat_dist(dfmat_inaug))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)


#relative frequency analysis (keyness)

require(quanteda)
require(quanteda.corpora)
require(lubridate)

corp_news <- download("data_corpus_guardian")


toks_news <- tokens(Post.MarcosM_corpus, remove_punct = TRUE) 
dfmat_news <- dfm(toks_news)

tstat_key <- textstat_keyness(dfmat_news, 
                              target = year(dfmat_news$date) >= 1989)
textplot_keyness(tstat_key)

??textstat_keyness


#collocation analysis

#explore td-idf

dfmat_ppsjmarcos

sherlock_tf_idf <- dfmat_ppsjmarcos %>%
  count(story, word, sort = TRUE) %>%
  bind_tf_idf(word, story, n) %>%
  arrange(-tf_idf) %>%
  group_by(story) %>%
  top_n(10) %>%
  ungroup


sherlock_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, story)) %>%
  ggplot(aes(word, tf_idf, fill = story)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ story, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Sherlock Holmes short stories",
       subtitle = "Individual stories focus on different characters and narrative elements")



#Combine PPSJ articles####

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
                                         "nnfdownloaded", "issues", "three", "thenphilippines"))

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

sentiment(CombineArt2)
sentiment_by(Antara2)
View(emotion(Antara2))
plot(emotion_by(Antara2))

plot(sentiment_by(Antara2))
extract_sentiment_terms(Antara2)
plot(sentiment(Antara2))


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

topfeatures(dfmat_CombineArt, 25)
#ppsj.top.words <-topfeatures(dfmat_CombineArt, 10)
#View(ppsj.top.words)

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




