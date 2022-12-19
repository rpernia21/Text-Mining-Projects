


setwd("C:/Users/proyekto_v2/Desktop/rprojects")
require(quanteda)
require(readtext)


path_data <- system.file("C:/Users/proyekto_v2/Desktop/rprojects")
dat_duterte <- readtext(paste0(path_data, "C:/Users/proyekto_v2/Desktop/rprojects/duterte2/*.txt"),
                        docvarsfrom = "filenames", 
                        docvarnames = c("year", "month", "day", "audience", "title"),
                        dvsep = "_", 
                        encoding = "ISO-8859-1")
str(dat_duterte)

require(lubridate)

date3 <- paste(dat_duterte$year, dat_duterte$month, dat_duterte$day, sep="-")
date2<-as.Date(date3)
dat_duterte$date <- date2
head(dat_duterte, 5)


dat_dut <- read.csv("dat_duterte2.csv")
dat_duterte$audience <- dat_dut$audience
dat_duterte$year <- dat_dut$year
dat_duterte$month <- dat_dut$month
dat_duterte$day <- dat_dut$day
dat_duterte$daysona <- dat_dut$daysona
dat_duterte$dawmarawi <- dat_dut$daymarawi
dat_duterte$fst_year <- dat_dut$fst_year

date4 <- date2 - as.Date("2016-07-01","%Y-%m-%d")
dat_duterte$caldays <- date4
View(dat_duterte)

corp_duterte <- corpus(dat_duterte)
summary(corp_duterte, 10)
d <- summary(corp_duterte, 731)

#targeted dictionary analysis

require(quanteda)
require(lubridate)

date3 <- paste(dat_duterte$year, dat_duterte$month, dat_duterte$day, sep="-")
date2<-as.Date(date3)
dat_duterte$date <- date2
head(dat_duterte, 5)

docvars(corp_duterte, 'year') <- year(docvars(corp_duterte, 'date'))
docvars(corp_duterte, 'month') <- month(docvars(corp_duterte, 'date'))
docvars(corp_duterte, 'day') <- day(docvars(corp_duterte, 'date'))

#option
corp_duterte <- corpus_subset(corp_news, 'year' >= 2016)


#basic sentiment analysis

toks_duterte <- tokens(corp_duterte, remove_punct = TRUE)
lengths(data_dictionary_LSD2015)
toks_duterte_lsd <- tokens_lookup(toks_duterte, dictionary =  data_dictionary_LSD2015[1:2])
head(toks_duterte_lsd, 2)
dfmat_duterte_lsd <- dfm(toks_duterte_lsd)
head(dfmat_duterte_lsd, 2)



#targeted analysis

economy <- c("appreciate", "bankrupt*", "business", "infrast*", "budget*", "capital*", "*employ*", "corporation", "competition", "cash", "consumer*", "cost*", "crash", "credit", "currency", "debt", "deficit", "deposit", "depression", "econom*", "financ*", "fiscal", "global", "inflation", "interest", "international", "invest*", "infrastructur*", "loan", "loss", "market*", "monetary", "money*", "loan*", "recession*", "savings", "spend*", "stock*", "tax", "revenue", "trade", "value", "labor*", "work", "wage*")
toks_econ <- tokens_keep(toks_duterte, pattern = phrase(economy), window = 10)
dfmat_econ_lsd <- dfm(toks_econ, dictionary = data_dictionary_LSD2015[1:2])
head(dfmat_econ_lsd, 731)
convert(dfmat_econ_lsd, to = "data.frame")
econ <- convert(dfmat_econ_lsd, to = "data.frame")

dfmat_econ_lsd <- dfm(toks_econ, dictionary = data_dictionary_LSD2015[1:2]) %>% 
  dfm_group(group = 'date', fill = TRUE) 

matplot(dfmat_econ_lsd, type = 'l', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
axis(1, seq_len(ndoc(dfmat_econ_lsd)), ymd("2016-06-30") + date(seq_len(ndoc(dfmat_econ_lsd)) - 1))
legend('topleft', col = 1:2, legend = c('Negative', 'Positive'), lty = 1, bg = 'white')

#another representation
n_econ <- ntoken(dfm(toks_econ, group = docvars(toks_econ, 'date')))
plot((dfmat_econ_lsd[,2] - dfmat_econ_lsd[,1]) / n_econ, 
     type = 'l', ylab = 'Sentiment', xlab = '', xaxt = 'n')
axis(1, seq_len(ndoc(dfmat_econ_lsd)), ymd("2016-06-30") + days(seq_len(ndoc(dfmat_econ_lsd)) - 1))
grid()
abline(h = 0, lty = 2)




#by text targeted
drug <- c('drug*', 'drog*', 'shabu', 'pusher*', 'addict*', 'adik', 'drug-*', 'methampenamine', 'marijuana', 'cocaine')
toks_drug <- tokens_keep(toks_duterte, pattern = phrase(drug), window = 10)
dfmat_drug_lsd <- dfm(toks_drug, dictionary = data_dictionary_LSD2015[1:2])
head(dfmat_drug_lsd, 731)




drug <- c('drug*', 'drog*', 'shabu', 'pusher*', 'addict*', 'adik', 'drug-*', 'methampenamine', 'marijuana', 'cocaine')
toks_drug <- tokens_keep(toks_duterte, pattern = phrase(drug), window = 10)
dfmat_drug_lsd <- dfm(toks_drug, dictionary = data_dictionary_LSD2015[1:2]) %>% 
  dfm_group(group = 'month', fill = TRUE) 

matplot(dfmat_drug_lsd, type = 'l', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
axis(1, seq_len(ndoc(dfmat_drug_lsd)), ymd("2016-06-30") + months(seq_len(ndoc(dfmat_drug_lsd)) - 1))
legend('topleft', col = 1:2, legend = c('Negative', 'Positive'), lty = 1, bg = 'white')


n_drug <- ntoken(dfm(toks_drug, group = docvars(toks_drug, 'month')))
plot((dfmat_drug_lsd[,2] - dfmat_drug_lsd[,1]) / n_drug, 
     type = 'l', ylab = 'Sentiment', xlab = '', xaxt = 'n')
axis(1, seq_len(ndoc(dfmat_drug_lsd)), ymd("2016-06-30") + months(seq_len(ndoc(dfmat_drug_lsd)) - 1))
grid()
abline(h = 0, lty = 2)


#collocation analysis

toks_duterte <- tokens(corp_duterte, remove_punct = TRUE)
tstat_col_caps <- tokens_select(toks_duterte, pattern = '^[A-Z]', 
                                valuetype = 'regex', 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 100)
head(tstat_col_caps, 20)

tstat_col2 <- tokens_select(toks_duterte, pattern = '^[A-Z]', 
                            valuetype = 'regex', 
                            case_insensitive = FALSE, 
                            padding = TRUE) %>% 
  textstat_collocations(min_count = 100, size = 3)
head(tstat_col2, 20)



#targeted word collocation

ndoc(corp_duterte)
range(docvars(corp_duterte, 'date'))

#cleaning
require(dplyr)

mystopwords <- c("â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ")
toks_duterte <- tokens(corp_duterte, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte, pattern = stopwords('en'), padding = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte2, pattern = stopwords('tl', source = "stopwords-iso"), padding = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte2, pattern = mystopwords, padding = TRUE)


kw_drug <- kwic(toks_duterte2, pattern = c('droga*', 'drug*'))
head(kw_drug, 10)

dfmat_duterte <- dfm(toks_duterte2, stopwords('en'), remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
mystopwords <- c("â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ")

dfmat_duterte <- dfm_remove(dfmat_duterte, stopwords('tl', source = "stopwords-iso"))

dfmat_duterte <- dfm_remove(dfmat_duterte, pattern = mystopwords, valuetype = 'fixed')
dfmat_duterte <- dfm_trim(dfmat_duterte, min_termfreq = 100)



# analysis of co-occuring words with drugs

corp_duterte <- corpus(dat_duterte)
mystopwords <- c("â", "gamay", "dayon", "tan-aw", "kinahanglan", "bitawan", "kadtong", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ", "s", "q", "bid", "translation", "ms", "unya", "gud", "gani", "naay", "usa", "diri", "ra", "unya", "kanang","ingon", "didto", "ngadto", "hi", "mr", "ms", "kanang", "karon", "diri", "sec", "mic", "blah", "cum", "wa", "naa", "naay", "unsa", "anang", "ngari", "ning", "kayoâ", "piã", "ng", "ang", "mga", "niadto", "kana", "kanang", "kaayo", "muingon", "pila", "â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ", "s", "q", "bid", "translation", "ms", "unya", "gud", "gani", "naay", "usa", "diri", "ra", "unya", "kanang","ingon", "didto", "ngadto", "hi", "mr", "ms", "kanang", "karon", "diri", "mic", "blah", "cum", "wa", "naa", "naay", "unsa", "anang", "ngari", "ning", "kayoâ", "piã", "ng", "ang", "mga", "ã", "tat", "lam", "daghan", "kani", "garbled", "kani", "kinsa", "iyongâ", "dun", "kaning", "kuan", "mo'g", "asa", "giingnan", "nganong", "katong", "koy", "iyay", "sigeg", "gi", "nay", "duha","ko'g", "ngano", "diay", "n", "ila", "ga", "wasnâ", "kog", "mog", "msmes", "dr", "naaâ", "taâ", "na'y", "ninyoâ", "nga", "one", "two", "three", "yang", "ito*", "four", "already", "kindly", "tikas pahinga", "thank you","translation", "mo", "wala", "just", "kayo", "ninyo", "diyan", "president", "si", "can", "talaga", "man", "pag", "akong", "p", "kay", "pag", "man", "alam", "tapos", "pati", "di", "kay", "t", "m", "really", "g", "said", "now", "go", "like", "y", "time", "itong", "thank you", "ganun", "iyan", "po", "sir", "itong", "natin", "speech", "ba", "inyo", "also", "yun", "talagang", "ayaw", "tsaka", "imong", "nimong", "duterte", "even", "years", "sige", "rin", "kayong", "even", "day", "nimo", "atong", "kwarta", "tinuod", "ani", "kamo", "kamong", "maayo", "nato", "tan-awa", "pong", "e", "speaks", "walay", "didn*", "tanang", "aning", "pag-abot", "motor", "gift", "unsay", "murag", "bitaw", "nong", "balay", "kada", "pagkahuman", "ginoo", "someone", "ol", "basig", "adto", "kahibaw", "gwapo", "gwapa", "hasta", "tawo", "amo", "ihatag", "ma'y", "roy", "dato", "dako", "ko'y", "kuwan", "e", "uban", "ma'y", "yuta", "imo", "mama", "tagaan", "maayong", "imo", "yawa", "yeah", "sheâ", "gabii", "unta", "wonâ", "alas", "ingna", "ingnon", "jr", "reallyâ", "kungâ", "mo.â")


toks_duterte <- tokens(corp_duterte, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte, pattern = stopwords('en'), padding = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte2, pattern = stopwords('tl', source = "stopwords-iso"), padding = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte2, pattern = mystopwords, padding = TRUE)


drug <- c('drug*', 'drog*', 'shabu', 'pusher*', 'addict*', 'adik', 'drug-*', 'methampenamine', 'marijuana', 'cocaine')
toks_drug <- tokens_keep(toks_duterte2, pattern = phrase(drug), window = 10)
toks_nodrug <- tokens_remove(toks_duterte2, pattern = phrase(drug), window = 10) 

----------------------------
  
  tstat_drug <- tokens_select(toks_duterte2, pattern = drug, 
                              valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) %>% 
  textstat_collocations(min_count = 100)
head(tstat_drug, 20)




dfmat_drug <- tokens_remove(toks_duterte2, stopwords('tl', source = "stopwords-iso"))

dfmat_drug <- tokens_remove(toks_duterte2, pattern = mystopwords, valuetype = 'fixed')

dfmat_drug <- dfm(toks_drug)
dfmat_nodrug <- dfm(toks_nodrug)

tstat_key_drug <- textstat_keyness(rbind(dfmat_drug, dfmat_nodrug), seq_len(ndoc(dfmat_drug)))
tstat_key_drug_subset <- tstat_key_drug[tstat_key_drug$n_target > 10, ]
head(tstat_key_drug_subset, 50)


fcmat_drugs <- fcm(toks_drug)
dim(fcmat_drugs)

feat <- names(topfeatures(fcmat_drugs, 50))
fcmat_drugs_select <- fcm_select(fcmat_drugs, pattern = feat)
dim(fcmat_drugs_select)

#co-occurence network
size <- log(colSums(dfm_select(dfmat_drug, feat)))
set.seed(144)
textplot_network(fcmat_drugs_select, min_freq = 0.8, vertex_size = size / max(size) * 3)



#constructing FCM


corp_duterte <- corpus(dat_duterte)
dfmat_duterte <- dfm(corp_duterte, remove = stopwords('en'), remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
mystopwords <- c("â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ", "s", "q", "bid", "translation", "ms", "unya", "gud", "gani", "naay", "usa", "diri", "ra", "unya", "kanang","ingon", "didto", "ngadto", "hi", "mr", "ms", "kanang", "karon", "diri", "sec", "mic", "blah", "cum", "wa", "naa", "naay", "unsa", "anang", "ngari", "ning", "kayoâ", "piã", "ng", "ang", "mga", "niadto", "kana", "kanang", "kaayo", "muingon", "pila", "â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ", "s", "q", "bid", "translation", "ms", "unya", "gud", "gani", "naay", "usa", "diri", "ra", "unya", "kanang","ingon", "didto", "ngadto", "hi", "mr", "ms", "kanang", "karon", "diri", "mic", "blah", "cum", "wa", "naa", "naay", "unsa", "anang", "ngari", "ning", "kayoâ", "piã", "ng", "ang", "mga", "ã", "tat", "lam", "daghan", "kani", "garbled", "kani", "kinsa", "iyongâ", "dun", "kaning", "kuan", "mo'g", "asa", "giingnan", "nganong", "katong", "koy", "iyay", "sigeg", "gi", "nay", "duha","ko'g", "ngano", "diay", "n", "ila", "ga", "wasnâ", "kog", "mog", "msmes", "dr", "naaâ", "taâ", "na'y", "ninyoâ", "nga", "one", "two", "three", "yang", "ito*", "four", "already", "kindly", "tikas pahinga", "thank you","translation*", "mo", "wala", "just", "kayo", "ninyo", "diyan", "president", "si", "can", "talaga", "man", "pag", "akong", "p", "kay", "pag", "man", "alam", "tapos", "pati", "di", "kay", "t", "m", "really", "g", "said", "now", "go", "like", "y", "time", "itong", "thank you", "ganun", "iyan", "po", "sir", "itong", "natin", "speech", "ba", "inyo", "also", "yun", "talagang", "ayaw", "tsaka", "imong", "nimong", "duterte", "even", "years", "sige", "rin", "kayong", "even")

dfmat_duterte <- dfm_remove(dfmat_duterte, stopwords('tl', source = "stopwords-iso"))

dfmat_duterte <- dfm_remove(dfmat_duterte, pattern = mystopwords, valuetype = 'fixed')



dfmat_duterte <- dfm_trim(dfmat_duterte, min_termfreq = 100)

dfmat_duterte <- dfm_trim(dfmat_duterte, min_termfreq = 100)

topfeatures(dfmat_duterte)

dfmat_duterte_90 <- dfm_trim(dfmat_duterte, max_docfreq = 0.9, docfreq_type = "prop")
nfeat(dfmat_duterte_10)

feat <- names(topfeatures(fcmat_news, 50))
fcmat_news_select <- fcm_select(fcmat_news, pattern = feat)
dim(fcmat_news_select)


--------------------------
  
  
  toks_duterte <- tokens(corp_duterte, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte, pattern = stopwords('en'), padding = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte2, pattern = stopwords('tl', source = "stopwords-iso"), padding = TRUE)
toks_duterte2 <- tokens_remove(toks_duterte2, pattern = mystopwords, padding = TRUE)

drug <- c('drug*', '*drog*', 'shabu', 'pusher', 'addict', 'adik', 'metamphenamine', 'drug-*')
toks_drug <- tokens_keep(toks_duterte2, pattern = phrase(drug), window = 10)
dfm_sona <- dfm(toks_duterte2)

dfm_duterte <- dfm(toks_duterte2)
freq_weight <- textstat_frequency(dfmat_duterte, n = 10, groups = "audience")
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")



#feature co-occurence

# full list of stopwords

mystopwords <- c("â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ", "s", "q", "bid", "translation", "ms", "unya", "gud", "gani", "naay", "usa", "diri", "ra", "unya", "kanang","ingon", "didto", "ngadto", "hi", "mr", "ms", "kanang", "karon", "diri", "sec", "mic", "blah", "cum", "wa", "naa", "naay", "unsa", "anang", "ngari", "ning", "kayoâ", "piã", "ng", "ang", "mga", "niadto", "kana", "kanang", "kaayo", "muingon", "pila", "â", "yung", "lang", "yan", "s", "eh", "kasi", "ug", "mao", "ta", "og", "naa", "ana", "diha", "pud", "dili", "itâ", "naman", "iâ", "s", "q", "bid", "translation", "ms", "unya", "gud", "gani", "naay", "usa", "diri", "ra", "unya", "kanang","ingon", "didto", "ngadto", "hi", "mr", "ms", "kanang", "karon", "diri", "sec", "mic", "blah", "cum", "wa", "naa", "naay", "unsa", "anang", "ngari", "ning", "kayoâ", "piã", "ng", "ang", "mga", "ã", "tat", "lam", "daghan", "tanan*", "atong", "nimong", "kamong", "kani", "nato", "garbled", "imo", "kamo", "kani", "atong", "imong", "kinsa", "iyongâ", "dun", "kaning", "tanan", "nimo", "kuan", "mo'g", "asa", "usec", "giingnan", "n", "ila", "ga", "wasnâ", "kog", "mog", "msmes", "dr")

#topic modelling

require(topicmodels)






dfmat_duterte_tm <- dfm_trim(dfm_duterte, min_termfreq = 0.95, termfreq_type = "quantile", max_docfreq = 0.1, docfreq_type = "prop")

dfmat_duterte_tm <- dfmat_duterte_tm[ntoken(dfmat_duterte_tm) > 0,]
dtm <- convert(dfmat_duterte_tm, to = "topicmodels")
lda <- LDA(dtm, k = 10, method = "Gibbs", 
           control = list(verbose=25L, seed = 12345, burnin = 1000, iter = 5000))
terms(lda, 10)
docvars(dfmat_duterte_tm, 'topic') <- topics(lda)
head(topics(lda), 10)


postTopics <- data.frame(posterior(lda)$topic)

dat_duterte$prob_topic <- lda@gamma[,19]
agg <- aggregate(dat_duterte$prob_topic, by=list(date=dat_duterte$date), FUN=mean)
plot(agg$date, agg$x, type="l", xlab="date", ylab="Avg. prob. of article about topic 19", main="Estimated proportion of articles about ASEAN")

#for extracting month
month(dmy(some_date))


# saving as csv file
topic2 <- docvars(dfmat_duterte_tm, 'topic')
dat_duterte$topic2<- topic2

write.csv(dat_duterte,'dat_duterte.csv')



#using stm package

require(stm)
dfmat_duterte_tm <- dfm_trim(dfm_duterte, min_termfreq = 0.9, termfreq_type = "quantile", max_docfreq = 0.2, docfreq_type = "prop")


topic.count <- 20
dfm2stm <- convert(dfmat_duterte_tm, to = "stm")
model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, K = topic.count, prevalence=~audience+date, max.em.its=75,data = dfm2stm$meta, init.type = "Spectral") 

#model selection

dutmod <- selectModel(dfm2stm$documents, dfm2stm$vocab, K=20, prevalence=~audience+inf+satis+fst_year+s(caldays), max.em.its=75, data=dfm2stm$meta, runs=20, seed=123)
plotModels(dutmod)
plot(model.stm, type="summary", xlim=c(0,.4))
selectedModel4 <- dutSelect$runout[[4]]

topicQuality(model=selectedModel4, documents=dfm2stm$documents)

#selecting model 4
selectedModel4 <- dutSelect$runout[[4]]

#correlation between topics


mod.out.corr <- topicCorr(selectedMode20_1)
plot(mod.out.corr)

plot(prep, "caldays", method="continuous", topics=20, model=z, printlegend=FALSE, xaxt="n", 
     xlab="Time")
monthseq <- seq(from=as.Date("2016-04-10"), to=as.Date("2018-02-30"), by="date")
monthnames <- months(monthseq)
axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)

#estimating
dfm2stm$meta$audience <- as.factor(dfm2stm$meta$audience)
dfm2stm$meta$fst_year <- as.factor(dfm2stm$meta$fst_year)
prep20_4 <- estimateEffect(1:20 ~ audience+fst_year+satis+inf+s(caldays), selectedModel4, meta=dfm2stm$meta, uncertainty="Global")


prep20_ <- estimateEffect(topics=c(2,8,12,16,19,20,21,24) ~ audience+satis+inf+s(daysona), selectedModel20_4, meta=dfm2stm$meta, uncertainty="Global") 





topics=c(2,8,12,16,19,20,21,24)

data.frame(t(labelTopics(model.stm, n = 10)$prob))
s <- data.frame(t(labelTopics(model.stm, n = 10)$prob))


plot(model.stm, type = "summary", text.cex = 0.5)

plot(model.stm, type = "perspectives", topics = c(16,21))

#stm version 2
dfm2stm <- convert(dfmat_duterte_tm, to = "stm")
meta <- dfm2stm
model.stm2 <- stm(dfm2stm$documents, vocab = out$vocab, K = 20, prevalence =~ caldays,
                  + max.em.its = 75, data = out$meta, init.type = "Spectral")





------------------------
  #removing features more than 90 percent
  
  dfmat_duterte_90 <- dfm_trim(dfmat_duterte, max_docfreq = 0.9, docfreq_type = "prop")
nfeat(dfmat_duterte_10)

topfeatures(dfmat_duterte_90)

fcmat_duterte <- fcm(dfmat_duterte_90)
dim(fcmat_duterte)
-------------------------
  # creating the collocation network
  
  fcmat_duterte <- fcm(dfmat_duterte)
dim(fcmat_duterte)

feat <- names(topfeatures(fcmat_duterte, 50))
fcmat_duterte_select <- fcm_select(fcmat_duterte, pattern = feat)
dim(fcmat_duterte_select)

size <- log(colSums(dfm_select(dfmat_duterte, feat)))
set.seed(144)
textplot_network(fcmat_duterte_select, min_freq = 0.8, vertex_size = size / max(size) * 3)





library(tidyverse)
library(lubridate)


date <- paste(d$year, d$month, d$day, sep="-")
date2<-as.Date(date)




date3 <- date2 - ymd("2016-04-10")
if (require(ggplot2))
  ggplot(data = d, aes(x = date3, y = Tokens, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = c(seq(0, 683, 1)), breaks = seq(0, 683, 1)) +
  theme_bw()

toks_duterte <- tokens(corp_duterte)
toks_duterte_nopunct <- tokens(corp_duterte, remove_punct = TRUE)
head(toks_duterte_nopunct[[1]], 50)
kw_duterte <- kwic(toks_duterte_nopunct, pattern =  'drug*')
head(kw_duterte, 10)

#kwic patterns
kw_duterte <- kwic(toks_duterte_nopunct, pattern = c('drog*', 'drug*'), window = 7)
head(kw_duterte, 10)

toks_duterte_nopunctnostop <- tokens_select(toks_duterte_nopunct, pattern = stopwords('en'), selection = 'remove')
head(toks_duterte_nopunctnostop[[1]], 50)

#remove with padding
toks_duterte_nopunctnostop <- tokens_remove(toks_duterte_nopunct, pattern = stopwords('en'), padding = TRUE)
head(toks_duterte_nopunctnostop[[1]], 50)

#constructing dfmat
dfmat_toks_duterte_nopunctnostop <- dfm(toks_duterte_nopunctnostop)


#check stopwords
require("stopwords")
head(stopwords::stopwords("tl", source = "stopwords-iso"), 20)

#constructing df matrix
require(quanteda.corpora)
dfmat_duterte <- dfm(corp_duterte, remove = stopwords('en'), remove_punct = TRUE)
dfmat_duterte <- dfm_remove(dfmat_duterte, stopwords('tl', source = "stopwords-iso"))
dfmat_duterte <- dfm_remove(dfmat_duterte, stopwords(c("â", "yung", "lang", "yan", "s", "eh", "kasi"))
                            dfmat_duterte <- dfm_remove(dfmat_duterte, pattern = c('*-time', 'updated-*', 'gmt', 'bst'))
                            dfmat_duterte <- dfm_trim(dfmat_duterte, min_termfreq = 100)
                            topfeatures(dfmat_duterte)
                            
                            
                            
                            library(tm)
                            docs <- Corpus(DirSource("C:/Users/proyekto_v2/Desktop/rprojects/duterte", encoding = "utf-8"))
                            docs <- tm_map(docs, content_transformer(tolower))
                            
                            toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
                            docs <- tm_map(docs, toSpace, "-")
                            docs <- tm_map(docs, toSpace, "'")
                            docs <- tm_map(docs, toSpace, ";")
                            docs <- tm_map(docs, toSpace, "")
                            docs <- tm_map(docs, toSpace, "")
                            docs <- tm_map(docs, toSpace, "â")
                            docs <- tm_map(docs, toSpace, "")
                            docs <- tm_map(docs, toSpace, "¦")
                            docs <- tm_map(docs, toSpace, "")
                            docs <- tm_map(docs, toSpace, ":")
                            docs <- tm_map(docs, toSpace, "")
                            docs <- tm_map(docs, toSpace, "@")
                            docs <- tm_map(docs, toSpace, ".")
                            
                            docs <- tm_map(docs, removePunctuation)
                            docs <- tm_map(docs, removeNumbers)
                            docs <- tm_map(docs, removeWords, stopwords("english"))
                            docs <- tm_map(docs, stripWhitespace)
                            docs <- tm_map(docs,stemDocument)
                            
                            corp_tm <- tm::VCorpus(tm::VectorSource(docs))
                            corp_quanteda <- corpus(corp_tm)
                            summary(corp_quanteda)
                            
                            my_dfm <- dfm(corp_quanteda, remove = stopwords("english"), remove_punct = TRUE)
                            my_dfm
                            
                            set.seed(100)
                            textplot_wordcloud(my_dfm, min_count = 6, random_order = FALSE,
                                               rotation = .25, 
                                               color = RColorBrewer::brewer.pal(8,"Dark2"))
                            
                            
                            dtm <- DocumentTermMatrix(docs)
                            
                            filenames <- list.files(getwd(),pattern="C:/Users/proyekto_v2/Desktop/rprojects/duterte/*.txt")
                            files <- lapply(filenames,readLines)
                            
                            rownames(dtm) <- filenames
                            freq <- colSums(as.matrix(dtm))
                            length(freq)
                            ord <- order(freq,decreasing=TRUE)
                            freq[ord]
                            write.csv(freq[ord],"word_freq.csv")
                            
                            
                            
 -------------------------
                              
Order of presentation
                            1. frequency of text (topics) by audience
                            2. timeline of drugs issue 
                            
                            toks_dut <- tokens(corp_duterte, remove_punct = TRUE, remove_number=TRUE)
                            toks_drug <- tokens_keep(toks_duterte2, pattern = drug, window = 10) 
                            toks_nodrug <- tokens_remove(toks_duterte2, pattern = drug, window = 10)
                            dfmat_drug <- dfm(toks_drug)
                            dfmat_nodrug <- dfm(toks_nodrug)
                            tstat_key_drug <- textstat_keyness(rbind(dfmat_drug, dfmat_nodrug), seq_len(ndoc(dfmat_drug)))
                            tstat_key_drug_subset <- tstat_key_drug[tstat_key_drug$n_target > 10, ]
                            head(tstat_key_drug_subset, 30)
                            
                            
                            ----------------------
                              #for econ variable
                              
                              page <- read.csv("pagination2.csv")
                            
                            path_data <- system.file("C:/Users/proyekto_v2/Desktop/rprojects")
                            dat_econ <- readtext(paste0(path_data, "C:/Users/proyekto_v2/Desktop/rprojects/econ net sources/*.txt"),docvarsfrom = "filenames", encoding = "ISO-8859-1")
                            
                            library(lubridate)
                            page$date2 <- as.Date(page$date, "%B%d,%Y")
                            merged.data <- merge(dat_econ, page, by=c("doc_id"))
                            merged <- merged.data[order(dmerged.data$date2),]
                            
                            corp_econ <- corpus(merged)
                            toks_econ <- tokens(corp_econ, remove_punct = TRUE)
                            lengths(data_dictionary_LSD2015)
                            toks_econ_lsd <- tokens_lookup(toks_econ, dictionary =  data_dictionary_LSD2015[1:2])
                            head(toks_econ_lsd, 2)
                            dfmat_econ_lsd <- dfm(toks_econ_lsd)
                            head(dfmat_econ_lsd, 2)
                            
                            
                            #econ2
                            
                            economy <- c("appreciate", "bankrupt*", "business", "infrast*", "budget*", "capital*", "*employ*", "corporation", "competition", "cash", "consumer*", "cost*", "crash", "credit", "currency", "debt", "deficit", "deposit", "depression", "econom*", "financ*", "fiscal", "global", "inflation", "interest", "international", "invest*", "infrastructur*", "loan", "loss", "market*", "monetary", "money*", "loan*", "recession*", "savings", "spend*", "stock*", "tax", "revenue", "trade", "value", "labor*", "work", "wage*")
                            toks_econ2 <- tokens_keep(toks_econ, pattern = phrase(economy), window = 10)
                            dfmat_econ2_lsd <- dfm(toks_econ, dictionary = data_dictionary_LSD2015[1:2])
                            
                            n_econ2 <- ntoken(dfm(toks2_econ, group = docvars(toks_econ2, 'date2')))
                            plot((dfmat_econ2_lsd[,2] - dfmat_econ2_lsd[,1]) / n_econ, 
                                 type = 'l', ylab = 'Sentiment', xlab = '', xaxt = 'n')
                            axis(1, seq_len(ndoc(dfmat_econ2_lsd)), ymd("2016-06-30") + days(seq_len(ndoc(dfmat_econ2_lsd)) - 1))
                            grid()
                            abline(h = 0, lty = 2)
                            ---------------------------------
                              #china keyword
                              
                              
                              china <- c('chin*','tsino', 'tsina', 'intsik', 'xi jinping', 'xi', 'jinping', 'beijing', 'west philippine sea', 'spratl*', 'exclusive economic zone', 'maritime', 'territories', 'arbitral*', 'ecc', 'south china sea')
                            
                            
                            kw_china <- kwic(toks_duterte2, pattern = china)
                            head(china, 10)
                            
                            
                            #nation keyword
                            nation <- c('country*','philippine*', 'filipin*', 'people', 'bayan', 'bansa', '*bayan', 'citizen', 'nation*', 'spratl*')
                            
                            kw_nation <- kwic(toks_duterte2, pattern = nation)
                            head(kw_nation, 10)
                            
                            ------------------
                              dfmat_duterte_tm <- dfm_trim(dfm_duterte, min_termfreq = 0.9, termfreq_type = "quantile", max_docfreq = 0.2, docfreq_type = "prop")
                            > 
                              > dfmat_duterte_tm <- dfmat_duterte_tm[ntoken(dfmat_duterte_tm) > 0,]
                            > dtm <- convert(dfmat_duterte_tm, to = "topicmodels")
                            > lda <- LDA(dtm, k = 10, method = "Gibbs", 
                                         +            control = list(verbose=25L, seed = 12345, burnin = 1000, iter = 5000))
                            
                            
                            
                            Topic 1       Topic 2         Topic 3         Topic 4      Topic 5       Topic 6   
                            [1,] "reform"      "rape"          "valedictorian" "islam"      "pnp"         "nako"    
                            [2,] "aide"        "extrajudicial" "oil"           "sayyaf"     "afp"         "gyud"    
                            [3,] "cheers"      "cocaine"       "bello"         "abu"        "commander"   "tanan"   
                            [4,] "ilonggo"     "line"          "tugade"        "nur"        "delfin"      "nimo"    
                            [5,] "honest"      "streets"       "bebot"         "federal"    "camp"        "atong"   
                            [6,] "trillanes"   "nine"          "ilocano"       "magellan"   "performance" "kwarta"  
                            [7,] "roxas"       "trial"         "unclear"       "tausug"     "personnel"   "tinuod"  
                            [8,] "boracay"     "count"         "cheers"        "mn"         "troops"      "ani"     
                            [9,] "bakla"       "cases"         "boracay"       "spaniards"  "operations"  "kamo"    
                            [10,] "diokno"      "age"           "ofw"           "misuari"    "safe"        "kamong"  
                            [11,] "tatad"       "intended"      "sonny"         "violence"   "lieutenant"  "nalang"  
                            [12,] "año"         "finish"        "transaction"   "brothers"   "civilian"    "maayo"   
                            [13,] "church"      "fired"         "bir"           "christians" "class"       "nato"    
                            [14,] "senador"     "judge"         "lopez"         "malay"      "kampo"       "tan-awa" 
                            [15,] "gift"        "listening"     "hotel"         "dureza"     "task"        "daghang" 
                            [16,] "norte"       "dysfunctional" "project"       "allah"      "enemy"       "walay"   
                            [17,] "alunan"      "careful"       "labor"         "rebellion"  "safety"      "aning"   
                            [18,] "malacañan"   "eat"           "inquirer"      "lanao"      "defend"      "tanang"  
                            [19,] "simbahan"    "knew"          "classmate"     "island"     "aã"          "murag"   
                            [20,] "mangudadatu" "level"         "commercial"    "murad"      "families"    "pag-abot"
                            Topic 7         Topic 8       Topic 9     Topic 10   
                            [1,] "asean"         "climate"     "ganoon"    "lupa"     
                            [2,] "japan"         "mining"      "budget"    "ibinigay" 
                            [3,] "relations"     "act"         "e"         "magbili"  
                            [4,] "welcome"       "shall"       "aâ"        "tignan"   
                            [5,] "minister"      "water"       "akoâ"      "kinuha"   
                            [6,] "greater"       "projects"    "nong"      "magsalita"
                            [7,] "asia"          "claim"       "warrant"   "patayan"  
                            [8,] "forward"       "meet"        "expletive" "pinapatay"
                            [9,] "common"        "health"      "koâ"       "harap"    
                            [10,] "shared"        "bank"        "rehab"     "masabi"   
                            [11,] "ensure"        "pong"        "akongâ"    "medisina" 
                            [12,] "partners"      "afternoon"   "magâ"      "mag-usap" 
                            [13,] "friendship"    "decide"      "andâ"      "bunganga" 
                            [14,] "excellency"    "evening"     "you.â"     "pumatay"  
                            [15,] "prime"         "immediately" "hospital"  "binigyan" 
                            [16,] "investments"   "tulfo"       "letâ"      "salita"   
                            [17,] "indeed"        "agencies"    "eu"        "ospital"  
                            [18,] "distinguished" "senate"      "doctor"    "u"        
                            [19,] "peoples"       "tax"         "isâ"       "taon"     
                            [20,] "opportunity"   "services"    "yan.â"     "barilin" 
                            
                            nbreg applause_freq asean climate  reform extrajud minda afp_pnp notice_strike logpse inf ib4.i_aud  i.year econ_neg econ_pos , exp(sentences) robust
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            