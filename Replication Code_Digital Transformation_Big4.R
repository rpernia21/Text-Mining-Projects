####R Codes for the "Big 4" Data####

#clean environment
rm(list=ls())

#install packages
library(tm)
library(NLP)
library(RColorBrewer)
library(readtext)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(lattice)
library(stringr)
library(dplyr) 
library(tidytext)
library(stopwords)

#Data import: all docs files containing 668 observations
'Merge_word<- readtext("MergeData_word/*.docx", 
                         docvarsfrom = "filenames", 
                         docvarnames = NULL,
                         dvsep = "_") 
'
#Data import: all .txt files containing 1433 observations
Merge_txt <- readtext("MergeData_txt/*.txt",
                docvarsfrom = "filenames", 
                docvarnames = NULL,
                dvsep = "_") 

'#Data import: all .txt files containing 199 observations
WoS <- readtext("WoS_textfiles/*.txt",
                docvarsfrom = "filenames", 
                docvarnames = NULL,
                dvsep = "_")
#Merge datasets
Merge <-full_join(Merge_txt,Merge_word)
MergeAll <-full_join(Merge, WoS)

#View data: select only text column 
MergeAll <-MergeAll$text

'

Strait.times <-readLines("C:/Users/User/Desktop/R projects/Textmining Project/Strait Times/Strait Times.txt")
Strait.times

#Remove special characters
Strait.times <- gsub("[^0-9A-Za-z///' ]","'" , Strait.times,ignore.case = TRUE)
Strait.times <- gsub("''","" , Strait.times ,ignore.case = TRUE)

#Create a corpus
Strait.times_corpus <- iconv(Strait.times, to = "UTF-8")
corpus <- Corpus(VectorSource(Strait.times_corpus))

'#Remove special characters again 
MergeAll_corpus  <- gsub("[^0-9A-Za-z///' ]","'" , MergeAll_corpus ,ignore.case = TRUE)
MergeAll_corpus<- gsub("''","" , MergeAll_corpus ,ignore.case = TRUE)
'
# Data Preprocessing 
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stemDocument)
cleanset <-tm_map(corpus, removeWords, stopwords('English'))
cleanset <-tm_map(cleanset, removeWords, stopwords('en'))

#List of all unnecessary words to be removed 
cleanset<-tm_map(cleanset,removeWords, c("business","businesses", "management","die", "view", "vol", "order", "journal", "both", "study",
                                         "much", "table","between","literature","others","next","available", "results",
                                         "best", "view","article","digital","transformation","still","can","just","full","first","our", "however","will","what","help","its","these","over","that", "this", "from", "have", "due", "like", "well",
                                         "iot", "sold", "many","how","they","which","but","into","been","does","betwen","now","likely","case",
                                         "may", "see", "rise","most","through","use","one","some","set","llp","his","etc","too","you","when","cio",
                                         "across", "percent", "deliotte","even","many","did","break","lot","only","app","wall",
                                         "able","ibid","see","yet","also", "figure","deloitteglobal", "endnotes", "copyright","was","who","years","each","she","such","four","sources","'s","very","pilot","would","look","share","them","teams","low","cios","were","while",
                                         "based","better","moves","pay","cloud","example","last","its","own","two","realtime","run","sap",
                                         "other","and", "april", "dttl","executivesummary", "sourcedeloitteanalysis","per", "touchhelp", "idc", "contact", "august", "july","more","about","your",
                                         "months", "fill", "make", "more", "than", "had","has","their","top","any","out", "allows","tmt", "return", "all","not","the", "with", "for", "are",
                                         "new", "need", "lead", "come","could","should","tech","come","trial","visit",
                                         "plan","enable","one","first","see","may","live","affect","step", "towards","port","continued","mit","law","often","risk","web","criteria","ibm","res","bus","fig",
                                         "initial","bic","line","inf","thomas","basic","siri","bell","must","vice","paper","critical","show","include",
                                         "increase","main","bpm","learn","roles","via","mis",
                                         "used","three","review","roles","shift","eur","turn",
                                         "int","different","role","old","then","large","same",
                                         "nie","dimension","includes","being","mcdm","failure",
                                         "fast","show","via","several","flow","method","find",
                                         "van","approach","then","overall","core","within","conduct","kinds","age","way","eds",
                                         "small","tools","studies","micro","airline","after","york",
                                         "lack","built","act","few","cluster","where","erp","itself",
                                         "davis","face","term","year","free","here","oil","cost","free",
                                         "less","rather","high","era","xij","source","six","aim","idea",
                                         "elements","viz","senior","kinds","using","bid","far","cases","focus",
                                         "face","take","part","build","test","fit","under","further","logic","range",
                                         "soa","certain","scale","dat","refers","existing","class","smes","loss",
                                         "claim","tou","vir","adopt","long","sas","methods","articles","there","because",
                                         "want","fuzzy","made","thus","end","sales","allow","level","nature",
                                         "framework","theory","related","brm","size",
                                         "result","variables","users","report",
                                         "items","since","retail","date","form",
                                         "control","vast","offers","users","yin",
                                         "revenue","tool","point","school","activities",
                                         "research","school","adoption","theoretical","creation","actors","beta","rate","term",
                                         "list","prior","field","fta","lower","past","ideas","levels","work",
                                         "world","final","create","human","access","team","term","time","met",
                                         "units","light","function","influence","issues","until","yoo","offer",
                                         "analysis","found","audit","those","five","angel","considered","bmi","iii",
                                         "effect","base","ict","life","art","open","before","trends","relevant",
                                         "account","succeed","fun","touch","fundamental","whole","cash","virtual",
                                         "barrier","orientation","online","key","factor","act","without","agile","moreover","dts","trust","given","factors","der",
                                         "understanding","user","rev","goal","activity","total","ips","costs",
                                         "cent","sets","moreover","planning","text","insights","various","created","car","refer",
                                         "barriers","manag","lee","recent","toward","sector",
                                         "interview","sector","path","ability","limited",
                                         "ensure","alternative","mean","define","assessment",
                                         "uses","structures","side","higher","terms","members",
                                         "inc","practice","current","travel","culture","rights","led",
                                         "price","ahp","local","limited","working","second",
                                         "wat","fail","teece","dit","get","things",
                                         "usa","area","during","search","dit","basis",
                                         "ppm","track","type","types","shows","agility","whether",
                                         "system","harvard","alliance","evidence","finally",
                                         "gas","word","number","doi","joy","emerald","focused","involved","enabled",
                                         "kpmg","deloitte","pwc","deloitt","studi", "â€“", "digit", "busi","transform",
                                         "rms","fintech","sport","digitalization","bia","amazon","music","digitization","index",
                                         "csfs","latin","retrieved","csr","scs","dierent",
                                         "wto","accessed","nancial","media","journals","energy",
                                         "crossref","railway","digitalisation","respondents",
                                         "program","project","hfl","sdgs","suap","sci",
                                         "ipe","angels","amazons","syst","kuaishou","ent","ereadiness",
                                         "verhoef","dlt","scss","publicidad","netflix","nhs","pss","content","kko",
                                         "papers","variable","quarterly","startups","xbrl",
                                         "cbec","crosshierarchical","interviews","matrix","model","code","interviewe","weight","chain","variabl","digitalis",
                                         "hospit","zerowatermark","ereadi","circular","figur","reexiv","proposit","kkco","agil", "newspap","watermark","news","retriev","fuzzi",
                                         "digitali","actor","upm","score","oer","cashback",
                                         "conict","crosshierarch","museum","workday","oracl","successfactor","custom","hcm","foundri",
                                         "dsts","biopharma","gartner","ambit","survey","podcast",
                                         "ageoraci","cloudbas","christiana","say","trend","uber","ageoracl","servicesoracl","trish","vendor","cyber",
                                         "swiss","around","independent",
                                         "measures","response","vat",
                                         "ceos","read","kbpdf","kpmgs",
                                         "pdf","entity","advisory","reported",
                                         "topics","leading","library",
                                         "personalized","dashboard","orwelcomeyouve","tagname","ani","entiti",
                                         "overview","ceo","respons","measur", "models"))

cleanset <- tm_map(cleanset, stripWhitespace)

# Create Term document matrix for tokenization
tdm <- TermDocumentMatrix(cleanset)
#tdm.matrix <- as.matrix(tdm)

#Create word clouds
wordcloud(cleanset,min.freq = 200, colors = brewer.pal(8,"Dark2"), random.order=F,rot.per=.20)
wordcloud(cleanset,min.freq = 200, rot.per = .10)

#Finding the most frequent terms
findFreqTerms(tdm, lowfreq = 70)

#Arrange the word counts in decreasing order
Cleanset_sorted <-sort(rowSums(tdm.matrix), decreasing = T)
View(Cleanset_sorted)[1:25,] #get the top 25

#Create a dataframe
Cleanset_df <- data.frame (word = names(Cleanset_sorted), freq=Cleanset_sorted)
head(Cleanset_df) 

#Retain only the top 25
Cleanset_hist <-Cleanset_df[1:25,]
View(Cleanset_hist)

####TF-IDF Test####
review_dtm_tfidf <- DocumentTermMatrix(cleanset, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

# Check the document
inspect(review_dtm_tfidf)

#Rearranged in decreasing order
freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
View(freq)[1:25,] #get the top 25

#Create a dataframe
TFIDF <- data.frame (word = names(freq), freq=freq)
View(TFIDF)

#Retain only the top 25
TFIDF_hist <-TFIDF[1:25,]
View(TFIDF_hist)

#Output data in a Table
write.table(Cleanset_hist, file="ReplicationCodeMergeData.csv", sep=",")
write.table(TFIDF_hist, file="ReplicationCodeMergeData_TFIDF.csv", sep=",")

#End of Replication####
