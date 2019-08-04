# Gold Quill
rm(list = ls())
library(tidyverse)
library(janitor)
setwd("~/Publications/PeerReviewJournal/2019/ASFMRAmetaASFMRA")
list.files()
quillData<-read_csv("ASFMRAanalysis.csv")
quillData<-subset(quillData, year>=2004)

quillData$numAuthors<-10-rowSums(is.na(cbind(quillData$author1, quillData$author2, quillData$author3, quillData$author4, quillData$author5, quillData$author6, quillData$author7, quillData$author8, quillData$author9,quillData$author10)))

quillData$numWtitle<-ifelse(sapply(strsplit(quillData$Title, " "), length)>=2, sapply(strsplit(quillData$Title, " "), length),NA)

data=subset(quillData, GoldQuill == 1)

ggplot(quillData) + 
  geom_histogram(aes(x=numWtitle), stat="bin", binwidth = 0.5) + 
  geom_histogram(data=subset(quillData, goldQuill == 1), aes(x=numWtitle), stat="bin", binwidth = 0.5, fill="gold") + 
  xlab("Number of Words in Title") + ylab("Number of papers") +
  scale_x_continuous(breaks=seq(0,100,1)) + theme_bw()   
ggsave("numWords.png", width=6, height=4)

ggplot(quillData) + geom_histogram(aes(x=numAuthors), stat="bin", binwidth = 0.5) + 
  xlab("Number of Authors") + ylab("Number of papers") +
  scale_x_continuous(breaks=seq(0,10,1)) + theme_bw()   
ggsave("oldFig2numAuthors.png", width=6, height=4)

ggplot(quillData) + 
  geom_histogram(aes(x=numAuthors), stat="bin", binwidth = 0.5) + 
  geom_histogram(data=subset(quillData, goldQuill == 1), aes(x=numAuthors), stat="bin", binwidth = 0.5, fill="gold") + 
  xlab("Number of Authors") + ylab("Number of papers") +
  scale_x_continuous(breaks=seq(0,10,1)) + theme_bw()   
ggsave("Fig2numAuthorsAll.png", width=6, height=4)

tabyl(quillData, numAuthors, year)
aggregate(numAuthors ~year, data=quillData, FUN="mean")

ggplot(quillData) + geom_col(aes(x=year, y=numAuthors)) + #, col="black", fill="grey") +
  ylab("Number of authors") + 
#  scale_x_continuous((breaks=seq(2004, 2020, 1))) +
  xlab(NULL) +  theme_bw()
ggsave("Fig3numAuthorsYear.png", width=6, height=4)
  
  quillDat1<-subset(quillData, goldQuill==1)
quillDat0<-subset(quillData, goldQuill==0)
head(quillData)


# ? does number of words in title matter? change over time? 
#tabyl(quillDat1$numWtitle)
plot(quillDat1$year, quillDat1$numWtitle)


GQauthors0<-c(quillDat1$author1, quillDat1$author2, quillDat1$author3, quillDat1$author4, quillDat1$author5, quillDat1$author6, quillDat1$author7, quillDat1$author8, quillDat1$author9, quillDat1$author10)
GQauthors1<-subset(GQauthors0, is.na(GQauthors0)!=TRUE)
GQauthors2<-tabyl(GQauthors1)
ggplot(subset(GQauthors2, n>=1), aes(x=reorder(GQauthors1, n), y=n)) + 
  xlab("Gold Quill Co-authors") + ylab("number of articles") +
  scale_y_continuous(breaks=seq(0,10,1)) +
  geom_bar(stat="identity") + theme_bw() + coord_flip()
ggsave("GQcoauthors.png", width=6, height=4)

GQinstits0<-c(quillDat1$instAuthor1, quillDat1$instAuthor2, quillDat1$instAuthor3, quillDat1$instAuthor4, quillDat1$instAuthor5, quillDat1$instAuthor6, quillDat1$instAuthor7, quillDat1$instAuthor8, quillDat1$instAuthor9, quillDat1$instAuthor10)
GQinstits1<-subset(GQinstits0, is.na(GQinstits0)!=TRUE)
GQinstits2<-tabyl(GQinstits1)
ggplot(GQinstits2, aes(x=reorder(GQinstits1, n), y=n)) + 
  xlab("Gold Quill Institutions (including co-authors)") + ylab("number of articles") +
  scale_y_continuous(breaks=seq(0,50,1)) +
    geom_bar(stat="identity") + theme_bw() + coord_flip()
ggsave("Fig11GQcoauthorsInstits.png", width=6, height=4)

instits0<-c(quillDat0$instAuthor1, quillDat0$instAuthor2, quillDat0$instAuthor3, quillDat0$instAuthor4, quillDat0$instAuthor5, quillDat0$instAuthor6, quillDat0$instAuthor7, quillDat0$instAuthor8, quillDat0$instAuthor9, quillDat0$instAuthor10)
instits1<-subset(instits0, is.na(instits0)!=TRUE)
instits2<-tabyl(instits1)

GQinstits3<-GQinstits2
names(GQinstits3)<-c("loc", "win", "percent")
allInstits0<-c(quillData$instAuthor1, quillData$instAuthor2, quillData$instAuthor3, quillData$instAuthor4, quillData$instAuthor5, quillData$instAuthor6, quillData$instAuthor7, quillData$instAuthor8, quillData$instAuthor9, quillData$instAuthor10)
allInstits1<-subset(allInstits0, is.na(allInstits0)!=TRUE)
allInstits2<-tabyl(allInstits1)

instits3<-allInstits2
names(instits3)<-c("loc", "all", "percent")
instits4<-full_join(instits3, GQinstits3, by="loc")

#this needs a legend
ggplot(instits4, aes(x=loc)) + 
  geom_bar(aes(x=reorder(loc,all), y=all), stat="identity", col="black", fill="grey") +
  geom_bar(aes(x=loc, y= win), stat="identity", col="black", fill="gold") +
  scale_y_continuous(breaks=seq(0,150,5)) +
  ylab("number of authors") + xlab(NULL) +
  theme_bw() + coord_flip()
ggsave("Fig6AllInstitsAllPapers.png", width=6, height=8)



authors1<-tabyl(quillData$author1)
names(authors1)<-c("author", "n", "percent")

GQauthorsLead<-tabyl(quillDat1$author1)
names(GQauthorsLead)<-c("author", "n", "percent")

ggplot(subset(GQauthorsLead, n>=1), aes(x=reorder(author, n), y=n)) + 
  geom_bar(stat="identity", color="black", fill="grey") + 
  xlab("Gold Quill lead authors") +
  ylab("number of articles") + 
  scale_y_continuous(breaks=seq(0,10,1)) +
  theme_bw() + coord_flip()
ggsave("leadAuthorGQall.png", width=6, height=8)

ggplot(subset(GQauthorsLead, n>=2), aes(x=reorder(author, n), y=n)) + 
  geom_bar(stat="identity", color="black", fill="grey") + xlab("lead author") +
  ylab("frequency") + 
  theme_bw() + coord_flip()
ggsave("leadAuthorGQ_GT1.png", width=6, height=10)

ggplot(subset(authors1, n>=3), aes(x=reorder(author, n), y=n)) + 
  geom_bar(stat="identity", color="black", fill="grey") + xlab("lead author") +
  ylab("frequency") + 
scale_y_continuous(breaks=seq(0,20,2)) +
    theme_bw() + coord_flip()
ggsave("Fig4leadAuthorAllASFMRA0.png", width=6, height=4)

ggplot(subset(quillData, !is.na(author1)), aes(x=author1)) + 
  geom_bar(color="black", fill="grey") + xlab("lead author") +
  ylab("frequency") + 
  theme_bw() + coord_flip()
ggsave("AllLeadAuthors.png", width=6, height=4)


instit<-tabyl(quillData$instAuthor1)
names(instit)<-c("instit", "n", "percent1", "percent2")
instit<-subset(instit, n>=2)
ggplot(subset(instit, !is.na(instit)), aes(x=reorder(instit, n), y=n)) + 
  geom_bar(stat="identity") +#, color="black", fill="grey") + 
  xlab("institution of lead author (all articles)") +
  ylab("frequency") + scale_y_continuous(breaks = seq(0,50,2)) + 
  theme_bw() + coord_flip()
ggsave("Fig5AllInstitsLead.png", width=6, height=6, units="in")

mean(tabyl(quillData$year)$n)
min(tabyl(quillData$year)$n)
max(tabyl(quillData$year)$n)


ggplot(quillData, aes(x=year)) + 
  geom_bar(color="black", fill="grey") + 
  xlab(NULL) +
  ylab("Number of articles") +   theme_bw()
ggsave("Fig1numArticlesYear.png", width=6, height=4)



geoState<-tabyl(quillData$stateLead)
names(geoState)<-c("state", "n", "percent1", "percent2")
geoState<-subset(geoState, n>=2)
ggplot(subset(geoState, !is.na(state)), aes(x=reorder(state, n), y=n)) + 
  geom_bar(stat="identity", position="dodge", color="black", fill="grey") + 
  xlab("Geo-location of lead author (all papers)") +
  ylab("frequency") + 
  theme_bw() + coord_flip() + scale_y_continuous(breaks=seq(0,30,2))
ggsave("stateLeadAllPapers.png", width = 6, height = 4, units="in")

GQgeoState<-tabyl(quillDat1$stateLead)
names(GQgeoState)<-c("state", "n", "percent1")#, "percent2")
GQgeoState<-subset(GQgeoState, n>=1)
ggplot(subset(GQgeoState, !is.na(state)), aes(x=reorder(state, n), y=n)) + 
  geom_bar(stat="identity", position="dodge", color="black", fill="grey") + 
  xlab("Geo-location of lead author (Gold Quill papers)") +
  ylab("frequency") + 
  theme_bw() + coord_flip() + scale_y_continuous(breaks=seq(0,30,1))
ggsave("Fig10GQstateLead.png", width = 6, height = 4, units="in")


#### plots authors as GQ and all papers
allAuthors0<-c(quillData$author1, quillData$author2, quillData$author3, quillData$author4, quillData$author5, quillData$author6, quillData$author7, quillData$author8, quillData$author9, quillData$author10)
allAuthors1<-subset(allAuthors0, is.na(allAuthors0)!=TRUE)
allAuthors2<-tabyl(allAuthors1)

allAuthors3<-allAuthors2
names(allAuthors3)<-c("author", "all", "percent")

GQAuthors0<-c(quillDat1$author1, quillDat1$author2, quillDat1$author3, quillDat1$author4, quillDat1$author5, quillDat1$author6, quillDat1$author7, quillDat1$author8, quillDat1$author9, quillDat1$author10)
GQAuthors1<-subset(GQAuthors0, is.na(GQAuthors0)!=TRUE)
GQAuthors2<-tabyl(GQAuthors1)

GQAuthors3<-GQAuthors2
names(GQAuthors3)<-c("author", "win", "percent")


allAuthors4<-full_join(allAuthors3, GQAuthors3, by="author")

ggplot(allAuthors4, aes(x=author)) + 
  geom_bar(aes(x=reorder(author,all), y=all), stat="identity", col="black", fill="grey") +
  geom_bar(aes(x=author, y= win), stat="identity", col="black", fill="gold") +
  scale_y_continuous(breaks=seq(0,50,5)) +
  ylab("number of papers") + xlab(NULL) +
  theme_bw() + coord_flip()
ggsave("AllAuthorsAllPapers.png", width=6, height=16)


#### text mining

# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- quillDat0$Title
text0 <- quillData$Title
text1 <-quillDat1$Title

docs <- Corpus(VectorSource(text))
inspect(docs)
docs1 <- Corpus(VectorSource(text1))
docs0 <- Corpus(VectorSource(text0))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, "\\|")
inspect(docs)

docs0 <- tm_map(docs0, toSpace, "/")
docs0 <- tm_map(docs0, toSpace, "@")
docs0 <- tm_map(docs0, toSpace, ",")
docs0 <- tm_map(docs0, toSpace, "\\|")
inspect(docs0)

docs1 <- tm_map(docs1, toSpace, "/")
docs1 <- tm_map(docs1, toSpace, "@")
docs1 <- tm_map(docs1, toSpace, ",")
docs1 <- tm_map(docs1, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
docs1 <- tm_map(docs1, content_transformer(tolower))
docs0 <- tm_map(docs0, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
docs0 <- tm_map(docs0, removeNumbers)
docs1 <- tm_map(docs1, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs0 <- tm_map(docs0, removeWords, stopwords("english"))
docs1 <- tm_map(docs1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
docs0 <- tm_map(docs0, removePunctuation)
docs1 <- tm_map(docs1, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
docs0 <- tm_map(docs0, stripWhitespace)
docs1 <- tm_map(docs1, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)
#docs0 <- tm_map(docs0, stemDocument)
#docs1 <- tm_map(docs1, stemDocument)

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "farms", replacement = "farm")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "farms", replacement = "farm")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "farms", replacement = "farm")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "values", replacement = "value")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "values", replacement = "value")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "values", replacement = "value")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "economics", replacement = "economic")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "economics", replacement = "economic")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "economics", replacement = "economic")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "costs", replacement = "cost")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "costs", replacement = "cost")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "costs", replacement = "cost")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "prices", replacement = "price")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "prices", replacement = "price")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "prices", replacement = "price")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "farmers", replacement = "farmer")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "farmers", replacement = "farmer")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "farmers", replacement = "farmer")))


docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "valuing", replacement = "value")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "valuing", replacement = "value")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "valuing", replacement = "value")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "using", replacement = "use")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "using", replacement = "use")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "using", replacement = "use")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "technologies", replacement = "technology")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "technologies", replacement = "technology")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "technologies", replacement = "technology")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "soybeans", replacement = "soybean")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "soybeans", replacement = "soybean")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "soybeans", replacement = "soybean")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "rate", replacement = "rates")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "rate", replacement = "rates")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "rate", replacement = "rates")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "producing", replacement = "production")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "producing", replacement = "production")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "producing", replacement = "production")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "pricing", replacement = "price")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "pricing", replacement = "price")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "pricing", replacement = "price")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "marketing", replacement = "market")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "marketing", replacement = "market")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "marketing", replacement = "market")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "managing", replacement = "management")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "managing", replacement = "management")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "managing", replacement = "management")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "managers", replacement = "management")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "managers", replacement = "management")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "managers", replacement = "management")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "leases", replacement = "lease")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "leases", replacement = "lease")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "leases", replacement = "lease")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "impacts", replacement = "impact")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "impacts", replacement = "impact")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "impacts", replacement = "impact")))


docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "crop", replacement = "crops")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "crop", replacement = "crops")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "crop", replacement = "crops")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "cropss", replacement = "crops")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "cropss", replacement = "crops")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "cropss", replacement = "crops")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "changing", replacement = "changes")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "changing", replacement = "changes")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "changing", replacement = "changes")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "appraisers", replacement = "appraisal")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "appraisers", replacement = "appraisal")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "appraisers", replacement = "appraisal")))


docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "agricultural", replacement = "agriculture")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "agricultural", replacement = "agriculture")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "agricultural", replacement = "agriculture")))

docs<-tm_map(docs, content_transformer(function(x) 
  gsub(x, pattern = "producers", replacement = "farmer")))
docs0<-tm_map(docs0, content_transformer(function(x) 
  gsub(x, pattern = "producers", replacement = "farmer")))
docs1<-tm_map(docs1, content_transformer(function(x) 
  gsub(x, pattern = "producers", replacement = "farmer")))

inspect(docs)
inspect(docs0)
inspect(docs1)


dtm <- TermDocumentMatrix(docs)
inspect(dtm[1:5, 1:33])
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

dtm0 <- TermDocumentMatrix(docs0)
inspect(dtm0[1:5, 1:33])
m0 <- as.matrix(dtm0)
m0
v0 <- sort(rowSums(m0),decreasing=TRUE)
d0 <- data.frame(word = names(v0),freq=v0)
head(d0, 100)


dtm1 <- TermDocumentMatrix(docs1)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
head(d1, 10)


set.seed(1234)
png("allWords.png", width=800, height=800)
wordcloud(words = d0$word, freq = d0$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.4, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


d03<-subset(d0, freq>=3)
ggplot(d03, aes(x=reorder(word, freq), y=freq)) +   geom_bar(stat="identity") + 
  xlab(NULL) + ylab("frequency count") + 
  scale_y_continuous(breaks=seq(0,50,1)) +
  theme_bw() + coord_flip()
ggsave("titleWordFreq3.png", width=6, height=18, units="in")

d04<-subset(d0, freq>=4)
ggplot(d04, aes(x=reorder(word, freq), y=freq)) +   geom_bar(stat="identity") + 
  xlab(NULL) + ylab("frequency count") + 
  scale_y_continuous(breaks=seq(0,50,1)) +
  theme_bw() + coord_flip()
ggsave("titleWordFreq0.png", width=6, height=4, units="in")

d02<-subset(d0, freq>=2)
ggplot(d02, aes(x=reorder(word, freq), y=freq)) +   geom_bar(stat="identity") + 
  xlab(NULL) + ylab("frequency count") +
  scale_y_continuous(breaks=seq(0,50,1)) +
  theme_bw() + coord_flip()
ggsave("titleWordFreq2.png", width=6, height=18, units="in")


###############
#creates graph of commonly used words in titles

d1$quill<-"won"
d$quill<-"all"
d1$freqGQ<-d1$freq
d1$winner<-"winner"
d0$freqAll<-d0$freq
d0$winner<-"NotWinner"
dall<-full_join(d1,d0, by="word")
head(dall)
dall$freq<-dall$freq.x+dall$freq.y
dall<-subset(dall, freqAll>=9)
ggplot(dall) + 
  geom_bar(aes(x=reorder(word, freqAll), y=freqAll), stat="identity", col="black", fill="grey") + 
  geom_bar(aes(x=word, y=freqGQ), stat="identity", col="black", fill="gold") + 
  xlab(NULL) + ylab("word frequency in titles of all articles (Gold Quill in gold)") + theme_bw() + coord_flip()
ggsave("Fig7wordCountAllPapers.png", width=6, height=5)

ggplot(dall) + 
  geom_bar(aes(x=word, y=freqAll), stat="identity", col="black", fill="grey") + 
  geom_bar(aes(x=word, y=freqGQ), stat="identity", col="black", fill="gold") + 
  xlab(NULL) + ylab("word frequency in titles of all articles (Gold Quill in gold)") + theme_bw() + coord_flip()
ggsave("testFig7wordCountAllPapers.png", width=6, height=40)
