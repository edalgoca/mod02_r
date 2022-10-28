library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)

options(mc.cores=1)

path = getwd()

dir = DirSource(paste(path,"/",sep=""), encoding = "UTF-8")
corpus = Corpus(dir)

length(corpus)

myStopwords = c(stopwords(),"film","films","movie","movies")

tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = myStopwords,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]

freq = sort(rowSums(as.matrix(tdm)), decreasing = T)
set.seed(1234)

word.cloud=wordcloud(words=names(freq), freq=freq,min.freq=400, random.order=F, colors=pal)
