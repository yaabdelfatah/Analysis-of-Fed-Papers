###Load Relevant Packages
library(tm)
library(wordcloud)
library(factoextra)
library(rstatix)

###Load Fed Papers into a corpus
FedPapersCorpus <- Corpus(DirSource("FedPapers"))
ndocs<-length(FedPapersCorpus)

###Check that all documents are loaded
summary(FedPapersCorpus)

###ignore extremely rare words
minTermFreq <- ndocs * 0.0001

###ignore extremely common words
maxTermFreq <- ndocs * 1

###Create a Document Term Matrix
FedPapersDTM <- DocumentTermMatrix(FedPapersCorpus, 
            control = list(
              stopwords=TRUE, 
              wordLength= c(4,10), 
              removePunctuation= TRUE, 
              removeNumbers= TRUE, 
              tolower=TRUE, 
              remove_separators=TRUE, 
              stopwords("english"), 
              bounds=list(global = c(minTermFreq, maxTermFreq))
              ))

###Create a Labled Matrix with Words as Column Names and Rows as Each Paper
FedPapersMatrix <- as.matrix(FedPapersDTM)

###Look at Word Frequencies
WordFreq <- colSums(FedPapersMatrix)
head(WordFreq)

###Order Word Frequency
ord<- order(WordFreq)

###Print 10 Most Frequent Words
WordFreq[tail(ord, n=10)]

###Print 10 Least Frequent Words (this seems less useful because lots of words only used once)
WordFreq[head(ord, n=10)]

###Print Out the Number of Words Per Doc
(Words_Per_Doc <- rowSums(FedPapersMatrix))

###Create a wordcloud for Hamilton

###Create a corpus just for Hamilton. Start by putting all the Hamilton Docs in their own file
HamiltonCorpus <- Corpus(DirSource("Hamilton"))
ndocsH<- length(HamiltonCorpus)

###Create a Document Term Matrix for Hamilton
minTermFreqH <- ndocsH * 0.0001
maxTermFreqH <- ndocsH * 1
HamiltonDTM <- DocumentTermMatrix(HamiltonCorpus, 
                                   control = list(
                                     stopwords=TRUE, 
                                     wordLength= c(4,10), 
                                     removePunctuation= TRUE, 
                                     removeNumbers= TRUE, 
                                     tolower=TRUE, 
                                     remove_separators=TRUE, 
                                     stopwords("english"), 
                                     bounds=list(global = c(minTermFreqH, maxTermFreqH))
                                   ))
###Create Matrix
HamiltonMatrix <- as.matrix(HamiltonDTM)

###Create Wordcloud
wordcloud(colnames(HamiltonMatrix), HamiltonMatrix, min.freq=4)

###Create a wordcloud for Madison. Start by putting all the Madison docs into their own file.

###Create a corpus just for Madison
MadisonCorpus <- Corpus(DirSource("Madison"))
ndocsM<- length(MadisonCorpus)

###Create a Document Term Matrix for Madison
minTermFreqM <- ndocsM * 0.0001
maxTermFreqM <- ndocsM * 1
MadisonDTM <- DocumentTermMatrix(MadisonCorpus, 
                                  control = list(
                                    stopwords=TRUE, 
                                    wordLength= c(4,10), 
                                    removePunctuation= TRUE, 
                                    removeNumbers= TRUE, 
                                    tolower=TRUE, 
                                    remove_separators=TRUE, 
                                    stopwords("english"), 
                                    bounds=list(global = c(minTermFreqM, maxTermFreqM))
                                  ))

###Create Matrix
MadisonMatrix <- as.matrix(MadisonDTM)

###Create Wordcloud
wordcloud(colnames(MadisonMatrix), MadisonMatrix, min.freq=4)

###The two wordclouds suggest that Hamilton and Madison had different priorities in their writing
###despite writing on a similar subject matter

###Clustering the Data
distMatrix_E <- dist(FedPapersMatrix, method="euclidean")
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=4)

###K Means Clustering
distance1 <- get_dist(FedPapersMatrix,method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance2 <- get_dist(FedPapersMatrix,method = "pearson")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance3 <- get_dist(FedPapersMatrix,method = "canberra")
fviz_dist(distance3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance4 <- get_dist(FedPapersMatrix,method = "spearman")
distance4 <- get_dist(FedPapersMatrix,method = "spearman")
fviz_dist(distance4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
Y<- t(FedPapersMatrix)
kmeansFIT_1 <- kmeans(Y,centers=5)
summary(kmeansFIT_1)
fviz_cluster(kmeansFIT_1, data = Y)

###How would you determine who wrote the disputed texts?
###It might be possible to determine who wrote the disputed papers by comparing the word usage
### and word frequency in the disputed texts with that of the other author's




