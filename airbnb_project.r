#The original data set came from Airbnb listing csv
#where information was given about the clients who list their places on Airbnb
#The focus of this analysis is on how those clients describe their homes on the Airbnb website
#Text Mining tools and techniques are used on the description column of the listing data set 
#The goal is to find interesting patterns in what type of words people use to market their homes on Airbnb  

install.packages('tm') #text mining package
install.packages('dplyr') #needed for 'select' function
install.packages("readr") #needed for writing new csv file
install.packages('wordcloud') # highlight the most commonly cited words in a text using a quick visualization
install.packages('SnowballC')#used for stemming words stripping them to their basic forms
library(dplyr)
library(readr)
library(tm)
library(wordcloud)
library(SnowballC)
setwd("~/Desktop")

listings <- read.csv("listings.csv") #original data set
description <- listings %>% select(description) #creates new data frame but only description column will be included 
#creates new csv file based on the new data set
write_csv(description, "Airbnb_location_decriptions.csv") 
#Later the new csv was converted to a txt file in excel
description <- readLines("Airbnb_location_decriptions.txt") #load txt file

#create corpus. Corpora are collections of documents containing (natural language) text.
#VectorSource takes arugment x which represents a vector giving the texts. 
#A vector source interprets each element of the vector x as a document.
#May not explictly state it but this is a simple corpus. It is optimized for the most common usage scenario: importing plain 
#texts from files in a directory or directly from a vector in R, preprocessing and transforming the texts, 
#and finally exporting them to a term-document matrix.
txt_corpus <- Corpus(VectorSource(description)) #Corpus is used for linguistic analysis

#clean corpus
#the tm_map function transforms the corpus based on the arguments given and returns the
#same length charcter vector in this situation since we are using simple corpus function
txt_corpus <- tm_map(txt_corpus, tolower) #makes all words in the text lower case
# gsub() function in R is global replace function, which replaces all instances of the substring not just the first
#or the first argument can be a  regular expression.
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) #remove any non-English words
txt_corpus <- tm_map(txt_corpus, content_transformer(removeNumPunct))
#stop words which are common words that don't add much information 
#stopwords function needs to be set to english
txt_corpus <- tm_map(txt_corpus, removeWords, stopwords("en")) #removes stopwords
txt_corpus <- tm_map(txt_corpus, stripWhitespace) #strip extra whitespace
txt_corpus <- tm_map(txt_corpus, removeNumbers) #remove numbers
txt_corpus <- tm_map(txt_corpus, removePunctuation) #removes punctuations from text
#Stemming is the process of gathering words of similar origin into one word 
#for example “communication”, “communicates”, “communicate”. 
#Stemming helps us increase accuracy in our mined text by removing suffixes and reducing words to their basic forms.
txt_corpus <- tm_map(txt_corpus, stemDocument) 

# Create term document matrix
tdm <- TermDocumentMatrix(txt_corpus) #a matrix with terms as rows and documents as columns 
#the matrix's holds the frequency each term has in a specific document.   
#Remember the vector source function above
#treated each term/element that includes I believe empty space as a document.  

t <- removeSparseTerms(tdm, sparse = 0.90) #some of the cells in the matrix contain zero and we are removing some of them 

m <- as.matrix(t)

# Hierarchical clustering using dendrogram
#Ward's minimum variance method aims at finding compact, spherical clusters. 
#The complete linkage method finds similar clusters. 
#The single linkage method (which is closely related to the minimal spanning tree) adopts a ‘friends of friends’ clustering strategy.
distance <- dist(scale(m))
hc.complete <- hclust(distance, method = "complete")
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
hc <- hclust(distance, method = "ward.D")
plot(hc, main = "Ward.D Linkage", xlab = "", sub = "", cex = .9)
abline(h = 600, col = "red") #cuts the dendrogram to create 4 clusters
hc

# k-means clustering 
set.seed(2)
km.out <- kmeans(m, 3, nstart = 20)
km.out$cluster
km.out2 <- kmeans(m, 2, nstart = 20)
km.out2$cluster
km.out4 <- kmeans(m, 4, nstart = 20)
km.out4$cluster
km.out5 <- kmeans(m, 5, nstart = 20)
km.out5$cluster
km.out$tot.withinss
km.out2$tot.withinss
km.out4$tot.withinss
km.out5$tot.withinss
km.out

number_of_occurances <- rowSums(m) #sum up number of occurances of each word
number_of_occurances <- sort(number_of_occurances, decreasing = TRUE) #sort with the most frequent words appearing first

#plot wordcloud
wordcloud(head(names(number_of_occurances), 45), head(number_of_occurances, 45), scale = c(4, 1))






