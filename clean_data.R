# Set the wroking directory
setwd("C:/Users/Sean/Desktop/Coursera")

# Download the dataset and unzip it
if (!file.exists("Dataset.zip")) {
  fileURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(fileURL, destfile = "Dataset.zip", method = "curl")
  unlink(fileURL)
  unzip("Dataset.zip")
}

# Read in files, making sure to skip NULL values in Twitter file
# Use binary method to deal with missing EOL in News file

blogFile <- file("./final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(blogFile, encoding="UTF-8", skipNul=TRUE)
close(blogFile)

newsFile <- file("./final/en_US/en_US.news.txt", "rb")
news <- readLines(newsFile, encoding="UTF-8", skipNul=TRUE)
close(newsFile)

tweetFile <- file("./final/en_US/en_US.twitter.txt", "rb")
tweets<- readLines(tweetFile, encoding="UTF-8", skipNul=TRUE)
close(tweetFile)


# Sample the data
set.seed(729)
blogSample <- sample(blogs, 10000)
newsSample <- sample(news, 10000)
tweetSample <- sample(tweets, 10000)   
allSample <- c(blogSample, newsSample, tweetSample)  

# Remove large objects to save memory
rm(blogs)
rm(news)
rm(tweets)

# Cleaning the Data

# Use stringi package to remove latin characters
library(stringi)
allSample2 <- stri_trans_general(allSample, "latin-ascii")

# Use custom function to remove all odd characters
charfixer <- function(x){x <- gsub("[^a-zA-Z0-9',-:;.!?]", " ", x)}
allSample3 <- charfixer(allSample2)

# Make all characters lowercase
allSample4 <- stri_trans_tolower(allSample3)

# Load profanities file
con <- "C:/Users/Sean/Desktop/fresh_start/profanities.txt"
profanities <- readLines(con, -1)

cleanCorpus <- function(corpus){
  # Helper function to preprocess corpus
  library(tm)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, profanities)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Frequency calculation function
frequency <- function(tdm){
  # Helper function to tabulate frequency
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  frequency <- data.frame(word=names(freq), freq=freq)
  return(frequency)
}

# Create Corpus
library(tm)
allSampleCorpus <- VCorpus(VectorSource(allSample4))

# Run cleaning function on corpus
allSampleClean <- cleanCorpus(allSampleCorpus)

# Create Term Document Matrix
# 50870 terms, 30,000 documents
allSampleTDM <- TermDocumentMatrix(allSampleClean)

# Remove sparse terms
# terms: 2486, documents: 30000
allSampleSparse <- removeSparseTerms(allSampleTDM, 0.999)

# Calculate term frequencies
# max freq: 2953 ("said"), min freq:31 (numerous terms)
allSampleFrequency <- frequency(allSampleSparse)

# N-GRAM TOKENIZATION

# The Bigrams
# Custom Function using RWeka package for NGramTokenizers
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

# Create Term Document Matrix
# terms: 381781, documents: 30000
allSampleTDM2 <- TermDocumentMatrix(allSampleClean, control=list(tokenize=BigramTokenizer))

# Remove all terms which appear less than 0.1% of the documents
# terms: 87, documents: 30000
allSampleSparse2 <- removeSparseTerms(allSampleTDM2, 0.999)

# Calculate bigram frequencies
# max freq: 194 ("new york"), min freq:31 ("sounds like"; "last years")
allSampleFrequency2 <- frequency(allSampleSparse2)

# The Trigrams
# Custom Function using RWeka package for NGramTokenizers
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

# Create Term Document Matrix
# terms: 417527, documents: 30000
allSampleTDM3 <- TermDocumentMatrix(allSampleClean, control=list(tokenize=TrigramTokenizer))

# Remove all terms which appear less than 0.01% of the documents
# terms: 487, documents: 30000
allSampleSparse3 <- removeSparseTerms(allSampleTDM3, 0.9999)

# Calculate trigram frequencies
# max freq: 28 ("new york city"), min freq:3 (numerous terms)
allSampleFrequency3 <- frequency(allSampleSparse3)


# The Quadgrams
# Custom Function using RWeka package for NGramTokenizers
library(RWeka)
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

# Create Term Document Matrix
# terms: 393524, documents: 30000
allSampleTDM4 <- TermDocumentMatrix(allSampleClean, control=list(tokenize=QuadgramTokenizer))

# Remove all terms which appear less than 0.01% of the documents
# terms: 65, documents: 30000
allSampleSparse4 <- removeSparseTerms(allSampleTDM4, 0.9999)

# Calculate quadgram frequencies
# max freq: 8 ("g fat g saturated", "martin luther king jr"), min freq:3 (numerous terms)
allSampleFrequency4 <- frequency(allSampleSparse4)


##### SAVE EVERYTHING AS OBJECTS USING RDS !!!!!
setwd("C:/Users/Sean/Desktop/fresh_start/data_files")

saveRDS(allSampleCorpus, "allSampleCorpus.rds")
saveRDS(allSampleClean, "allSampleClean.rds")
saveRDS(allSampleTDM, "allSampleTDM.rds")
saveRDS(allSampleSparse, "allSampleSparse.rds")
saveRDS(allSampleFrequency, "allSampleFrequency.rds")

saveRDS(allSampleTDM2, "allSampleTDM2.rds")
saveRDS(allSampleSparse2, "allSampleSparse2.rds")
saveRDS(allSampleFrequency2, "allSampleFrequency2.rds")

saveRDS(allSampleTDM3, "allSampleTDM3.rds")
saveRDS(allSampleSparse3, "allSampleSparse3.rds")
saveRDS(allSampleFrequency3, "allSampleFrequency3.rds")

saveRDS(allSampleTDM4, "allSampleTDM4.rds")
saveRDS(allSampleSparse4, "allSampleSparse4.rds")
saveRDS(allSampleFrequency4, "allSampleFrequency4.rds")

saveRDS(allSampleCleanStopsIn, "allSampleCleanStopsIn.rds")

# READ OBJECTS BACK IN
setwd("C:/Users/Sean/Desktop/fresh_start/data_files")
allSampleCorpus <- readRDS("allSampleCorpus.rds") 
allSampleClean <- readRDS("allSampleClean.rds") 
allSampleCorpus <- readRDS("allSampleCorpus.rds") 
allSample <- readRDS("allSample.rds") 

# Ran the cleaning function on corpus, but left stopwords in
allSampleCleanStopsIn <- cleanCorpus(allSampleCorpus)