# SET WORKING DIRECTORY
setwd("C:/Users/Sean/Desktop/fresh_start/data_files")

# PACKAGES
library(tm)
library(stringi)

# READ IN basic table objects from file
# Note: Created in file "quanteda.R"

uni_freqTableNS <- readRDS("uni_freqTableNS.rds")
bi_freqTableNS <- readRDS("bi_freqTableNS.rds")
tri_freqTableNS <- readRDS("tri_freqTableNS.rds")
quad_freqTableNS <- readRDS("quad_freqTableNS.rds")


### THE BIGRAMS

# Function to tokenize and pull first word from each bigram
bigramFirst <- function(x){
  bigram_token <- scan_tokenizer(x)
  bigram_first <- bigram_token[1]
  return(bigram_first)
}

# Rename unigram frequency table columns to "first" and "freq1"
names(uni_freqTableNS)[names(uni_freqTableNS) =="ngram"] <- "first"
names(uni_freqTableNS)[names(uni_freqTableNS) =="frequency"] <- "freq1"

# Add column for probability of each unigram
# Note - Will underflow be a problem???
uni_freqTableNS$prob1 <- uni_freqTableNS$freq1 / sum(uni_freqTableNS$freq1)

# Rename bigram frequency table columns to "first2" and "freq2"
names(bi_freqTableNS)[names(bi_freqTableNS) =="ngram"] <- "first2"
names(bi_freqTableNS)[names(bi_freqTableNS) =="frequency"] <- "freq2"

# Add column to bigram frequency table with first words
bi_freqTableNS$first <- sapply(bi_freqTableNS$first2, bigramFirst)

# Merge the two dataframes on the column "first"
uni_and_bigrams <- merge(uni_freqTableNS,bi_freqTableNS,by="first")

# Calculate the conditional probability of each bigram
# Note - Will underflow be a problem???
uni_and_bigrams$prob2 <- uni_and_bigrams$freq2 / uni_and_bigrams$freq1

# Make new dataframe (much neater) - take only word, freq, prefix
bigramProbabilityNS <- data.frame(uni_and_bigrams$first, uni_and_bigrams$freq1, 
                                uni_and_bigrams$prob1, uni_and_bigrams$first2, 
                                uni_and_bigrams$freq2, uni_and_bigrams$prob2)
colnames(bigramProbabilityNS) = c("first", "freq1", "prob1", "first2", "freq2", "prob2")         

# Re-order table by unigram column, then bigram frequency column
bigramProbabilityNS <- bigramProbabilityNS[order(bigramProbabilityNS$first, 
                             -bigramProbabilityNS$freq2),]


# Save ProbabilityNS tables to file as rds object
saveRDS(uni_freqTableNS, "uniProbNS.rds")
saveRDS(bigramProbabilityNS, "biProbNS.rds")


### THE TRIGRAMS

# Function to tokenize and pull first two words from each trigram
trigramFirstTwo <- function(x){
  trigram_token <- scan_tokenizer(x)
  trigram_first_two <- trigram_token[1:2]
  trigram_first_two <- paste(trigram_token[1], trigram_token[2], sep = " ")
  return(trigram_first_two)
}

# Rename trigram frequency table columns to "first3" and "freq3"
names(tri_freqTableNS)[names(tri_freqTableNS) =="ngram"] <- "first3"
names(tri_freqTableNS)[names(tri_freqTableNS) =="frequency"] <- "freq3"

# Add column to trigram frequency table with first two words
tri_freqTableNS$first2 <- sapply(tri_freqTableNS$first3,trigramFirstTwo)

# Merge the two dataframes on the column "first2"
bi_and_trigrams <- merge(bigramProbabilityNS,tri_freqTableNS,by="first2")

# Calculate the conditional probability of each trigram
bi_and_trigrams$prob3 <- bi_and_trigrams$freq3 / bi_and_trigrams$freq2

# Make new dataframe (much neater) - take only word, freq, prefix
trigramProbabilityNS <- data.frame(bi_and_trigrams$first, bi_and_trigrams$freq1, 
                                 bi_and_trigrams$prob1, bi_and_trigrams$first2, 
                                 bi_and_trigrams$freq2, bi_and_trigrams$prob2,
                                 bi_and_trigrams$first3, bi_and_trigrams$freq3,
                                 bi_and_trigrams$prob3)
colnames(trigramProbabilityNS) = c("first", "freq1", "prob1", "first2", 
                                "freq2", "prob2", "first3", "freq3", 
                                "prob3")

# Re-order table by bigram column, then trigram frequency column
trigramProbabilityNS <- trigramProbabilityNS[order(trigramProbabilityNS$first2, 
                                                 -trigramProbabilityNS$freq3),]

# Save ProbabilityNS table to file as rds object
saveRDS(trigramProbabilityNS, "triProbNS.rds")

#### THE QUADGRAMS

# Function to tokenize and pull first three words from each quadgram
quadgramFirstThree <- function(x){
  quadgram_token <- scan_tokenizer(x)
  quadgram_first_three <- quadgram_token[1:3]
  quadgram_first_three <- paste(quadgram_token[1], quadgram_token[2], 
                                quadgram_token[3], sep = " ")
  return(quadgram_first_three)
}

# Rename quadgram frequency table columns to "first4" and "freq4"
names(quad_freqTableNS)[names(quad_freqTableNS) =="ngram"] <- "first4"
names(quad_freqTableNS)[names(quad_freqTableNS) =="frequency"] <- "freq4"

# Add column to quadgram frequency table with first three words
quad_freqTableNS$first3 <- sapply(quad_freqTableNS$first4,quadgramFirstThree)

# Merge the two dataframes on the column "first_three"
tri_and_quadgrams <- merge(trigramProbabilityNS,quad_freqTableNS,by="first3")

# Calculate the conditional probability of each quadgram
tri_and_quadgrams$prob4 <- tri_and_quadgrams$freq4 / tri_and_quadgrams$freq3

# Make new dataframe (much neater) - take only word, freq, prefix
quadgramProbabilityNS <- data.frame(tri_and_quadgrams$first, tri_and_quadgrams$freq1, 
                                  tri_and_quadgrams$prob1, tri_and_quadgrams$first2, 
                                  tri_and_quadgrams$freq2, tri_and_quadgrams$prob2,
                                  tri_and_quadgrams$first3, tri_and_quadgrams$freq3,
                                  tri_and_quadgrams$prob3, tri_and_quadgrams$first4,
                                  tri_and_quadgrams$freq4, tri_and_quadgrams$prob4)
colnames(quadgramProbabilityNS) = c("first", "freq1", "prob1", "first2", 
                                 "freq2", "prob2", "first3", "freq3", 
                                 "prob3", "first4", "freq4", "prob4")

# Re-order table by trigram column, then quadgram frequency column
quadgramProbabilityNS <- quadgramProbabilityNS[order(quadgramProbabilityNS$first3, 
                                                   -quadgramProbabilityNS$freq4),]


# Save ProbabilityNS table to file as rds object
saveRDS(quadgramProbabilityNS, "quadProbNS.rds")

# READ OBJECTS BACK IN
unigramProbabilityNS <- readRDS("uniProbNS.rds")
bigramProbabilityNS <- readRDS("biProbNS.rds")
trigramProbabilityNS <- readRDS("triProbNS.rds")
quadgramProbabilityNS <- readRDS("quadProbNS.rds")

########## NEW EXPERIMENT

# Simply copy quad table to a new object and call it "master"
masterProbabilityNS <- quadgramProbabilityNS

# HELPER FUNCTIONS to find endings of ngrams

getLast <- function(x){
  ngram_token <- scan_tokenizer(x)
  last <- ngram_token[length(ngram_token)]
  return(toString(last))
}

getLast2 <- function(x){
  ngram_token <- scan_tokenizer(x)
  last2 <- paste(ngram_token[length(ngram_token)-1], 
                 ngram_token[length(ngram_token)])
  return(toString(last2))
}

getLast3 <- function(x){
  ngram_token <- scan_tokenizer(x)
  last3 <- paste(ngram_token[length(ngram_token)-2],
                 ngram_token[length(ngram_token)-1], 
                 ngram_token[length(ngram_token)])
  return(toString(last3))
}

# Add new columns "last" with the "n-1" ngram for each observation
masterProbabilityNS$last <- sapply(masterProbabilityNS$first4, getLast)
masterProbabilityNS$last2 <- sapply(masterProbabilityNS$first4, getLast2)
masterProbabilityNS$last3 <- sapply(masterProbabilityNS$first4, getLast3)

quadgramProbabilityNS$last <- sapply(quadgramProbabilityNS$first4, getLast)
trigramProbabilityNS$last <- sapply(trigramProbabilityNS$first3, getLast)
bigramProbabilityNS$last <- sapply(bigramProbabilityNS$first2, getLast)

# Save updated tables to file as rds object
saveRDS(masterProbabilityNS, "masterProbNS.rds")
saveRDS(quadgramProbabilityNS, "quadProbNS.rds")
saveRDS(trigramProbabilityNS, "triProbNS.rds")
saveRDS(bigramProbabilityNS, "biProbNS.rds")
