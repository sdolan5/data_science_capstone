# SET WORKING DIRECTORY
setwd("C:/Users/Sean/Desktop/Coursera")

# READ IN sample object from file
setwd("C:/Users/Sean/Desktop/fresh_start/data_files")
# Note: Stopwords and profanties were already removed in "clean_data.R"
allSampleClean <- readRDS("allSampleClean.rds")

# USE QUANTEDA package for Ngram Tokenization
library(quanteda)

# First, convert to a quanteda corpus object, because it will not handle
# the tm VCorpus object created in file "clean_data2.R"
quant_corpus <- corpus(allSampleClean)


# Change ngrams parameter to get unis, bis, tris, quads
allTokenizedUnigrams <- tokenize(quant_corpus, what ="word", removeNumbers=TRUE, removePunct = TRUE, 
                               removeTwitter =TRUE, removeSymbols= TRUE, ngrams =1)

allTokenizedBigrams <- tokenize(quant_corpus, what ="word", removeNumbers=TRUE, removePunct = TRUE, 
                                 removeTwitter =TRUE, removeSymbols= TRUE, ngrams =2, concatenator = " ")

allTokenizedTrigrams <- tokenize(quant_corpus, what ="word", removeNumbers=TRUE, removePunct = TRUE, 
                                removeTwitter =TRUE, removeSymbols= TRUE, ngrams = 3, concatenator = " ")

allTokenizedQuadgrams <- tokenize(quant_corpus, what ="word", removeNumbers=TRUE, removePunct = TRUE, 
                                removeTwitter =TRUE, removeSymbols= TRUE, ngrams = 4, concatenator = " ")


# CREATE BASIC FREQUENCY TABLES
# First, the Unigrams
uni_quant_matrix <- dfm(allTokenizedUnigrams)
uni_freqTableNS <- data.frame(ngram = features(uni_quant_matrix), frequency = colSums(uni_quant_matrix),
                            row.names = NULL, stringsAsFactors = FALSE)

# Reorder dataframe by frequency column
uni_freqTableNS <- uni_freqTableNS[order(-uni_freqTableNS$frequency), ]

# Reorder dataframe by name, then frequency column
uni_freqTableNS <- uni_freqTableNS[order(uni_freqTableNS$ngram, 
                                     -uni_freqTableNS$frequency), ]

# The Bigrams
bi_quant_matrix <- dfm(allTokenizedBigrams)
bi_freqTableNS <- data.frame(ngram = features(bi_quant_matrix), frequency = colSums(bi_quant_matrix),
                           row.names = NULL, stringsAsFactors = FALSE)

# Reorder dataframe by name, then frequency column
bi_freqTableNS <- bi_freqTableNS[order(bi_freqTableNS$ngram, 
                                   -bi_freqTableNS$frequency), ]

# The Trigrams
tri_quant_matrix <- dfm(allTokenizedTrigrams)
tri_freqTableNS <- data.frame(ngram = features(tri_quant_matrix), frequency = colSums(tri_quant_matrix),
                            row.names = NULL, stringsAsFactors = FALSE)

# Reorder dataframe by name, then frequency column
tri_freqTableNS <- tri_freqTableNS[order(tri_freqTableNS$ngram, 
                                     -tri_freqTableNS$frequency), ]

# The Quadgrams
quad_quant_matrix <- dfm(allTokenizedQuadgrams)
quad_freqTableNS <- data.frame(ngram = features(quad_quant_matrix), frequency = colSums(quad_quant_matrix),
                             row.names = NULL, stringsAsFactors = FALSE)

# Reorder dataframe by name, then frequency column
quad_freqTableNS <- quad_freqTableNS[order(quad_freqTableNS$ngram, 
                                       -quad_freqTableNS$frequency), ]

## SAVE THESE (MOSTLY) CLEANED N-GRAM FREQ TABLES
setwd("C:/Users/Sean/Desktop/fresh_start/data_files")

saveRDS(uni_freqTableNS, "uni_freqTableNS.rds")
saveRDS(bi_freqTableNS, "bi_freqTableNS.rds")
saveRDS(tri_freqTableNS, "tri_freqTableNS.rds")
saveRDS(quad_freqTableNS, "quad_freqTableNS.rds")




