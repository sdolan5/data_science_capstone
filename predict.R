# SET WORKING DIRECTORY
setwd("C:/Users/Sean/Desktop/fresh_start/data_files")

# PACKAGES
library(tm)
library(stringi)

# READ IN TABLE OBJECTS
unigramProbabilityNS <- readRDS("uniProbNS.rds")
bigramProbabilityNS <- readRDS("biProbNS.rds")
trigramProbabilityNS <- readRDS("triProbNS.rds")
quadProbabilityNS <- readRDS("quadProbNS.rds")


# Function to take user_input, tokenize and return length
getLength <- function(x) {
  user_input <- x
  input_token <- scan_tokenizer(user_input)
  input_length <- length(input_token)
  return(input_length)
}

# Function to reformat user input
fixInput <- function(user_input){
  #user_input <- x
  input_length <- getLength(user_input)
  # CASE: User does not enter a word or phrase
  if(is.null(user_input)){
    return("Please enter a phrase at least one word long.")
  }else{
    # CASE: User enters a phrase more than three words long
    if(input_length > 3){
      # take phrase apart
      input_token <- scan_tokenizer(user_input)
      # choose only the last three words
      input_token <- input_token[(length(input_token) -2) :length(input_token)]
      # put back together
      fixed_input <- paste(input_token, collapse = " ")
    }else{
      # CASES: User enters phrase of one, two, or three words
      user_input <- stri_trans_general(user_input, "latin-ascii")
      # If "space" character is not part of set, or words will run together
      charfixer <- function(x){x <- gsub("[^a-zA-Z0-9',-:;.!? ]", "", x)}
      user_input <- charfixer(user_input)
      fixed_input <- stri_trans_tolower(user_input)
    } 
  }
  return  (fixed_input)
}

ngramSet <- function(user_input) { 
  input_length <- getLength(user_input)
  if(input_length == 1){
    ngramSet <- bigramProbabilityNS
  } else if(input_length == 2){
    ngramSet <- trigramProbabilityNS
  } else if(input_length >= 3){
    ngramSet <-  quadgramProbabilityNS
  }
  return(ngramSet)
}

# Function takes user input and returns list with fixed input and length
processInput <- function(x) {
  fixed_input <- vector("list", 2)
  # Fix the input
  fixed_input[1] <- fixInput(x)
  # Use input to get length
  fixed_input[2] <- as.numeric(getLength(fixed_input[1]))
  return(fixed_input)
} 



# Function to chop trigram to bigram, bigram to unigram
chopOne <- function(x) {
  # take phrase apart
  chop_token <- scan_tokenizer(x)
  # choose only the last three words
  chop_token2 <- chop_token[-1]
  # put back together
  chopped_input <- paste(chop_token2, collapse = " ")
  return(chopped_input)
} 


# PREDICTION FUNCTIONS

predict3 <- function(x) {
  # Reduce users' input to the last three words (trigram)
  fixed_input <- processInput(x)
  print(fixed_input[1])
  # Create subset of trigram matches from the quadgram table
  matched3 <- masterProbabilityNS[quadProbabilityNS$first3 == toString(fixed_input[1]), ]
  print("Checking quadgrams for matches")
  # Check to see if there is a match for the phrase in the trigram table
  if(nrow(matched3) > 0) {
    # return trigram prediction (last word with highest probability)
    predicted_word <- matched3$last[1]
    print(matched3)
    print("A match was found in the quadgram set")
    return(predicted_word)
  } else {
    print("No match at quadgram level, checking trigrams")
    # Cut a word off the input to make it a bigram
    new_input <- chopOne(fixed_input[1])
    print(new_input)
    # Create subset of bigram matches from the trigram table
    matched2 <- trigramProbabilityNS[trigramProbabilityNS$first2 == toString(new_input), ]
    if(nrow(matched2) > 0){ 
      # return bigram prediction
      predicted_word <- matched2$last[1]
      print("A match was found in the trigram set")
      return(predicted_word)
    } else {
      print("No match at trigram level, checking bigrams")
      # Cut a word off the input to make it a unigram
      new_input2 <- chopOne(new_input)
      print(new_input2)
      # Create subset of unigram matches from the bigram table
      matched1 <- bigramProbabilityNS[bigramProbabilityNS$first == toString(new_input2), ]
      if(nrow(matched1) > 0){
        # return unigram prediction
        predicted_word <- matched1$last[1]
        print("A match was found in the bigram set")
        return(predicted_word)
      } else {
        print("Sorry, no match at the bigram level either")
      }
    }
  }
}


predict2 <- function(x) {
  # Clean user input, which was exactly two words
  fixed_input <- processInput(x)
  print(fixed_input[1])
  # Create subset of bigram matches from the trigram table
  print("Checking trigrams for matches")
  matched2 <- trigramProbabilityNS[trigramProbabilityNS$first2 == toString(fixed_input[1]), ]
  # Check to see if there is a match for the phrase in the trigram table
  if(nrow(matched2) > 0){ 
    # return bigram prediction
    predicted_word <- matched2$last[1]
    print("A match was found in the trigram set")
    return(toString(predicted_word))
  } else {
    print("No match at trigram level, checking bigrams")
    # Cut a word off the input to make it a unigram
    new_input <- chopOne(fixed_input[1])
    print(new_input)
    # Create subset of unigram matches from the bigram table
    matched1 <- bigramProbabilityNS[bigramProbabilityNS$first== toString(new_input), ]
    if(nrow(matched1) > 0){
      # return unigram prediction
      predicted_word <- matched1$last[1]
      print("A match was found in the bigram set")
      return(toString(predicted_word))
    } else {
      print("Sorry, no match at the bigram level either")
    }
  }
}


predict1 <- function(x) {
  # Clean user input, which was exactly one word
  fixed_input <- processInput(x)
  print(fixed_input[1])
  print("Checking bigrams for matches")
  # Create subset of unigram matches from the bigram table
  matched1 <- bigramProbabilityNS[bigramProbabilityNS$first == toString(fixed_input[1]), ]
  if(nrow(matched1) > 0){
    # return unigram prediction
    predicted_word <- matched1$last[1]
    print("A match was found in the bigram set")
    return(toString(predicted_word))
  } else {
    print("Sorry, no match found for your phrase.")
  }
}


# Function to determine which Prediction function to use
whichPredict <- function(x) {
  fixed_input <- processInput(x)
  if(fixed_input[2] == 3) {
    print("Using the predict3 function")
    predict3(fixed_input[1])
  } else if(fixed_input[2] == 2) {
    print("Using the predict2 function")
    predict2(fixed_input[1])
  } else {
    print("Using the predict1 function")
    predict1(fixed_input[1])
  } 
}

 
# TESTS
getLength("let's play ball tomorrow night")
fixInput("let's play ball tomorrow night")
processInput("let's play ball tomorrow night")
chopOne("let's play ball tomorrow night")
whichPredict("our new york city")
whichPredict("dog")

