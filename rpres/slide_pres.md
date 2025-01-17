Finish My Thought
========================================================
author: Sean Dolan
date: January 1, 2017
autosize: true
css: slide_pres.css

Meeting a Need
========================================================

As we increasingly rely on mobile devices to stay connected and manage our "personal brands" on numerous social network sites every day, the sheer number of communications we are sending out daily has steadily increased. Our time is valuable to us, so there is a need for an application that is able to accurately and reliably complete our sentences for us so that we don't have to type everything out manually. 

To solve this problem, we created a generative language model based on a random sampling of three corpora: news articles, blog posts, and tweets. 

Next, a series of preprocessing operations were performed: 

- Conversion of text to ascii characters and lowercase
- Removal of whitespace, numbers, and punctuation
- Removal of stopwords and a custom list of profanities


N-Gram Tokenization
========================================================

After this, the data was ran through n-gram tokenizers (unigram, bigram, trigram, and quadgram) and then converted to a Term Document Matrix, followed by a Sparse Document Matrix (SDM). The SDM removes all the terms that occur below a certain frequency threshold.  Finally, a Term Frequency Table was created for each group of n-grams. 

Initially, this process was completed using the <strong>tm</strong> and <strong>Rweka</strong> libraries. Removing sparse entries made the size of the frequency tables more manageable, but resulted in a significant increase in the number of n-grams that would ultimately be inaccessible to the prediction algorithm (<i>SEE: Dealing With Unseen Words"</i>). A solution was found in the form of the <strong>quanteda</strong> library. Not only does the library have its own tokenize function, it is able to efficiently process very large matrices without removing sparse entries.


```r
# Change ngrams parameter to get bis, tris, quads
allTokenizedBigrams <- tokenize(quant_corpus, what ="word", removeNumbers=TRUE, removePunct = TRUE, removeTwitter =TRUE, removeSymbols= TRUE, ngrams =2, concatenator = " ")

allTokenizedTrigrams <- tokenize(quant_corpus, what ="word", removeNumbers=TRUE, removePunct = TRUE, removeTwitter =TRUE, removeSymbols= TRUE, ngrams = 3, concatenator = " ")
```

Tables and "Back- Off" Method
========================================================

Ultimately, all of this data is going to be used in an application, so we have to take into account that users expect queries to be processed within a second.  So, the decision was made to compile tables ahead of time, which will be loaded when the app starts up. This will enable an algorithm to use a "back-off method' to determine the most likely next word.  

The algorithm will first check if the first "n" words in the user's query can be completed with one ore more ngrams observed in the training data. 
In other words, if the user enters a three word phrase, the algorithm will check the table to see if there are any quadgrams which complete it. If there are none, then the model "backs off". It "chops" the first word off of the user's query, making it a two word phrase, then searches to see if there are any trigrams which complete it. If necessary, this is repeated once more, with only the last word in the user's query considered and the candidate bigrams searched.

Dealing with Unseen Words
========================================================

By far the biggest challenge in developing a phrase completion application was dealing with words and ngrams that were not seen in the training dataset.  In cases when a user's query contains these unseen words and phrases, it is not possible to return an answer.

One solution that was tried was "borrowing" probability mass from the more common words observed in the dataset, through one of several smoothing methods, including LaPlace and Good Turing.  

For example, LaPlace smoothing was attempted by adding 1 to every word countin the Term Document Matrix (including those words that had zero counts) and then normalizing, but this methods proved too difficult to implement due to memory limitations. 

In the end, we were able to create an app that runs quickly and which is functional.  The user simply types a phrase of one or more words into the input box and hits the predict button.  Unless the phrase contains very rare words, a reasonable predicition will usually be found by the time the algorithm has "backed off" to the bigram level.
