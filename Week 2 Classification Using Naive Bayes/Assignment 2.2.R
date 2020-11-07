# Step 1 – collecting data
## To develop the Naive Bayes classifier, we will use data adapted from the Roman Urdu Data Set Data Set at http://archive.ics.uci.edu/ml/datasets/Roman+Urdu+Data+Set


# Step 2 – exploring and preparing the data
ru_raw <- read.csv("Roman_Urdu_DataSet.csv", stringsAsFactors = FALSE)
str(ru_raw)

set.seed(9850)
gp <- runif(nrow(ru_raw))
ru_raw <- ru_raw[order(gp), ]

ru_raw$sentiment <- factor(ru_raw$sentiment)
str(ru_raw$sentiment)
table(ru_raw$sentiment)


## Data preparation – cleaning and standardizing text data
library(tm)
ru_corpus <- VCorpus(VectorSource(ru_raw$comment))
print(ru_corpus)

ru_corpus_clean <- tm_map(ru_corpus,
                           content_transformer(tolower))

ru_corpus_clean <- tm_map(ru_corpus_clean, removeNumbers)
ru_corpus_clean <- tm_map(ru_corpus_clean,
                           removeWords, stopwords())
ru_corpus_clean <- tm_map(ru_corpus_clean, removePunctuation)
library(SnowballC)
ru_corpus_clean <- tm_map(ru_corpus_clean, stemDocument)
ru_corpus_clean <- tm_map(ru_corpus_clean, stripWhitespace)

## Data preparation – splitting text documents into words
ru_dtm <- DocumentTermMatrix(ru_corpus_clean)

## Data preparation – creating training and test datasets
ru_dtm_train <- ru_dtm[1:8475, ]
ru_dtm_test <- ru_dtm[8476:11300, ]

ru_train_labels <- ru_raw[1:8475, ]$sentiment
ru_test_labels <- ru_raw[8476:11300, ]$sentiment

prop.table(table(ru_train_labels))
prop.table(table(ru_test_labels))

## Visualizing text data – word clouds
library(wordcloud)
wordcloud(ru_corpus_clean, min.freq = 50, random.order = FALSE)

neg <- subset(ru_raw, sentiment == "Negative")
pos <- subset(ru_raw, sentiment == "Positive")
wordcloud(neg$comment, max.words = 40, scale = c(3, 0.5))
wordcloud(pos$comment, max.words = 40, scale = c(3, 0.5))

## Data preparation – creating indicator features for frequent words
ru_freq_words <- findFreqTerms(ru_dtm_train, 5)
str(ru_freq_words)

ru_dtm_freq_train <- ru_dtm_train[ , ru_freq_words]
ru_dtm_freq_test <- ru_dtm_test[ , ru_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

ru_train <- apply(ru_dtm_freq_train, MARGIN = 2,
                   convert_counts)
ru_test <- apply(ru_dtm_freq_test, MARGIN = 2,
                  convert_counts)


# Step 3 – training a model on the data
library(e1071)
ru_classifier <- naiveBayes(ru_train, ru_train_labels)


# Step 4 – evaluating model performance
ru_test_pred <- predict(ru_classifier, ru_test)

library(gmodels)
CrossTable(ru_test_pred, ru_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))


# Step 5 – improving model performance
ru_classifier2 <- naiveBayes(ru_train, ru_train_labels,
                              laplace = 1)
ru_test_pred2 <- predict(ru_classifier2, ru_test)
CrossTable(ru_test_pred2, ru_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

