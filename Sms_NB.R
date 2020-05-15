#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Inlcuding libraries<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
library(naivebayes)
library(dplyr)
library(gmodels)
library(wordcloud)
library(tm)
library(corpus)
library(SnowballC)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Loading the data set<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sms = read.csv(file.choose())
str(sms)

sms$type <- factor(sms$type)
str(sms$type) #Success!

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Cleaning and standardizing text data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sms_corpus <- VCorpus(VectorSource(sms$text))
typeof(sms_corpus) # Just to show that it is a list
print(sms_corpus)
#inspect(sms_corpus)
length(sms_corpus) %>%
  sample(replace = FALSE) %>%
  sort.list(decreasing = FALSE) %>%
  head(2) %>%
  sms_corpus[.] %>%
  inspect()

#wordStem(c("jump", "jumping", "jumped", "jumps"))
#After performing our stemming, we will need to remove the whitespace or blank spaces that are left from the previous preprocessing steps
sms_corpus_clean <- sms_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)


#Now we can look at the change before processing and after processing.
cat("The text document prior processing:", "\n")
for(i in 1:3){
  print(as.character(sms_corpus[[i]]))
}
cat("\n")
cat("The text document after processing:", "\n")
for(i in 1:3){
  print(as.character(sms_corpus_clean[[i]]))
}

#Splitting text documents into words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm_no_prep <- DocumentTermMatrix(
  sms_corpus,
  control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    removePunctuation = TRUE,
    stemming = TRUE
  )
)

#We shall compare the two matrices:
cat("Our matrix with preprocessing:", "\n")
sms_dtm
cat("\n")
cat("Our matrix without preprocessing:", "\n")
sms_dtm_no_prep

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Creating the Train and test dataset.<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- sms[1:4169, ]$type
sms_test_labels <- sms[4170:5559, ]$type

#A proportion table will help show if our test and training sets are both representative of the whole dataset.
cat("Our training data")
sms_train_labels %>%
  table %>%
  prop.table
cat("\n")
cat("Our testing data")
sms_test_labels %>%
  table %>%
  prop.table


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Creating a Word Cloud visualization<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
sms_corpus <- tm_map(sms_corpus, PlainTextDocument)
#Creating Spam WordCloud
par(mfcol = c(1, 2))
spam <- sms %>%
  subset(type == "spam")
spamCloud <- wordcloud(spam$text, max.words = 40, scale = c(3, 0.6))
#Creating ham wordcloud
par(mfcol = c(1, 2))
ham <- sms %>%
  subset(type == "ham")
hamCloud <- wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Creating indicator features for frequent words<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sms_dtm_freq_train <- sms_dtm_train %>%
  findFreqTerms(5) %>%
  sms_dtm_train[ , .]
sms_dtm_freq_test <- sms_dtm_test %>%
  findFreqTerms(5) %>%
  sms_dtm_test[ , .]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Training a model on the data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sms_train <- sms_dtm_freq_train %>%
  apply(MARGIN = 2, convert_counts)
sms_test <- sms_dtm_freq_test %>%
  apply(MARGIN = 2, convert_counts)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Evaluating model performance<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_pred <- predict(sms_classifier, sms_test)
CrossTable(sms_pred, sms_test_labels, prop.chisq = FALSE, chisq = FALSE, 
           prop.t = FALSE,
           dnn = c("Predicted", "Actual"))
