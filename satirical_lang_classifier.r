library(RedditExtractoR)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

List generation and binary assignment

drop <- c("text", "date_utc", "comments", "url")

onion_list <- find_thread_urls(keywords = NA, sort_by = "top", subreddit = "theonion", period = "all")
onion_list <- onion_list[,!(names(onion_list)%in%drop)]
rownames(onion_list) <- 1:nrow(onion_list)

not_onion_list <- find_thread_urls(keywords = NA, sort_by = "top", subreddit = "nottheonion", period = "all")
not_onion_list <- not_onion_list[,!(names(not_onion_list)%in%drop)]
rownames(not_onion_list) <- 1:nrow(not_onion_list)

onion_list$subreddit[onion_list$subreddit == "TheOnion"] <- 1
onion_list$subreddit <- as.integer(onion_list$subreddit)

not_onion_list$subreddit[not_onion_list$subreddit == "nottheonion"] <- 0
not_onion_list$subreddit <- as.integer(not_onion_list$subreddit)

# 49.3%/ 50.7%

final_list <- rbind.data.frame(onion_list, not_onion_list)
final_list <- final_list[sample(nrow(final_list)),]

write.csv(final_list, file = "onion_test_Rstudio.csv")
final_list <- read.csv("onion_test_Rstudio.csv")

# List cleaning

final_list$title <- tolower(final_list$title)
final_list <- final_list[,!(names(final_list)%in%"X")] # drops the old index
final_list$title <- str_replace_all(final_list$title, "[]", "") # un-rendered symbols
final_list$title <- str_replace_all(final_list$title, "[']", "")
final_list$title <- str_replace_all(final_list$title, "[-:;“”]", "") # common punctuation
final_list$title <- str_replace_all(final_list$title, "[&]", "and")
head(final_list)

bulk_text <- final_list[,1]
TextDoc <- Corpus(VectorSource(bulk_text))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
TextDoc <- tm_map(TextDoc, removePunctuation)
TextDoc <- tm_map(TextDoc, stripWhitespace)
TextDoc <- tm_map(TextDoc, stemDocument) # text stemming, reduces words to their 
#root form

TextDoc_dtm <- TermDocumentMatrix(TextDoc) # building a term document matrix
dtm_matrix <- as.matrix(TextDoc_dtm) # storing as matrix
dtm_values <- sort(rowSums(dtm_matrix),decreasing=TRUE) # sort by value of frequency
dtm_df <- data.frame(word = names(dtm_values),freq=dtm_values) # store as dataframe
most_freq <- as.character(head(dtm_df, 10))

# Visualization of frequency

coulor_1 <- brewer.pal(10, "RdBu")
barplot(dtm_df[1:10,]$freq, las = 2, names.arg = dtm_df[1:10,]$word,
        col = coulor_1, main = "Top Words by Frequency, n = 10",
        ylab = "Word Frequencies"

set.seed(2323)
par(mar = c(1,1,1,1)) # adjusts margins for plot area
wordcloud(words = dtm_df$word, freq = dtm_df$freq, min.freq = 5,
          max.words = 100, random.order = FALSE, rot.per = 0.4,
          colors = brewer.pal(8, "RdBu"), scale = c(4,0.5)
)

findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), 
           corlimit = 0.25) # attempting to find associations

# get_method function and associated methods

# syuzhet → overall positivity or negativity 
syuzhet_vector <- get_sentiment(text, method = "syuzhet")
head(syuzhet_vector, 10)
summary(syuzhet_vector)
sentiment_list <- cbind(final_list, syuzhet_vector)

# bing → binary scale with -1 indicating negative and +1 indicating positive sentiment
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector, 10)
summary(bing_vector)
sentiment_list <- cbind(sentiment_list, bing_vector)

# afinn → integer scale ranging from -5 to +5
afinn_vector <- get_sentiment(text, method = "afinn")
head(afinn_vector, 10)
summary(afinn_vector)
sentiment_list <- cbind(sentiment_list, afinn_vector)

normalized_vectors <- rbind( # normalize the scores across vectors
  sign(syuzhet_vector),
  sign(bing_vector),
  sign(afinn_vector)
)

normalized_vectors <- as.data.frame(t(normalized_vectors))
names(normalized_vectors)[names(normalized_vectors) == "V1"] <- "syuzhet_normal"
names(normalized_vectors)[names(normalized_vectors) == "V2"] <- "bing_normal"
names(normalized_vectors)[names(normalized_vectors) == "V3"] <- "afinn_normal"

sentiment_list <- cbind(sentiment_list, normalized_vectors)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row

emotion <- get_nrc_sentiment(text)
head(emotion, 10) # words associated with particular emotion
sentiment_list <- cbind(sentiment_list, emotion)

# Bar Plot showing the count of words in the text, associated with each emotion

emot_plot <- data.frame(t(emotion))
#The function rowSums computes column sums across rows for each level of a grouping variable.
emot_plot <- data.frame(rowSums(emot_plot[2:nrow(final_list)]))
names(emot_plot)[1] <- "count"
emot_plot <- cbind("sentiment" = rownames(emot_plot), emot_plot)
rownames(emot_plot) <- NULL
emot_plot_2 <- emot_plot[1:8,]
quickplot(sentiment, data=emot_plot_2, weight=count, geom= "bar", 
          fill = sentiment, ylab = "count")+ggtitle("Survey sentiments")

# Plot two - count of words associated with each sentiment, expressed as a percentage

par(mar = c(5,5,5,2))
barplot(
  sort(colSums(prop.table(emotion[,1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab = "%"
)

sentiment_list_drop <- sentiment_list[,-(1),drop=FALSE]
sentiment_list_NA <- na.omit(sentiment_list_drop)

set.seed(2323)
indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
train_df <- sentiment_list_NA[indicies,]
test_df <- sentiment_list_NA[-indicies,]

log_reg_2 <- glm(subreddit~., data = train_df, family = binomial)
summary(log_reg_2)

preds <- predict(log_reg_2, newdata = test_df, type = "response")
preds[1:5]
preds_encoded <- ifelse(preds < 0.5, 0, 1)
preds_encoded[1:5]
sum(preds_encoded == test_df$subreddit)/nrow(test_df) # 0.5972
table(preds_encoded, test_df$subreddit)

library(MASS)

set.seed(2323)
indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
train_df <- sentiment_list_NA[indicies,]
test_df <- sentiment_list_NA[-indicies,]

ldafit_1 <- lda(subreddit~., data = train_df)
summary(ldafit_1)
ldafit_1

lda_preds <- predict(ldafit_1, newdata = test_df)
lda_preds$posterior[1:10,]
lda_preds$class[1:100]
sum(lda_preds$class == test_df$subreddit)/nrow(test_df) # 0.5992 

table(lda_preds$class, test_df$subreddit)

ldafit_1 <- lda(subreddit~., data = train_df)

set.seed(2323)
qdafit_1 <- qda(subreddit~., data = train_df)
qda_preds <- predict(qdafit_1, newdata = test_df)
sum(qda_preds$class == test_df$subreddit)/nrow(test_df) # 0.5971
table(qda_preds$class, test_df$subreddit)

k <- nrow(sentiment_list_NA) # 'nrow' for true LOOCV
acc_collection <- rep(0,k)
set.seed(2323)

sentiment_list_NA <- na.omit(sentiment_list_drop)

for(i in 1:k){
  print(i)
  indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
  train_df <- sentiment_list_NA[indicies,]
  test_df <- sentiment_list_NA[-indicies,]
  log_reg_1 <- glm(subreddit~., data = train_df, family = binomial)
  preds <- predict(log_reg_1, newdata = test_df,
                      type = "response")
  preds_encoded <- ifelse(preds < 0.5, 0, 1)
  acc_collection[i] <- sum(preds_encoded == test_df$subreddit)/nrow(test_df)
}

mean(acc_collection) # 10:0.5771; 100:0.5671; 1000:0.566;, nrow:0.5656  

density(preds_encoded)
plot(density(preds_encoded), main = "Densite Curves for GLM LOOCV, k = 1974[nrow]", 
     type = "h", col = "darkcyan")

k <- nrow(sentiment_list_NA) # 'nrow' for true LOOCV
acc_collection_lda <- rep(0,k)
set.seed(2323)

for(i in 1:k){
  print(i)
  indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
  train_df <- sentiment_list_NA[indicies,]
  test_df <- sentiment_list_NA[-indicies,]
  ldafit_2 <- lda(subreddit~., data = train_df, family = binomial)
  preds_lda <- predict(ldafit_2, newdata = test_df)
  lda_preds_density <- as.numeric(lda_preds$class) -1
  acc_collection_lda[i] <- sum(lda_preds$class == test_df$subreddit)/nrow(test_df)
}

mean(acc_collection_lda) # 0.5009

plot(density(lda_preds_density),main = "Density Curve for LDA LOOCV [k = nrow(1974)]",
     type = "h", col = "coral3")

k <- nrow(sentiment_list_NA) # 'nrow' for true LOOCV
acc_collection_qda <- rep(0,k)
set.seed(2323)

for(i in 1:k){
  print(i)
  indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
  train_df <- sentiment_list_NA[indicies,]
  test_df <- sentiment_list_NA[-indicies,]
  qdafit_2 <- qda(subreddit~., data = train_df, family = binomial)
  preds_qda <- predict(qdafit_2, newdata = test_df)
  qda_preds_density <- as.numeric(qda_preds$class) -1
  acc_collection_qda[i] <- sum(qda_preds$class == test_df$subreddit)/nrow(test_df)
}

mean(acc_collection_qda) # 0.5014

library(rpart)

set.seed(2323)

indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
train_df <- sentiment_list_NA[indicies,]
test_df <- sentiment_list_NA[-indicies,]

tree1 <- rpart(subreddit~., data = train_df)
plot(tree1, uniform = TRUE, branch = 0.5, compress = FALSE, margin = 0.1)
text(tree1, cex = .9)

preds_tree <- predict(tree1, newdata = test_df)
mse <- sqrt(mean((test_df$subreddit - preds_tree)^2))
mse # 0.4945

sentiment_list_NA <- na.omit(sentiment_list_drop)

set.seed(2323)

cv_values <- rep(0, nrow(sentiment_list_NA))

for(i in 1:length(cv_values)){
  print(i)
  indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
  train_df <- sentiment_list_NA[indicies,]
  test_df <- sentiment_list_NA[-indicies,]
  tree1 <- rpart(subreddit~., data = train_df)
  preds_tree <- predict(tree1, newdata = test_df)
  mse <- sqrt(mean((test_df$subreddit - preds_tree)^2))
  cv_values[i] <- mse
}

cv_values

mean(cv_values) # 0.4973

plot(tree1, uniform = TRUE, branch = 0.5, compress = FALSE, margin=0.1)
text(tree1, cex = .9)

# did not prune tree as there's nothing to prune...

library(randomForest)

sqrt(ncol(train_df))

set.seed(2323)

indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
train_df <- sentiment_list_NA[indicies,]
test_df <- sentiment_list_NA[-indicies,]
random_f <- randomForest(subreddit~., data = train_df, mtry = 2, importance = TRUE)
preds <- predict(random_f, newdata = test_df)
mse <- sqrt(mean((preds-test_df$subreddit)^2))
mse # 0.5078

cv_values_2 <- rep(0, 100)

for(i in 1:length(cv_values_2)){
  print(i)
  indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
  train_df <- sentiment_list_NA[indicies,]
  test_df <- sentiment_list_NA[-indicies,]
  random_f <- randomForest(subreddit~., data = train_df, mtry = 2, importance = TRUE)
  preds <- predict(random_f, newdata = test_df)
  mse <- sqrt(mean((preds-test_df$subreddit)^2))
  cv_values_2[i] <- mse
}

mean(cv_values_2) 

library(gbm)

set.seed(2323)

indicies <- sample(1:nrow(sentiment_list_NA), 0.75*nrow(sentiment_list_NA))
train_df <- sentiment_list_NA[indicies,]
test_df <- sentiment_list_NA[-indicies,]

boost1 <- gbm(subreddit~., data = train_df, distribution = "gaussian", n.trees = 5000, 
              interaction.depth = 4, verbose = TRUE)
preds <- predict(boost1, newdata = test_df, n.trees = 500)
mse <- sqrt(mean((preds - test_df$medv)^2))
mse

# https://github.com/lukefeilberg/onion/blob/master/Onion.ipynb