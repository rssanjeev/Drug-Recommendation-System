

#install.packages("mlbench")
#install.packages("caret")
#install.packages("tidyverse")
install.packages("tidytext")
library(caret)
library(mlbench)
library(tidyverse)
library(tidytext)


train = read.csv("drugsComTrain_raw.csv")
test = read.csv("drugsComTest_raw.csv")
train %>% head()
View(train)
summary(train)
str(train)


nrow(train)
#161297

train[duplicated(train)|duplicated(train, fromLast = T), ]
#No duplicate values present in the data.

sapply(train, function(x) sum(is.na(x)))
#Checking the missing values in dataset.

#Finding the total count of missing values.
sum(!complete.cases(train))

head(train)


train  <- train %>% mutate(document = row_number())
train$condition <- as.character(train$condition)
train$review <- as.character(train$review)
train$drugName <- as.character(train$drugName)
summary(train)
str(train)


trainText <- train %>% 
  unnest_tokens(word, review) %>% 
  anti_join(stop_words)


length(unique(trainText$drugName))


unique(trainText$drugName)
abc = trainText[which(trainText$drugName %in% c("Methadose","Didrex","Forteo","Kava")),]
abc

abc %>% 
  count(word, drugName, sort = TRUE) %>% 
  mutate(drugName = factor(drugName),
         word = as.factor(word),
         word = fct_reorder(word,n)
  ) %>% 
  group_by(drugName) %>% 
  top_n(10)  %>% 
  ungroup() %>% 
  ggplot(aes(word, n)) +
  geom_col(aes(fill = drugName)) +
  facet_wrap(~drugName, scales = "free") +
  coord_flip()

View(abc)
abc 
library(caret)
library(LiblineaR)
review_ted_model <- train(condition ~ word, data = abc, method = "svmLinear3")

plot(review_ted_model)

View(test)
predict_svm_rbf <- predict(review_ted_model, newdata = test)
confusionMatrix(predict_svm_rbf, test$condition)

review_ted_model2 <- train(condition ~ word, data = abc,
                           method = "svmLinear",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
                          # tuneGrid = expand.grid(C = seq(0, 1, 0.05))
                           )

review_ted_model2$accuracy

plot(review_ted_model2)


#
#Linear SVM Performance Evaluation: Hold-out Method
predict_svm_linear <- predict(review_ted_model2, newdata = diabetes_test)

review_ted_model3 <- train(condition ~ word, data = abc,
                       preProcess = c("center", "scale"),
                       tuneGrid = expand.grid(sigma = seq(0, 1, 0.1),
                                              C = seq(0, 1, 0.1)),
                       method = "svmRadial",
                       trControl = trainControl(method = "boot",
                                                number = 20))

#KNN


start3 <- Sys.time()
model_knn <- train(condition ~ word, data = abc, method = "knn")
Sys.time() - start3
model_knn



review_ted_model$accuracy
abc %>% 
  mutate(drugName = factor(drugName)) %>% 
  count(word, drugName, sort = TRUE) %>% 
  bind_tf_idf(drugName, word, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = as.factor(word),
         word = fct_reorder(word, n)) %>% 
  split(.$drugName) %>% 
  purrr::map(~head(10, x = .)) %>% 
  purrr::map(~tbl_df(data = .)) %>% 
  purrr::map(~ggplot(data = ., mapping = aes(word, n)) + 
               geom_col(aes(fill = word), alpha = 0.8, color = "black") +
               geom_line(group = 1) +
               geom_point() +
               coord_flip() +
               guides(alpha = FALSE) +
               labs(title = paste("Words in",.$drugName,"articles weighted by tf_idf"), x = "Word", y = "Count", subtitle = "Sorted by word count")
  )            

install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)
library(ggrepel)

set.seed(100)

topicBigrams <- train %>% 
  unnest_tokens(bigrams, review, token = "ngrams", n = 2) %>% 
  separate(bigrams, sep = " ", into = c("word1", "word2")) %>% 
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>% 
  filter(word1!=" " | word2 != " ")
#unite(bigram, word1, word2, sep = " ") %>% 

topicBigrams %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n>20) %>% 
  graph_from_data_frame() %>% 
  ggraph() +
  geom_edge_link(aes(edge_alpha = n)) +
  geom_node_point(alpha = 0.8, size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  guides(alpha = FALSE, edge_alpha = FALSE) +
  theme_void()

install.packages('widyr')
library(widyr)


set.seed(100)

headlineText %>% 
  group_by(word) %>% 
  filter(n()>50) %>% 
  pairwise_cor(word, selftext, sort = TRUE) %>% 
  filter(correlation>0.13) %>% 
  graph_from_data_frame() %>% 
  ggraph() +
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_node_point(size = 3, alpha = 0.8) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()




