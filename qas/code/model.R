library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
library(tictoc)
my_data2 = read_excel("data.xlsx",sheet="DARF data")
cleansed=my_data2[!is.na(my_data2$`Cause of Injury Code`)&!is.na(my_data2$Narrative), ]
cleansed=data.frame(cbind(cleansed$`Cause of Injury Code`,cleansed$Narrative))
cleansed$X2=as.character(cleansed$X2)
cleansed=cleansed[-c(70,8572),]
cleansed$id=1:16171

cleansed_tokens <- cleansed %>%
  unnest_tokens(output = word, input = X2) %>%
  # remove numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # remove stop words
  anti_join(stop_words) %>%

  # stem the words
  mutate(word = SnowballC::wordStem(word))
cleansed_dtm <- cleansed_tokens %>%
  # get count of each token in each document
  count(id, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = id, term = word, value = n)
#a=as.numeric(cleansed_dtm$dimnames$Docs)
#p=unlist(list(1:16173))
#p[!p %in% a]
cleansed_tokens %>%
  # get count of each token in each document
  count(id, word) %>%
  # create a document-term matrix with all features and tf-idf weighting
  cast_dtm(document = id, term = word, value = n,
           weighting = tm::weightTfIdf)
cleansed_dtm=removeSparseTerms(cleansed_dtm, sparse = .99)
cleansed_tfidf <- cleansed_tokens %>%
  count(X1, word) %>%
  bind_tf_idf(term = word, document = X1, n = n)

plot_cleansed <- cleansed_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
plotcleansed=plot_cleansed[!(plot_cleansed$word %in% c("vehicl","pt","patient","nil","car","pain")),]
# graph the top 10 tokens for 4 categories
plotcleansed %>%
  filter(X1 %in% c("MOTORVEH",
                      "STATOBJ",
                      "NONCOLL", "COLLOTHER","OTHRUNSPEC","PEDANIMAL")) %>%
  group_by(X1) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, X1)) %>%
  
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ X1, scales = "free") +
  coord_flip()

cleansed$major <- as.numeric( factor(cleansed$X1) ) -1
x=as.matrix(cleansed_dtm)
cleansed_rf <- caret::train(x= as.matrix(cleansed_dtm),
                            y = make.names(cleansed$major),
                     method = "ranger", 
                     num.trees = 5,
                     trControl = trainControl(method = "oob"))



cleansed_slice <- slice(cleansed, as.numeric(cleansed_dtm$dimnames$Docs))
tic()
cleansed_rf_10 <- train(x = as.matrix(cleansed_dtm),
                        y = factor(cleansed_slice$major),
                        method = "ranger",
                        num.trees = 200,
                        importance = "impurity",
                        trControl = trainControl(method = "oob"))
toc()

cleansed_rf_10$finalModel %>%
  # extract variable importance metrics
  ranger::importance() %>%
  # convert to a data frame
  enframe(name = "variable", value = "varimp") %>%
  top_n(n = 20, wt = varimp) %>%
  # plot the metrics
  ggplot(aes(x = fct_reorder(variable, varimp), y = varimp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Token",
       y = "Variable importance (higher is more important)")
plot(margin(cleansed_rf_10,cleansed$major))
saveRDS(cleansed_rf_10, "./final_model2.rds")
