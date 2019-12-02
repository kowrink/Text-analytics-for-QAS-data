library(dplyr)
library(tidytext)
library(tictoc)
library(caret)
library(tidyverse)
library(tidytext)
a=c(1:16171)
cleansed=cbind(a,cleansed)
rawtext <- cleansed %>%
  group_by(id) %>%
  ungroup()

tidytext=rawtext %>%
  unnest_tokens(word, X2)

tidytext <- tidytext %>%
  anti_join(stop_words)
load("vec.Rdata")


sampledata <- cbind(word_vectors, sampleids=rownames(word_vectors))
vectorframe=data.frame(sampledata)
mergematrix=merge(tidytext,vectorframe, by.x="word", by.y="sampleids")

sampledata=aggregate(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,
                           V11,V12,V13,V14,V15,V16,V17,V18,
                           V19,V20,V21,V22,V23,V24,V25,V26,V27,
                           V28,V29,V30,V31,V32,V33,V34,V35,V36,
                           V37,V38,V39,V40,V41,V42,V43,V44,V45,
                           V46,V47,V48,V49,V50
                           ) ~ a, data=mergematrix, FUN=sum)
traindata=merge(sampledata,rawtext, by.x="a", by.y="a")
traindata2=traindata %>%
  filter(X1 %in% c("MOTORVEH",
                   "STATOBJ",
                   "NONCOLL", "COLLOTHER","OTHRUNSPEC","PEDANIMAL"))
traindata2=traindata2[ ,-c(52,53,54) ]
traindata=traindata2[ ,-c(1,52) ]
tic()
rf_vector <- train(x = as.matrix(traindata),
                        y = factor(traindata2$major),
                        method = "ranger",
                        num.trees = 200,
                        importance = "impurity",
                        trControl = trainControl(method = "oob"))

toc()


svm_vector = svm(factor(traindata2$major) ~ ., data = traindata, scale = FALSE, kernel = "radial", cost = 5)



######################################
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(svm_vector, traindata)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)
func = predict(vector, xgrid, decision.values = TRUE)
func = attributes(func)$decision

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(vector, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)



rf_vector$finalModel %>%
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