
install.packages("widyr")
install.packages("igraph")
install.packages("ggraph")
install.packages("dplyr")
install.packages('Rcpp')
install.packages("topicmodels")
library(compare)
library(igraph)
library(ggraph)
library(xlsx)
library(readxl) 
library(tidyr)
library(janeaustenr)
library(psych)
library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)
library(widyr)
require(plyr)
library(tidytext)
library(topicmodels)
original_books <- otxt %>%
  group_by(id) %>%
  mutate(id2 = row_number()) %>%
  ungroup()

my_data <- read_excel("data.xlsx",sheet="EARF data")
my_data2 = read_excel("data.xlsx",sheet="DARF data")
otxt=data.frame(id=my_data$`EARF Number`,txt=paste(my_data$`Case Nature Code`,my_data$`Final Assessment Code`,my_data$`Case Comments`),stringsAsFactors = FALSE)

original_books <- otxt %>%
  group_by(id) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

tidy_books=original_books %>%
  unnest_tokens(word, txt)



tidy_books <- tidy_books %>%
  anti_join(stop_words)
frequency= tidy_books %>%
  count(word, sort = TRUE) 
wordcloud(words = frequency$word, freq = frequency$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

write.csv(frequency, file = "frequency.csv")
tidy_books= tidy_books[,-2]
ggplot(my_data$`Case Nature`, aes(x=type)) +
  geom_bar()

word_pairs <- interesting_desc %>%
  pairwise_count(word, id, sort = TRUE)

word_pairs %>%
  filter(item1 == "trauma")
word_pairs %>%
  filter(item1 == "alcohol")

word_cors <- interesting_desc %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, id, sort = TRUE)
word_cor3<- tidy_books %>%
  group_by(word) %>%
  filter(n() >= 300) %>%
  pairwise_cor(word, id, sort = TRUE)
write.csv(word_cor2, file = "cor.csv")
df$x <- paste(df$n,df$s)
cor_cause= word_cor3 %>%
  filter(item1 %in% c("achlrel", "bitesting", "drgmedrlt", "envirexp","medical","mtlhlthbvr","trauma","nilpr","otherspe",
                      "poisoning","socialsit","unkncause","bruising","abrasgraz","pain","alcintox") & correlation> 0.1) %>%
  group_by(item1)




count(my_data2$`Cause of Injury Code`)
otxt2=data.frame(id=my_data2$`EARF Number`,txt=paste(my_data2$`Cause of Injury Code`,my_data2$Narrative),stringsAsFactors = FALSE)
original_books2 <- otxt2 %>%
  group_by(id) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()
tidy_books2=original_books2 %>%
  unnest_tokens(word, txt)

tidy_books2 <- tidy_books2 %>%
  anti_join(stop_words)
word_cor4<- tidy_books2 %>%
  group_by(word) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, id, sort = TRUE)
cor_situ= word_cor4 %>%
  filter(item1 %in% c("statobj", "samelevel", "qbnoncoll", "pedcycle","pedanimal","othrunspec","noncoll","motorveh","collother") & correlation> 0.1) %>%
  group_by(item1)


postcode_fre=plyr::count(my_data$Postcode)
postcode_fre %>% arrange(desc(freq))
street_fre=plyr::count(my_data$`Street Name`)
street_fre %>% arrange(desc(freq))
write.csv(postcode_fre, file = "postcode_fre.csv")


otxt3=data.frame(id=my_data$`EARF Number`,txt=paste(my_data$Postcode,my_data$`Case Comments`,my_data$`Street Name`),stringsAsFactors = FALSE)
original_books3 <- otxt3 %>%
  group_by(id) %>%
  ungroup()
tidy_books3=original_books3 %>%
  unnest_tokens(word, txt)

tidy_books3 <- tidy_books3 %>%
  anti_join(stop_words)
word_cor4<- tidy_books3 %>%
  group_by(word) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, id, sort = TRUE)
cor_situ= word_cor4 %>%
  filter (as.numeric(item1)>1000 & correlation> 0.1) %>%
  group_by(item1)
postcode=read.csv("australian_postcodes.csv",stringsAsFactors = FALSE)
postcode=postcode[,1:3]
postcode1=filter(postcode, X!="NSW" & X!="VIC" & X!="WA"  & X!="SA" & X!="TAS"  & X!="NT"
                 & X!="ACT")
postcode=postcode1[,1:2]
cor_situ1=cor_situ[,1:2]
names(cor_situ1) = c("postcode","locality")
cor_situ1$postcode=as.numeric(cor_situ1$postcode)

postcode1=postcode1 %>%
  unnest_tokens(word,locality)
postcode1=postcode1[,-2]
names(postcode1) = c("postcode","locality")

names(cor_situ) = c("postcode","locality","correlation")

tn=merge(cor_situ1,postcode1,by = c("postcode","locality"))

cor_situ$item1=as.numeric(cor_situ$item1)
names(tn)=c("item1","locality")
names(cor_situ)=c("item1","locality","correlation")
cor_situ=anti_join(cor_situ,tn,by="item1")
cor_situ$len=nchar()
cor_situ1=filter(cor_situ,item1>1000 & nchar(locality)>4)
hosp=filter(cor_situ,item1>1000 & nchar(locality)<=4)



write.csv(cor_situ1, file = "location_highfrewords.csv")    
write.csv(hosp, file = "hosp.csv")
  
qas_forgrams <- original_books%>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 4)%>%
  separate(ngram, c("word1", "word2", "word3","word4"), sep = " ")

head=filter(qas_forgrams,word1 =='head'|word2 == 'head'|word3 == 'head'|word4 == 'head') %>%
  count(word1, word2, word3,word4,sort = TRUE)
AFINN <- get_sentiments("afinn")
belt=filter(qas_forgrams,word1 =='belt'|word2 == 'belt'|word3 == 'belt'|word4 == 'belt') %>%
  count(word1, word2, word3,word4,sort = TRUE)
head_senti = head %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE)
belt_senti = belt %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%
  count(word2, score, sort = TRUE)
tidy_books %>% 
  count(word, sort = TRUE)
cor=read.csv("cor.csv")
cor=cor[,-1]
# Geo_info=data.frame(my_data$`EARF Number`,my_data$`Street Number`,my_data$Postcode)
# colnames(Geo_info)[colnames(Geo_info)=="my_data..EARF.Number."] <- "id"
# tidy_books3=left_join(tidy_books,Geo_info[,c("id","my_data.Postcode")])
# tidy_books3=tidy_books3[complete.cases(tidy_books3), ]
# colnames(tidy_books3)=c("id","line","word","postcode")
# word_cor5<- tidy_books3 %>%
#   group_by(postcode) %>%
#   filter(n() >= 50) %>%
#   cor(tidy_books3)
# cor_situ= word_cor5 %>%
#   filter(as.numeric(item1)>1000 & correlation> 0.1) %>%
#   group_by(item1)
# 
# bigram_counts <- bigrams_filtered %>% 
#   count(word1, word2, sort = TRUE)
# 
# tidy_books=tidy_books[,-2]
# word_pairs <- tidy_books %>%
#   pairwise_count(word, id, sort = TRUE)
# desc_word_pairs <- nasa_desc %>% 
#   pairwise_count(word, id, sort = TRUE, upper = FALSE)
# 
# library(ggplot2)
# library(igraph)
# library(ggraph)
# set.seed(1234)
#   cor %>%
#   filter(correlation >= 0.20) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "cyan4") +
#   geom_node_point(size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE,
#                  point.padding = unit(0.2, "lines")) +
#   theme_void()

words_desc <- tibble(id = my_data$`EARF Number`, 
                    desc = paste(my_data$`Case Nature Code`,my_data$`Final Assessment Code`,my_data$`Case Comments`,my_data$Postcode,my_data$`Street Name`),stringsAsFactors = FALSE)
words_desc <- words_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)
words_desc %>% 
  dplyr::count(word, sort = TRUE)
word_counts <- words_desc %>%
  dplyr::count(id, word, sort = TRUE) %>%
  ungroup()
desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)
desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))  
tidy_lda <- tidy(desc_lda)    
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
lda_gamma <- tidy(desc_lda, matrix = "gamma")
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))
ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))


