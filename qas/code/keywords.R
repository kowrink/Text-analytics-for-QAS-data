options(java.parameters = "- Xmx1024m")
require("NLP")
## Some text.
s=my_data2$Narrative
library(openNLP)
library(openNLPmodels.en)
library(udpipe)
library(ngram)
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.4-190531.udpipe')
# s <- as.String(s)
# ## Need sentence and word token annotations.
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
# parse_annotator <- Parse_Annotator()
# ## Compute the parse annotations only.
# p <- parse_annotator(s, a2)
# ## Extract the formatted parse trees.
# ptexts <- sapply(p$features, `[[`, "parse")
# ptexts
# ## Read into NLP Tree objects.
# ptrees <- lapply(ptexts, Tree_parse)
# ptree



####

qas_annotate <- udpipe_annotate(udmodel_english, my_data2$Narrative)
x <- data.frame(qas_annotate)
library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")


stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

####RAKE, identify the keyword
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = stats[8:28,], col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")



x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, pattern="((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))"
                            ,is_regex=TRUE,term = tolower(x$token), detailed = TRUE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")


x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats2 <- keywords_phrases(x = x$phrase_tag, pattern="((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)"
                          ,is_regex=TRUE,term = tolower(x$token), detailed = TRUE)
stats2 <- subset(stats2, ngram > 1 & freq > 3)
stats2$key <- factor(stats2$keyword, levels = rev(stats2$keyword))
barchart(key ~ freq, data = head(stats2, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")




l1=c()
a=wordcount(my_data2$Narrative, sep = " ", count.function = sum)
for (id in 1:29763) {
  no=wordcount(my_data2$Narrative[id], sep = " ", count.function = sum)
  l1=c(l1,no)
}
l <- cumsum(l)
id=c(1:29763)
idrange=data.frame(id,l)
from=l-l1+1
idrange=cbind(idrange,from)
library(data.table)

# convert your datasets in data.table
setDT(idrange) 
setDT(stats)

output <- stats[idrange, on = .(start >= from , end <= l), nomatch = NA, # indicate y range
             .(keyword,id,ngram )]  # indicate columns in the o
my_data2$id=c(1:29763)
label=my_data2[,5]
label=cbind(label,my_data2$id)
output= merge(output, label, by.y="my_data2$id",by.x="id")

my_data2 %>%
  group_by(`Cause of Injury Code`) %>%
  summarise(n()) %>%
  top_n(10)

output <- na.omit(output) 
output=output %>%
  filter(`Cause of Injury Code` %in% c("MOTORVEH",
                   "STATOBJ",
                   "NONCOLL", "COLLOTHER","OTHRUNSPEC","PEDANIMAL","PEDCYCLE","QBNONCOLL","SAMELEVEL"))
output_tokens <- output %>%
  unnest_tokens(output = word, input = keyword) %>%
  # remove numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # remove stop words
  anti_join(stop_words) %>%
  
  # stem the words
  mutate(word = SnowballC::wordStem(word))
output_dtm <- output_tokens %>%
  # get count of each token in each document
  count(id, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = id, term = word, value = n)
#a=as.numeric(cleansed_dtm$dimnames$Docs)
#p=unlist(list(1:16173))
#p[!p %in% a]
output_tokens %>%
  # get count of each token in each document
  count(id, word) %>%
  # create a document-term matrix with all features and tf-idf weighting
  cast_dtm(document = id, term = word, value = n,
           weighting = tm::weightTfIdf)
output_dtm=removeSparseTerms(output_dtm, sparse = .99)
output_tfidf <- output_tokens %>%
  count(id, word) %>%
  bind_tf_idf(term = word, document = id, n = n)

output_cleansed <- output_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

output_tfidf2 <- output_tokens %>%
  count(`Cause of Injury Code`, word) %>%
  bind_tf_idf(term = word, document = `Cause of Injury Code`, n = n)

output_cleansed2 <- output_tfidf2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
#plotcleansed=output_cleansed[!(plot_cleansed$word %in% c("vehicl","pt","patient","nil","car","pain")),]
# graph the top 10 tokens for 4 categories
output_cleansed2 %>%
  filter(`Cause of Injury Code` %in% c("MOTORVEH",
                   "STATOBJ",
                   "NONCOLL", "COLLOTHER","OTHRUNSPEC","PEDANIMAL")) %>%
  group_by(`Cause of Injury Code`) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, `Cause of Injury Code`)) %>%
  
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~ `Cause of Injury Code`, scales = "free") +
  coord_flip()

output$major <- as.numeric( factor(output$`Cause of Injury Code`) ) -1

output_slice <- slice(output, as.numeric(output_dtm$dimnames$Docs))
tic()
output_rf_10 <- train(x = as.matrix(output_dtm),
                        y = factor(output_slice$major),
                        method = "ranger",
                        num.trees = 200,
                        importance = "impurity",
                        trControl = trainControl(method = "oob"))
toc()

output_rf_10$finalModel %>%
  # extract variable importance metrics
  ranger::importance() %>%
  # convert to a data frame
  enframe(name = "variable", value = "varimp") %>%
  top_n(n = 25, wt = varimp) %>%
  # plot the metrics
  ggplot(aes(x = fct_reorder(variable, varimp), y = varimp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Token",
       y = "Variable importance (higher is more important)")
plot(margin(cleansed_rf_10,cleansed$major))
plot(margin(output_rf_10,cleansed$major))
saveRDS(cleansed_rf_10, "./final_model2.rds")