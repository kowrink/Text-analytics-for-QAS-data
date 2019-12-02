library(NLP)
library(tm)  # make sure to load this prior to openNLP
library(openNLP)
library(openNLPmodels.en)

init_s_w = annotate(otxt, list(Maxent_Sent_Token_Annotator(),
                                      Maxent_Word_Token_Annotator()))
pos_res = annotate(otxt, Maxent_POS_Tag_Annotator(), init_s_w)
word_subset = subset(pos_res, type=='word')
tags = sapply(word_subset$features , '[[', "POS")

txt_pos = data_frame(word=otxt[word_subset], pos=tags) %>% 
  filter(!str_detect(pos, pattern='[[:punct:]]'))

#Word embedding
library(keras)
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(otxtrw)

library(reticulate)
library(purrr)
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}

embedding_size <- 128  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1   
input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)
embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()
dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")
embed_model <- keras_model(list(input_target, input_context), output)
embed_model %>% compile(loss = "binary_crossentropy", optimizer = "adam")


embed_model %>%
  fit_generator(
    skipgrams_generator(otxtrw, tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 100000, epochs = 5
  )





library(text2vec)
result <- aggregate(word ~ id , data = DF, paste, collapse = " ")
tokens <- space_tokenizer(result)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
RcppParallel::setThreadOptions(numThreads = 4)
model_matrix=glove$fit_transform(tcm, n_iter=20)

wv_context = glove$components
word_vectors = model_matrix + t(wv_context)


atonamy = word_vectors["bp", , drop = FALSE] +
  word_vectors["vss", , drop = FALSE] + word_vectors["wnl", , drop = FALSE] +  word_vectors["stress", , drop = FALSE] + word_vectors["panic", , drop = FALSE]

atonamy_cos_sim = sim2(x = word_vectors, y = atonamy, method = "cosine", norm = "l2")
head(sort(atonamy_cos_sim[,1], decreasing = TRUE), 15)
