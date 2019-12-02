library(Rtsne)
library(ggplot2)
library(plotly)

tsne <- Rtsne(word_vectors[14385:14885,], perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(word_vectors)[14385:14885]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)
tsne_plot
