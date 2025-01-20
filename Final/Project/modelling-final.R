install.packages("tm")           
install.packages("slam")        
install.packages("topicmodels") 
install.packages("LDAvis")       
install.packages("openxlsx")     
install.packages("ldatuning")    
install.packages("text2vec")     
install.packages("Matrix")       
install.packages("textmineR")    
install.packages("ggplot2")      
install.packages("wordcloud")    
install.packages("proxy")

library(tm)
library(slam)
library(topicmodels)
library(LDAvis)
library(openxlsx)
library(ldatuning)
library(text2vec)
library(Matrix)
library(textmineR)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(proxy)

data <- read.xlsx("data_processed.xlsx")

data$Article <- sapply(data$Article, function(x) {
  cleaned_text <- paste(unlist(strsplit(x, ",")), collapse = " ")  
  gsub("\\s+", " ", cleaned_text)  
})

head(data$Article)

corpus <- Corpus(VectorSource(data$Article))

dtm <- DocumentTermMatrix(corpus)
dtm

term_freq <- col_sums(dtm)
doc_freq <- col_sums(dtm > 0)

dtm <- removeSparseTerms(dtm, 0.90)
dtm_tfidf <- weightTfIdf(dtm)

dtm_tfidf <- weightTfIdf(dtm)

dtm_tfidf

inspect(dtm[1:5, 1:5])    
inspect(dtm_tfidf[1:5, 1:5])

control_params <- list(
  iter = 5000,  
  burnin = 3000,  
  thin = 3,
  verbose = 2
)

num <- 10
lda_model <- LDA(dtm, k = num, method = "Gibbs", control = control_params)
lda_model

calculate_coherence <- function(dtm, k) {
  top_terms <- terms(lda_model, k)
  term_matrix <- as.matrix(dtm)
  coherence_scores <- numeric(k)
  
  for (topic_idx in 1:k) {
    topic_words <- top_terms[topic_idx, ]

    word_indices <- which(colnames(term_matrix) %in% topic_words)

    if (length(word_indices) > 1) {
      pairwise_similarities <- proxy::dist(term_matrix[, word_indices], method = "cosine")
      coherence_scores[topic_idx] <- mean(pairwise_similarities) 
    }
  }
  
  return(mean(coherence_scores))
}

coherence_score <- calculate_coherence(dtm, 10)
coherence_score

top_terms <- terms(lda_model, 10)
top_terms

term_probabilities <- topicmodels::posterior(lda_model)$terms
(term_probabilities[,1])

num_top_terms <- 10

top_terms_with_probabilities <- apply(term_probabilities, 1, function(topic_probs) {
  top_indices <- order(topic_probs, decreasing = TRUE)[1:num_top_terms]
  
  data.frame(
    Term = colnames(term_probabilities)[top_indices],
    Probability = topic_probs[top_indices] * 100
  )
})

for (i in 1:num) {
  result <- paste("Top Term probabilities for Topic-", as.character(i))
  print(result)
  df <- top_terms_with_probabilities[[i]]
  print(df)
  
  bar_plot <- ggplot(df, aes(x = reorder(Term, -Probability), y = Probability)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Top Terms for Topic", i), x = "Terms", y = "Probability") +
    theme_minimal() +
    coord_flip()
  
  print(bar_plot)
  Sys.sleep(1)
  
  wordcloud(words = df$Term, 
            freq = df$Probability, 
            scale = c(3, 0.5), 
            colors = brewer.pal(8, "Dark2"))
  
  title(main = paste("Word Cloud for Topic", i))
  cat("\n")
  Sys.sleep(1)
}

topics_per_document <- topicmodels::posterior(lda_model)$topics
topics_per_document[1, ]
dominant_topics <- apply(topics_per_document, 1, which.max)

data$Dominant_Topic <- dominant_topics
head(data)

data$Most_Probable_Words <- sapply(data$Dominant_Topic, function(topic_num) {
  if (topic_num >= 1 && topic_num <= num) {
    most_probable_words <- top_terms[topic_num, ]
    return(paste(most_probable_words, collapse = ", "))
  } else {
    return("Invalid Topic Index")
  }
})


lda_vis <- LDAvis::createJSON(
  phi = topicmodels::posterior(lda_model)$terms,
  theta = topicmodels::posterior(lda_model)$topics,
  doc.length = slam::row_sums(dtm_tfidf),
  vocab = colnames(dtm_tfidf),
  term.frequency = slam::col_sums(dtm_tfidf)
)

LDAvis::serVis(lda_vis)


dtm_sparse <- as.simple_triplet_matrix(dtm)

perplexities <- c()

split_ratio <- 0.8
num_docs <- nrow(dtm_sparse)
train_indices <- sample(1:num_docs, size = floor(split_ratio * num_docs))

train_dtm <- dtm_sparse[train_indices, ]
test_dtm <- dtm_sparse[-train_indices, ]

train_perplexity <- topicmodels::perplexity(lda_model, newdata = train_dtm)
test_perplexity <- topicmodels::perplexity(lda_model, newdata = test_dtm)

perplexities <- c(perplexities, train_perplexity, test_perplexity)
mean_perplexity <- mean(perplexities)
cat("Average Perplexity: ", mean_perplexity, "\n")


dtm_matrix <- as.matrix(dtm)
dtm_sparse <- as(dtm_matrix, "CsparseMatrix")

calculate_pmi_coherence <- function(top_terms, dtm_matrix, smooth = 1) {
  term_freqs <- slam::col_sums(dtm_matrix)
  coherence_score <- 0
  
  for (i in 1:(length(top_terms) - 1)) {
    for (j in (i + 1):length(top_terms)) {
      term_i <- top_terms[i]
      term_j <- top_terms[j]
      co_occur <- dtm_matrix[, term_i] %*% dtm_matrix[, term_j]
      pmi <- log((co_occur + smooth) / (term_freqs[term_i] * term_freqs[term_j] + smooth))
      coherence_score <- coherence_score + pmi
    }
  }
  
  return(-coherence_score)
}

coherence_scores <- sapply(1:num, function(topic_idx) {
  topic_terms <- top_terms[, topic_idx]
  coherence_score <- calculate_pmi_coherence(topic_terms, dtm_matrix)
  return(coherence_score)
})
cat("Coherence Scores: ", coherence_scores, "\n")


topic_diversity <- mean(sapply(1:num, function(topic_idx) {
  topic_terms_weights <- lda_model@beta[, topic_idx]
  sd(topic_terms_weights)
  mean(topic_terms_weights)

  diversity_score <- sd(topic_terms_weights) / (-mean(topic_terms_weights))
  
  return(diversity_score)
}))
cat("Topic Diversity: ", topic_diversity, "\n")


topic_purity <- sapply(1:num, function(topic_idx) {
  topic_docs <- which(dominant_topics == topic_idx)
  topic_distribution <- topics_per_document[topic_docs, topic_idx]
  purity_score <- mean(topic_distribution)
  return(purity_score)
})
cat("Topic Purity: ", topic_purity, "\n")


topic_spread <- sapply(1:num, function(topic_idx) {
  topic_docs <- sum(dominant_topics == topic_idx)
  spread_score <- topic_docs / nrow(data)
  return(spread_score)
})
cat("Topic Spread: ", topic_spread, "\n")


topic_stability <- 0
for (run in 1:3) {
  lda_model_run <- LDA(dtm, k = num, method = "Gibbs", control = control_params)
  top_terms_run <- terms(lda_model_run, 10)
  stability_score <- mean(top_terms == top_terms_run)  
  topic_stability <- topic_stability + stability_score
}
topic_stability <- topic_stability / 3
cat("Topic Stability: ", topic_stability, "\n")

