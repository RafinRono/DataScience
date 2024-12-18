install.packages("rvest")
install.packages("polite")
install.packages("dplyr")
install.packages("tokenizers")
install.packages("tm")
install.packages("textstem")
install.packages("stringr")
install.packages("hunspell")
install.packages("openxlsx")

library(dplyr)
library(rvest)
library(polite)
library(tokenizers)
library(tm)
library(textstem)
library(stringr)
library(hunspell)
library(openxlsx)

url <- "https://www.thedailystar.net/todays-news"

bow <- bow(url)

print(bow)
rules<-bow$robotstxt
print(rules)

page <- read_html(url)

headlines <- page %>%
  html_nodes(".title a") %>%  
  html_text()

snippets <- page %>%
  html_nodes("p.intro") %>%  
  html_text()

data <- data.frame(Headline = headlines, Snippet = snippets)
write.xlsx(data, "data_original.xlsx")

print(headlines)
print(snippets)
print(data)

data_no_emojis <- data %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    emoji_pattern <- "[^\x01-\x7F]"
    text_without_emojis <- str_remove_all(x, emoji_pattern)
    
    return(text_without_emojis) 
  })))

print(data_no_emojis)

expand_contractions_manual <- function(text) {
  text <- gsub("\\bcan't\\b", "cannot", text)
  text <- gsub("\\bwon't\\b", "will not", text)
  text <- gsub("\\bshouldn't\\b", "should not", text)
  text <- gsub("\\bdon't\\b", "do not", text)
  text <- gsub("\\bdidn't\\b", "did not", text)
  text <- gsub("\\bhasn't\\b", "has not", text)
  text <- gsub("\\bhaven't\\b", "have not", text)
  text <- gsub("\\b isn't\\b", " is not", text)
  text <- gsub("\\baren't\\b", "are not", text)
  text <- gsub("\\bwasn't\\b", "was not", text)
  text <- gsub("\\bweren't\\b", "were not", text)
  text <- gsub("\\b i've\\b", " I have", text)  
  text <- gsub("\\b we've\\b", " We have", text)  
  return(text)
}

data_no_contraction <- data_no_emojis %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    un_contracted <- expand_contractions_manual(x)
    
    return(un_contracted)
  })))

print(data_no_contraction)

data_corrected <- data_no_contraction %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    words <- unlist(strsplit(x, " ")) 
    
    corrected_text <- sapply(words, function(word) {
      word <- as.character(word)
      
      misspelled_words <- hunspell(word)
      
      if (length(misspelled_words[[1]]) > 0) {
        suggestions <- hunspell_suggest(misspelled_words[[1]])
        if (length(suggestions) > 0) {
          return(suggestions[[1]][1])  
        } else {
          return(word)  
        }
      } else {
        return(word)
      }
    })
    
    paste(corrected_text, collapse = " ")
  })))

print(data_corrected)

data_cleaned <- data_corrected %>%
  mutate(
    Snippet = Snippet %>%
      tolower() %>%                             
      gsub("<[^>]+>", "", .) %>%                
      gsub("[[:punct:]]+", "", .) %>%           
      gsub("[0-9]", " ", .) %>%                  
      gsub("[^[:alnum:] ]", "", .) %>%        
      gsub("\\s+", " ", .) %>%                 
      trimws()                                  
  )

data_cleaned <- data_cleaned %>%
  mutate(
    Headline = Headline %>%
      tolower() %>%                             
      gsub("<[^>]+>", "", .) %>%               
      gsub("[[:punct:]]+", "", .) %>%           
      gsub("[0-9]", " ", .) %>%                  
      gsub("[^[:alnum:] ]", "", .) %>%        
      gsub("\\s+", " ", .) %>%                  
      trimws()                                  
  )

print(data_cleaned)

data_tokenized <- data_cleaned %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    tokens <- tokenize_words(x)  
    tokens[[1]] 
  })))

print(data_tokenized)

ordinal_map <- c("first" = "one", "second" = "two", "third" = "three", 
                 "fourth" = "four", "fifth" = "five", "sixth" = "six", 
                 "seventh" = "seven", "eighth" = "eight", "ninth" = "nine", 
                 "tenth" = "ten")

data_sampled <- data_tokenized %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    words <- unlist(strsplit(x, " "))  
    
    words <- sapply(words, function(word) {
      if (word %in% names(ordinal_map)) {
        return(ordinal_map[word])  
      } else {
        return(word)  
      }
    })

    return(unname(words))
  })))

print(data_sampled)

stop_words <- stopwords("en")
print(stop_words)

data_no_stopwords <- data_sampled %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    tokens_no_stopwords <- x[!x %in% stop_words]  
    return(tokens_no_stopwords)  
  })))

print(data_no_stopwords)

data_stemmed_lemmetated <- data_no_stopwords %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
    lemmatized_text <- lemmatize_words(x)
    stemmed_text <- stem_words(lemmatized_text) 
    
    return(stemmed_text) 
  })))

print(data_stemmed_lemmetated)

data_processed <- data_stemmed_lemmetated %>%
  mutate(across(c(Snippet, Headline), 
                ~ sapply(., function(x) {
                  return(paste(unlist(x), collapse = ", "))  
                })))

print(data_processed)

write.xlsx(data_processed, "data_processed_all_cols.xlsx")







