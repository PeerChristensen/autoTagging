# H2O word2vec

library(h2o)
library(tidyverse)
library(readxl)
library(ruimtehol)
library(caret)
library(tidytext)

h2o.init()

w2v_predict <- function(job.title, w2v, gbm) {
  words <- tokenize(as.character(as.h2o(job.title)))
  job.title.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  h2o.predict(gbm, job.title.vec)
}


df <- read_excel("NPS_mapping.xlsx") %>%
  select(Tekst, Score, Tag = `NPS TAG`) %>%
  mutate(Tag = str_replace_all(Tag, c("Unknow" = "Unknown","Unknownn" = "Unknown",
                                      "e-boger" = "e-bøger"))) %>%
  mutate(Tag = str_replace(Tag, " ","")) %>%
  mutate(Tag = str_to_lower(Tag),
         Tekst = str_replace_all(Tekst, "[[:punct:]]", "")) %>%
  filter(!is.na(Tag))

#### tokenize
my_stopwords <- str_remove(stopwords::stopwords("danish"),"ikke")

df <- df %>% 
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word,Tekst) %>%
  mutate(word = tm::removeWords(word, c(my_stopwords,"saxo"))) %>%
  filter(word != "")

word <- as.h2o(df$word)

# word2vec model
w2v.model <- h2o.word2vec(word, sent_sample_rate = 0, epochs = 10)

job.title.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")
valid.job.titles <- ! is.na(job.title.vecs$C1)

data <- h2o.cbind(job.titles[valid.job.titles, "category"], job.title.vecs[valid.job.titles, ])
data.split <- h2o.splitFrame(data, ratios = 0.8)

gbm.model <- h2o.gbm(x = names(job.title.vecs), y = "category",
                     training_frame = data.split[[1]], validation_frame = data.split[[2]])
