# H2O word2vec

library(h2o)
library(tidyverse)
library(readxl)
library(ruimtehol)
library(caret)
library(tidytext)

h2o.init()


df <- read_excel("NPS_mapping.xlsx") %>%
  select(Tekst, Tag = `NPS TAG`) %>%
  mutate(Tag = str_replace_all(Tag, c("Unknow" = "Unknown","Unknownn" = "Unknown",
                                      "e-boger" = "e-bøger"))) %>%
  mutate(Tag = str_replace(Tag, " ","")) %>%
  mutate(Tag = str_to_lower(Tag),
         Tekst = str_replace_all(Tekst, "[[:punct:]]", "")) %>%
  filter(!is.na(Tag)) 

#### tokenize
my_stopwords <- c(stopwords::stopwords("danish"),"ikke")

df <- df %>% 
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word,Tekst) %>%
  mutate(word = tm::removeWords(word, c(my_stopwords,"saxo"))) %>%
  filter(word != "") %>%
  mutate(Tag = factor(Tag))

df_hf <- as.h2o(df)

word <- h2o.tokenize(df_hf$word,"\\\\W+")
word <- tokenize(df_hf$word)

# word2vec model
w2v.model <- h2o.word2vec(word, sent_sample_rate = 0, epochs = 10)

tags_vecs <- h2o.transform(w2v.model, word, aggregate_method = "AVERAGE")
valid_tags_vecs <- ! is.na(tags_vecs$C1)

data <- h2o.cbind(df_hf[valid_tags_vecs, "Tag"], tags_vecs[valid_tags_vecs, ])
data.split <- h2o.splitFrame(data, ratios = 0.8)

aml <- h2o.automl(x = names(tags_vecs), y = "Tag",
                     training_frame = data.split[[1]], 
               validation_frame = data.split[[2]],
               max_runtime_secs = 500,
               balance_classes = F)

aml@leaderboard

perf <- h2o.performance(aml@leader,valid=T)

perf@metrics$cm

pred <- predict(aml@leader,data.split[[2]])

actual <- data.split[[2]]$Tag %>% as.vector()
pred <- as_tibble(pred) %>%
  mutate(actual = actual) %>%
  select(actual,predict)

table(pred$actual==pred$predict)
prop.table(table(pred$actual==pred$predict))
