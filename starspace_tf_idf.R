# starspace model, lemmatized, minus stop words, 

# starSpace

# lemmatized

library(udpipe)
library(tidyverse)
library(readxl)
library(ruimtehol)
library(caret)
library(tidytext)
library(ggthemes)

#udpipe_download_model(language = "danish")

lang_mod <- udpipe_load_model(file = "danish-ddt-ud-2.4-190531.udpipe")

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
  filter(word != "") %>%
  nest(word) %>%
  mutate(Tekst = map(data, unlist), 
         Tekst = map_chr(Tekst, paste, collapse = " ")) %>%
  select(-data)

x <- udpipe_annotate(lang_mod, x = df$Tekst)
x <- as_tibble(x) %>% select(doc_id,lemma)

words_remove <- x %>%
  mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>%
  left_join(df) %>%
  count(Tag,lemma,sort=T)  %>%
  bind_tf_idf(lemma, Tag, n) %>%
  filter(tf_idf < 0.0009) %>%
  pull(lemma)

x <- x %>%
  nest(lemma) %>%
  mutate(Tekst = map(data, unlist), 
         Tekst = map_chr(Tekst, paste, collapse = " ")) %>%
  select(-data) %>%
  mutate(Tekst =str_remove_all(Tekst,words_remove),
         Tekst = str_replace_all(Tekst,"  "," ")) %>%
  mutate(doc_id = row_number(),
         Tag = df$Tag)


ind <- createDataPartition(x$Tag, p = 0.8, list = F)

train <- x[ind,]

test <- x[-ind,]

test <- test %>%
  mutate(doc_id = row_number())

model <- embed_tagspace(x = train$Tekst, 
                        y = train$Tag, 
                        dim      = 100,
                        minCount = 2, 
                        ngrams   = 1, 
                        thread   = 32,
                        epoch    = 50, 
                        adagrad  = T,
                        negSearchLimit = 100, 
                        loss = "softmax")

# predict on test set
predicted <- predict(model, test$Tekst, k=1)

doc_id <- predicted %>% map_int(1)
tag_pred <- predicted %>% map(3) %>% map_chr(1)
similarity <- predicted %>% map(3) %>% map_dbl(3)

predictions <- tibble(doc_id,tag_pred, similarity) %>%
  left_join(test) %>%
  select(doc_id, Tekst, Tag, tag_pred, similarity)
#view(predictions)

table(predictions$Tag==predictions$tag_pred)
prop.table(table(predictions$Tag==predictions$tag_pred))

hist(predictions$similarity)
  
  # x %>%
  #   mutate(lemma = fct_reorder(lemma, tf_idf))  %>% 
  #   group_by(Tag) %>% 
  #   top_n(5, tf_idf) %>% 
  #   ungroup() %>%
  #   mutate(lemma = reorder(lemma, tf_idf)) %>%
  #   ggplot(aes(lemma, tf_idf, fill = Tag)) +
  #   geom_col(show.legend = FALSE) +
  #   labs(x = NULL, y = "tf-idf") +
  #   facet_wrap(~Tag, ncol = 3, scales = "free") +
  #   coord_flip()