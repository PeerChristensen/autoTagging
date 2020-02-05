
library(tidyverse)
library(readxl)
library(ruimtehol)
library(caret)

df <- read_excel("NPS_mapping.xlsx") %>%
  select(Tekst, Score, Tag = `NPS TAG`) %>%
  mutate(Tag = str_replace_all(Tag, c("Unknow" = "Unknown","Unknownn" = "Unknown",
                                      "e-boger" = "e-bøger"))) %>%
  mutate(Tag = str_replace(Tag, " ","")) %>%
  mutate(Tag = str_to_lower(Tag),
         Tekst = str_replace_all(Tekst, "[[:punct:]]", "")) %>%
  filter(!is.na(Tag))

ind <- createDataPartition(df$Tag, p = 0.8, list = F)

train <- df[ind,]
test <- df[-ind,]

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
tag <- predicted %>% map(3) %>% map_chr(1)

predictions <- tibble(doc_id,tag) %>%
  left_join(test)
#view(predictions)

table(predictions$Tag==predictions$tag)
prop.table(table(predictions$Tag==predictions$tag))


#######

# predict on untagged
untagged <- read_excel("NPS_mapping.xlsx") %>%
  select(Tekst, Score, Tag = `NPS TAG`) %>%
  filter(is.na(Tag) & Tekst != "Unknown") %>%
  mutate(doc_id = row_number())



pred <- predict(model,untagged$Tekst,k=1)

doc_id <- pred %>% map_int(1)
tag <- pred %>% map(3) %>% map_chr(1)

predictions <- tibble(doc_id,tag) %>%
  left_join(untagged)
#view(predictions)

train %>% count(Tag) 

table(!is.na(df$Tag))

df <- df %>% filter(!is.na(Tag))

