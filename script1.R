# automatisk tagging af NPS kommentarer
# Peer Christensen
# januar 2020

library(tidyverse)
library(ruimtehol)
library(RODBC)

credentials <- read_rds("credentials.rds")

channel <-odbcConnect(credentials[1], uid=credentials[2], pwd=credentials[3])

query <- "SELECT [OrderFeedbackDate_Key]
      ,[Customer_Key]
      ,[OrderFeedbackScore]
	  ,[OrderFeedBackText]
  FROM [EDW].[edw].[OrderFeedbackFact] t1
  inner join [EDW].[edw].[OrderFeedbackText] t2 on t2.[OrderFeedbackText_Key] = t1.[OrderFeedbackText_Key]
  where [OrderFeedbackDate_Key] >= 20190101"

df <- sqlQuery(channel, query)
