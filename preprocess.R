install.packages("tidyverse")
setwd("c:/bdata/raw")
library(tidyverse)
library(readr)


dir()

data <- read.csv("1인여성가구_2015-2022.csv", fileEncoding = "UTF-8") %>% as_tibble()
crime <- read.csv("서울시_5대범죄발생현황_2015-2022.csv")

colnames(data)

View(data)
data_rename <- data %>%
  rename(GU_NM = 자치구명,
         GENDER = 여자,
         BASE_YEAR = 시점)

crime_rename <-crime %>%
  rename(GU_NM =자치구명,
         HOMICIDE = 살인,
         ROBBERY = 강도,
         SEXUAL_ASS = 강간_강제추행,
         THEFT = 절도,
         VIOLENCE = 폭력,
         TOTAL_CRIME = 5대범죄소계,
         BASE_YEAR = 기준년도)
