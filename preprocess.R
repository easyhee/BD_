install.packages("tidyverse")
setwd("c:/bdata/raw")
library(tidyverse)
library(readr)

dir()

#1인 여성 가구수 데이터 불러오기
data <- read.csv("1인여성가구_2015-2022.csv",stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()

#자치구명과 여성 가구수만 추출
data <- data[, c("시점", "자치구명", "여자")]
#행 이름 수정
rownames(data) <- NULL
#시점 컬럼명 수정
colnames(data)[1] <- "연도"
#데이터 형식 통일 : 숫자로
data$여자 <- as.integer(data$여자)

crime_2015 <- read.csv("5대범죄발생현황_2015.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2016 <- read.csv("5대범죄발생현황_2016.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2017 <- read.csv("5대범죄발생현황_2017.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2018 <- read.csv("5대범죄발생현황_2018.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2019 <- read.csv("5대범죄발생현황_2019.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2020 <- read.csv("5대범죄발생현황_2020.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2021 <- read.csv("5대범죄발생현황_2021.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()
crime_2022 <- read.csv("5대범죄발생현황_2022.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8") %>% as_tibble()


crime_2015 <- crime_2015 %>% mutate(살인 =  as.integer(살인))
crime_2016 <- crime_2016 %>% mutate(살인 =  as.integer(살인))
crime_2017 <- crime_2017 %>% mutate(살인 =  as.integer(살인))
crime_2018 <- crime_2018 %>% mutate(살인 =  as.integer(살인))
crime_2019 <- crime_2019 %>% mutate(살인 =  as.integer(살인))
crime_2020 <- crime_2020 %>% mutate(살인 =  as.integer(살인))
crime_2021 <- crime_2021 %>% mutate(살인 =  as.integer(살인))
crime_2022 <- crime_2022 %>% mutate(살인 =  as.integer(살인))

crime_2015 <- crime_2015 %>% mutate(강도 =  as.integer(강도))
crime_2016 <- crime_2016 %>% mutate(강도 =  as.integer(강도))
crime_2017 <- crime_2017 %>% mutate(강도 =  as.integer(강도))
crime_2018 <- crime_2018 %>% mutate(강도 =  as.integer(강도))
crime_2019 <- crime_2019 %>% mutate(강도 =  as.integer(강도))
crime_2020 <- crime_2020 %>% mutate(강도 =  as.integer(강도))
crime_2021 <- crime_2021 %>% mutate(강도 =  as.integer(강도))
crime_2022 <- crime_2022 %>% mutate(강도 =  as.integer(강도))


crime_dt <- bind_rows(crime_2015,crime_2016,crime_2017,crime_2018,
                      crime_2019,crime_2020,crime_2021,crime_2022)

crime_dt <- crime_dt %>% 
  mutate(강도 =  as.integer(강도))

crime_dt <- crime_dt %>%
  select(자치구명, 연도, 살인, 강도, 강간, 절도, 폭력, 건수) 
View(crime_dt)

final_dt <- data %>% 
  left_join(crime_dt, by = c("자치구명", "연도"))

write.csv(final_dt, "preprocessed_data.csv", row.names = FALSE)

