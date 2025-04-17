##------------------
### pull API 
##------------------

library(httr)
library(jsonlite)
library(xml2)

# API Key and URL
api_key <- "KEY"
url <- "https://open.assembly.go.kr/portal/openapi/npffdutiapkzbfyvr?UNIT_CD=100020"

# API request
res <- GET(url, query = list(
  KEY = api_key,
  Type = "json",     
  pIndex = 1,        
  pSize = 10         
))

# parse
content <- content(res, as = "text", encoding = "UTF-8")
json_data <- fromJSON(content)


##------------------
### legislator data
##------------------

library(tidyverse)
library(stringr)

data_list <- list()

for (i in 6:21) {
  file_path <- paste0("/Users/hyoon/Desktop/Yoon2/korea data/representation/legislators", i, ".csv")
  data_list[[i - 5]] <- read.csv(file_path, fileEncoding = "EUC-KR")  
}

# congress 11-21
pol <- do.call(rbind, data_list) %>%
  `colnames<-`(c("congress_code", "congress", "name", "hanja_name", "english_name", "lunar_solar", "dob", "gender",
                 "party", "code", "district", "tenure", "tenure_congress", "national")) %>%
  mutate(congress = as.integer(str_extract(congress, "\\d+"))) %>% filter(congress > 10) %>%
  mutate(pr = case_when(national == "지역구" ~ 0,
                        national %in% c("전국구", "비례대표") ~ 1),
         province = str_split_fixed(district, " ", 2)[,1],
         district = str_split_fixed(district, " ", 2)[,2])

library(readxl)

pol22 <- read_excel("legislator22.xlsx") %>% select(-1) %>%
  `colnames<-`(c("congress", "name", "party", "committee", "district", "gender", "tenure", "pr")) %>%
  mutate(congress = 22,
         pr = case_when(pr == "지역구" ~ 0,
                        pr == "비례대표" ~ 1),
         province = str_split_fixed(district, " ", 2)[,1],
         district = str_split_fixed(district, " ", 2)[,2])

# congress 11-22
legislators <- bind_rows(pol, pol22)

# percentage of women legislators across years by district/pr
legislators %>% group_by(congress, pr) %>% summarize(female.pct = sum(gender=="여", is.na=T)/n()*100, .groups = "drop") %>%
  pivot_wider(names_from = pr, values_from = female.pct) %>%
  print(n = 100)

legislators %>% group_by(congress, pr) %>% summarize(female = sum(gender=="여", is.na=T), .groups = "drop") %>%
  pivot_wider(names_from = pr, values_from = female) %>%
  print(n = 100)


##------------------
### applicant data
##------------------


