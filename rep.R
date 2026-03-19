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


##-----------------------------
### LEGISLATOR DATA 1981-2024
##-----------------------------

library(tidyverse)
library(stringr)
library(readxl)


data_list <- list()

for (i in 6:21) {
  file_path <- paste0("/Users/hyoon/Desktop/Yoon2/korea data/rep/legislators", i, ".csv")
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


pol22 <- read_excel("legislator22.xlsx") %>% select(-1) %>%
  `colnames<-`(c("congress", "name", "party", "committee", "district", "gender", "tenure", "pr")) %>%
  mutate(congress = 22,
         pr = case_when(pr == "지역구" ~ 0,
                        pr == "비례대표" ~ 1),
         province = str_split_fixed(district, " ", 2)[,1],
         district = str_split_fixed(district, " ", 2)[,2])

# congress-year
elections <- data.frame(
  year = c(1948, 1950, 1954, 1958, 1960, 1963, 1967, 1971, 1973, 1978,
           1981, 1985, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016,
           2020, 2024),
  congress = 1:22)

# congress 11-22
leg <- bind_rows(pol, pol22) %>% left_join(elections, by="congress") %>% 
  mutate(birth_year = as.integer(substr(dob, 1, 4)),
         birth_year = case_when(birth_year > 2000 ~ birth_year - 100,
                                birth_year < 1900 ~ birth_year + 100,
                                TRUE ~ birth_year),
         tenure = case_when(tenure == "초선" ~ "1선",
                            tenure == "재선" ~ "2선",
                            TRUE ~ tenure),
         tenure = as.numeric(str_sub(tenure, 1, 1)),
         district = str_remove_all(district, ","),
         age = year - as.integer(birth_year),
         female = case_when(gender == "여" ~ 1,
                            gender == "남" ~ 0),
         province = ifelse(province %in% c("null",""), NA, province)) %>%
  mutate(province = case_when(province %in% c("강원", "강원도") ~ "gw",
                              province %in% c("경기", "경기도") ~ "gg",
                              province %in% c("경남", "경상남도") ~ "gn",
                              province %in% c("경북", "경상북도") ~ "gb",
                              province %in% c("광주", "광주광역시") ~ "gj",
                              province %in% c("대구", "대구광역시") ~ "dg",
                              province %in% c("대전", "대전광역시") ~ "dj",
                              province %in% c("부산", "부산광역시") ~ "bs",
                              province %in% c("서울", "서울특별시") ~ "sl",
                              province %in% c("세종특별자치시", "세종특별자치시갑", "세종특별자치시을") ~ "sj",
                              province %in% c("울산", "울산광역시") ~ "us",
                              province %in% c("인천", "인천광역시") ~ "ic",
                              province %in% c("전남", "전라남도") ~ "jn",
                              province %in% c("전라북도", "전북") ~ "jb",
                              province %in% c("제주", "제주도") ~ "jj",
                              province %in% c("충남", "충청남도") ~ "cn",
                              province %in% c("충북", "충청북도") ~ "cb",
                              province %in% c("비례(열)", "비례(한)", "비례대표", "비례대표(신한국당)", "전국구") ~ "")) %>%
  select(year, congress, province, district, pr, party, name, birth_year, age, female, tenure) %>%
  arrange(congress) %>% group_by(name, birth_year) %>% mutate(tenure2 = row_number()) %>% ungroup() %>%
  mutate(tenure2 = ifelse(congress == 22, tenure, tenure2))

leg22 <- read_excel("국회의원현황22.xlsx", sheet = 2) %>% select(congress, name, dob) %>%
  mutate(birth_year = as.integer(substr(dob, 1, 4)),
         age = 2024 - birth_year) %>%
  right_join(leg %>% filter(year == 2024), by = c("congress", "name")) %>%
  select(-c(birth_year.y, age.y, dob)) %>% rename(birth_year = birth_year.x,
                                             age = age.x)

leg <- leg %>% filter(congress < 22) %>% bind_rows(., leg22) %>%
  mutate(party_cons = case_when(congress == 22 & party == "국민의힘" ~ 1,
                            congress == 21 & party %in% c("미래통합당","국민의힘") ~ 1,
                            congress == 20 & party%in% c("새누리당","국민의당","미래통합당") ~ 1,
                            congress == 19 & party == "새누리당" ~ 1,
                            congress == 18 & party %in% c("한나라당","새누리당") ~ 1,
                            congress == 17 & party == "한나라당" ~ 1,
                            congress == 16 & party == "한나라당" ~ 1,
                            congress == 15 & party == "신한국당" ~ 1,
                            congress == 14 & party == "민주자유당" ~ 1,
                            congress == 13 & party == "민주정의당" ~ 1,
                            congress == 12 & party == "민주정의당" ~ 1,
                            congress == 11 & party == "민주정의당" ~ 1,
                            congress == 22 & party == "더불어민주당" ~ 0,
                            congress == 21 & party == "더불어민주당" ~ 0,
                            congress == 20 & party == "더불어민주당" ~ 0,
                            congress == 19 & party == "민주통합당" ~ 0,
                            congress == 18 & party %in% c("통합민주당","민주통합당") ~ 0,
                            congress == 17 & party == "열린우리당" ~ 0,
                            congress == 16 & party == "새천년민주당" ~ 0,
                            congress == 15 & party == "새정치국민회의" ~ 0,
                            congress == 14 & party == "민주당" ~ 0,
                            congress == 13 & party == "평화민주당" ~ 0,
                            congress == 12 & party == "신한민주당" ~ 0,
                            congress == 11 & party == "민주한국당" ~ 0,
                            TRUE ~ NA),
         party_cons = as.factor(party_cons)) 
  

##-----------------------------
### Descriptive Stats
## gender
##-----------------------------

leg %>% group_by(congress) %>% summarize(fem_pct = sum(female==1, na.rm=T)/n()*100)

# percentage of female legislators 11-22 congress (pooled)

fem_pool <- leg %>% group_by(congress) %>% summarize(fem_pct = sum(female==1, na.rm=T)/n()*100,
                                                     fem_pct_main = sum(female == 1 & !is.na(party_cons), na.rm = TRUE)/sum(!is.na(party_cons))*100,
                                                     fem_pct_non = sum(female == 1 & is.na(party_cons), na.rm = TRUE)/sum(is.na(party_cons))*100) 

ggplot(fem_pool, aes(x = congress, y = fem_pct)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "red", size = 0.3) + 
  scale_x_continuous(breaks = seq(min(fem_pool$congress), max(fem_pool$congress), by = 1)) +
  labs(
#    title = "% of Female Legislators by Congress",
    x = "Congress",
    y = "Female (%)") +
  theme_minimal()

ggsave("pct_years.png", plot = last_plot(),
      width = 6, height = 4)

# percentage of female legislators 11-22 congress (by party status)

fem_pool_two <- fem_pool %>% pivot_longer(cols = c(fem_pct, fem_pct_main, fem_pct_non), names_to = "type", values_to = "percent")

ggplot(fem_pool_two %>% filter(type != "fem_pct"), aes(x = congress, y = percent, color = type)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "red", size = 0.3) + 
  scale_x_continuous(breaks = seq(min(fem_pool_two$congress), max(fem_pool_two$congress), by = 1)) +
  labs(
#    title = "% of Female Legislators by Congress (by Party Status)",
    x = "Congress",
    y = "Female (%)",
    color = "Parties") +
  scale_color_manual(
    values = c("fem_pct_main" = "tomato", "fem_pct_non" = "steelblue"),
    labels = c("fem_pct_main" = "Major Parties", "fem_pct_non" = "Non-major Parties")) + 
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("pct_years_byparty.png", plot = last_plot(),
       width = 6, height = 4)

# two main parties--liberal, conservative

fem_party <- leg %>%
  group_by(congress, party_cons) %>%
  summarize(female.pct = mean(female == 1, na.rm = TRUE) * 100, .groups = "drop")

ggplot(fem_party %>% filter(!is.na(party_cons)), aes(x = congress, y = female.pct, group = party_cons, color = party_cons)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "red", size = 0.3) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  labs(
    x = "Congress",
    y = "Percentage of Women Legislators",
    color = "Party",
#    title = "Women Legislators by Party and Congress"
) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "darkred"),
    labels = c("0" = "Liberal", "1" = "Conservative")) +
  theme_minimal() +
  theme(legend.position = "bottom")


# percentage of female legislators across years by district/pr

# pooled

leg %>% group_by(congress, pr) %>% summarize(fem.pct = sum(female == 1, na.rm = TRUE)/n()* 100, .groups = "drop")

fem_pool2 <- leg %>% group_by(congress, pr) %>% summarize(fem.pct = sum(female == 1, na.rm = TRUE)/n()* 100, .groups = "drop") %>%
  mutate(pr = as.character(pr))

ggplot(fem_pool2, aes(x=congress, y=fem.pct, color=pr)) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept = 17, linetype = "dashed", color = "red", size = 0.3) + 
  scale_x_continuous(breaks = seq(min(fem_all$congress), max(fem_all$congress), by = 1)) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "darkorange"),
    labels = c("0" = "District", "1" = "PR")) +
  labs(
    x = "Congress",
    y = "Percentage Female",
    color = "PR Status",
#    title = "% of Female Legislators by PR Status"
) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("pct_years_bypr.png", plot = last_plot(),
       width = 6, height = 4)

# by party (lib v. cons)

fem2 <- leg %>% group_by(congress, pr, party_cons) %>%
  summarize(female.pct = sum(female == 1, na.rm = TRUE)/n()* 100, .groups = "drop") %>%
  mutate(pr = as.character(pr))

#fem_total <- leg %>% group_by(congress, party_cons) %>%
#  summarize(pr = "pooled", female.pct = sum(female == 1, na.rm = TRUE)/n()*100, .groups = "drop")
#fem_all <- bind_rows(fem, fem_total)

ggplot(fem2 %>% filter(!is.na(party_cons)), aes(x = congress, y = female.pct, color = pr, group = pr)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "red", size = 0.3) + 
  scale_x_continuous(breaks = seq(min(fem_all$congress), max(fem_all$congress), by = 1)) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "darkorange"),
    labels = c("0" = "District", "1" = "PR")) +
  labs(
    x = "Congress",
    y = "Percentage Female",
    color = "PR Status",
#    title = "Percentage of Female Legislators by Party"
) +
  facet_wrap(~ party_cons, labeller = as_labeller(c("0" = "Liberal", "1" = "Conservative"))) +
  theme_minimal() + 
  theme(legend.position = "bottom")

ggsave("pct_years_byprparty.png", plot = last_plot(),
       width = 6, height = 4)

##-----------------------------
### Descriptive Stats
## age
##-----------------------------

# distribution of age across years
ggplot(leg, aes(x=age)) + 
  geom_histogram(binwidth = 5, 
                 fill    = "steelblue", 
                 color   = "white") + 
  facet_wrap(~congress) +
  labs(
    title = "Distribution of Age Across Years",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()

# under 40
# 24년 민주당 총선 기준 45세 가산점
# 24년 국힘 총선 34세 20% 가산점 / 45세 15% 가산점

leg %>% group_by(congress) %>% summarize(num_50 = sum(age<50, na.rm=T),
                                         pct_50 = sum(age<50, na.rm=T)/n()*100,
                                         num_45 = sum(age<45, na.rm=T),
                                         pct_45 = sum(age<45, na.rm=T)/n()*100,
                                         num_40 = sum(age<40, na.rm=T),
                                         pct_40 = sum(age<40, na.rm=T)/n()*100,
                                         num_35 = sum(age<35, na.rm=T),
                                         pct_35 = sum(age<35, na.rm=T)/n()*100)

age_plot <- function(df, threshold) {
  
  # by party
  age <- df %>%
    group_by(congress, party_cons) %>%
    summarize(pct = mean(age < threshold, na.rm = TRUE) * 100, .groups = "drop")
  
  # pooled
  pooled <- df %>%
    group_by(congress) %>%
    summarize(pct = mean(age < threshold, na.rm = TRUE) * 100, .groups = "drop") %>%
    mutate(party_cons = "pooled")
  
  # combine
  age_data <- bind_rows(age, pooled) %>% drop_na(party_cons)
  
  # plot
  ggplot(age_data, aes(x = congress, y = pct, color = party_cons)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = pretty(unique(df$congress), n = 10)) +
    labs(
      title = paste0("Percentage of Politicians Under Age ", threshold, " by Party"),
      x = "Congress",
      y = paste0("Percent Under ", threshold),
      color = "Party"
    ) +
    theme_minimal()
}

age_plot(leg, 35)
age_plot(leg, 40)
age_plot(leg, 45)
age_plot(leg, 50)

# by district/pr
age_plot_pr <- function(df, threshold) {
  
  # calculate percentage
  age_pr <- df %>%
    group_by(congress, pr) %>%
    summarize(
      pct_under = mean(age < threshold, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(pr_label = ifelse(pr == 1, "pr", "district"))
  
  # plot
  ggplot(age_pr, aes(x = congress, y = pct_under, color = pr_label)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = pretty(unique(df$congress), n = 10)) +
    labs(
      title = paste0("Percentage of Politicians Under Age ", threshold, " by pr/district"),
      x = "Congress",
      y = paste0("Percent Under ", threshold),
      color = "PR/District"
    ) +
    theme_minimal()
}

age_plot_pr(leg, 35)
age_plot_pr(leg, 40)
age_plot_pr(leg, 45)
age_plot_pr(leg, 50)


##-----------------------------
### APPLICANT DATA 2012-2020
##-----------------------------

mp21 <- read_excel("applicant_list.xlsx", sheet=1) %>% slice(-c(1,2)) %>% select(-1) %>%
  `colnames<-`(c("province", "district", "num.app", "applicant", "exp", "gender", "age", "tenure", "incumbent")) %>% 
  fill(district, num.app) %>% mutate(year = 2020,
                                     congress = 21,
                                     party = "mp",
                                     age = year - as.integer(str_extract(age, "\\d+")),
                                     tenure = as.integer(str_extract(tenure, "\\d+")),
                                     incumbent = case_when(incumbent == "현직" ~ 1,
                                                           incumbent == "비현직" ~ 0),
                                     num.app = as.numeric(num.app),
                                     female = case_when(gender == "여성" ~ 1,
                                                        gender == "남성" ~ 0),
                                     district = str_remove_all(district, "\\(|\\)"))

ppp21 <- read_excel("applicant_list.xlsx", sheet=2) %>% slice(-c(1,2)) %>% select(-1) %>%
  `colnames<-`(c("province", "district", "num.app", "applicant", "exp", "gender", "age", "tenure", "incumbent")) %>% 
  fill(district, num.app) %>% mutate(year = 2020,
                                     congress = 21,
                                     party = "ppp",
                                     age = year - as.integer(str_extract(age, "\\d+")),
                                     tenure = as.integer(str_extract(tenure, "\\d+")),
                                     incumbent = case_when(incumbent == "현직" ~ 1,
                                                           incumbent == "비현직" ~ 0),
                                     num.app = as.numeric(num.app),
                                     female = case_when(gender == "여성" ~ 1,
                                                        gender == "남성" ~ 0),
                                     district = str_remove_all(district, "\\(|\\)")) %>%
  slice(-c(865:n()))

mp20 <- read_excel("applicant_list.xlsx", sheet=3) %>% slice(-c(1,2)) %>% select(-c(1,7)) %>%
  `colnames<-`(c("province", "district", "num.app", "applicant", "exp", "gender", "age", "tenure", "incumbent")) %>% 
  fill(district, num.app) %>% mutate(year = 2016,
                                     congress = 20,
                                     party = "mp",
                                     age = year - as.integer(str_extract(age, "\\d+")),
                                     tenure = as.integer(str_extract(tenure, "\\d+")),
                                     incumbent = case_when(incumbent == "현직" ~ 1,
                                                           incumbent == "비현직" ~ 0),
                                     num.app = as.numeric(num.app),
                                     female = case_when(gender == "여성" ~ 1,
                                                        gender == "남성" ~ 0),
                                     district = str_remove_all(district, "\\(|\\)"))

ppp20 <- read_excel("applicant_list.xlsx", sheet=4) %>% slice(-c(1,1)) %>%
  `colnames<-`(c("province", "district", "applicant", "exp", "gender", "age", "tenure", "incumbent")) %>%
  fill(province, district) %>% mutate(
    year = 2016,
    congress = 20,
    party = "ppp",
    num.app = str_extract(district, "\\(\\d+명\\)"),                    
    num.app = str_extract(num.app, "\\d+"),                       
    num.app = as.numeric(num.app),
    district = str_trim(str_extract(district, "^[^\\(]+")),
    district = str_remove_all(district, "[\\s\\r\\n\\·]"),
    age = year - as.integer(str_extract(age, "\\d+")),
    tenure = as.integer(str_extract(tenure, "\\d+")),
    incumbent = case_when(incumbent == "현직" ~ 1,
                          incumbent == "비현직" ~ 0),
    num.app = as.numeric(num.app),
    female = case_when(gender == "여성" ~ 1,
                       gender == "남성" ~ 0))

mp19 <- read_excel("applicant_list.xlsx", sheet=5) %>% slice(-1) %>%
  `colnames<-`(c("province", "district", "applicant", "exp", "gender", "age", "tenure", "incumbent")) %>%
  fill(province, district) %>% mutate(
    year = 2012,
    congress = 19,
    party = "mp",
    num.app = str_extract(district, "\\(\\d+\\)"),                    
    num.app = str_extract(num.app, "\\d+"),                       
    num.app = as.numeric(num.app),
    district = str_trim(str_extract(district, "^[^\\(]+")),
    district = str_remove_all(district, "[\\s\\r\\n\\·]"),
    age = year - as.integer(str_extract(age, "\\d+")),
    tenure = as.integer(str_extract(tenure, "\\d+")),
    incumbent = case_when(incumbent == "현직" ~ 1,
                          incumbent == "비현직" ~ 0),
    num.app = as.numeric(num.app),
    female = case_when(gender == "여성" ~ 1,
                       gender == "남성" ~ 0))

ppp19 <- read_excel("applicant_list.xlsx", sheet=6) %>% slice(-1) %>%
  `colnames<-`(c("province", "district", "applicant", "exp", "gender", "age", "tenure", "incumbent")) %>%
  fill(province, district) %>% mutate(
    year = 2012,
    congress = 19,
    party = "ppp",
    num.app = str_extract(district, "\\(\\d+명\\)"),                    
    num.app = str_extract(num.app, "\\d+"),                       
    num.app = as.numeric(num.app),
    district = str_trim(str_extract(district, "^[^\\(]+")),
    district = str_remove_all(district, "[\\s\\r\\n\\·]"),
    age = year - as.integer(str_extract(age, "\\d+")),
    age = ifelse(age > 100, year-age, age),
    tenure = as.integer(str_extract(tenure, "\\d+")),
    incumbent = case_when(incumbent == "현직" ~ 1,
                          incumbent == "비현직" ~ 0),
    num.app = as.numeric(num.app),
    female = case_when(gender == "여성" ~ 1,
                       gender == "남성" ~ 0))

app <- rbind(mp19, mp20, mp21, ppp19, ppp20, ppp21) %>% 
  select(year, congress, province, district, party, num.app, applicant, exp, female, age, tenure, incumbent) %>%
  mutate(province = case_when(province == "강원" ~ "gw",
                              province == "경기" ~ "gg",
                              province == "경남" ~ "gn",
                              province == "경북" ~ "gb",
                              province == "광주" ~ "gj",
                              province == "대구" ~ "dg",
                              province == "대전" ~ "dj",
                              province == "부산" ~ "bs",
                              province == "서울" ~ "sl",
                              province == "세종" ~ "sj",
                              province == "울산" ~ "us",
                              province == "인천" ~ "ic",
                              province == "전남" ~ "jn",
                              province == "전북" ~ "jb",
                              province == "제주" ~ "jj",
                              province == "충남" ~ "cn",
                              province == "충북" ~ "cb"),
         congress = case_when(year == 2012 ~ 19,
                              year == 2016 ~ 20,
                              year == 2020 ~ 21)) %>% 
  mutate(birth_year = year - age) %>%
  rename(name = applicant)

# problem: tenure is set to 2024 status
# need to adjust to the tenure to the year of
# if i join it with legislator data, i can partially solve this problem
# but if the applicant who had tenure but didn't get elected that year, there wouldn't be a match
# also can't tell apart those who held position before 2012 

# clean key variables for matching

app <- app %>% mutate(name = name |> 
                        str_squish() |> 
                        str_replace_all(" ", "") |> 
                        stringi::stri_trans_nfc(),
                      birth_year = as.integer(birth_year),
                      year = as.integer(year))

leg <- leg %>% mutate(name = name |> 
                        str_squish() |> 
                        str_replace_all(" ", "") |> 
                        stringi::stri_trans_nfc(),
                      birth_year = as.integer(birth_year),
                      year = as.integer(year))

# join with legislator data and pull the tenure of the prior election year if they held office

library(fuzzyjoin)

app_matched <- app %>% rename(app_year = year) %>% 
  fuzzy_left_join(
    leg %>% select(year, name, birth_year, tenure2),
    by = c("app_year" = "year", "name" = "name", "birth_year" = "birth_year"),
    match_fun = list(`>`, `==`, ~ abs(.x - .y) <= 1)
  ) %>%
  group_by(app_year, name.x, birth_year.x) %>%
  slice_max(order_by = year, n = 1, with_ties = F, na_rm = F) %>%
  ungroup()  %>%
  mutate(tenure_match = tenure2) %>%
  select(-name.y, -birth_year.y, -year, -tenure2) %>% 
  rename(
    name = name.x,
    birth_year = birth_year.x,
    year = app_year)

# add matched tenure variable to app dataset
app_match <- app %>% left_join(app_matched, by = names(app)) %>% relocate(tenure, tenure_match, .after = last_col())

# additionally match for those that didn't match
# check whether they were elected that year and match for those (then -1 bc they didn't win yet at the moment of application)

nomatch <- app_match %>% filter(is.na(tenure_match) & tenure != 0) %>% 
  left_join(leg %>% select(year, name, birth_year, tenure2), by = c("year", "name", "birth_year")) %>%
  mutate(tenure_match_new = tenure2 - 1) %>% select(-tenure2)

app_match2 <- app_match %>% left_join(nomatch, by = names(app_match)) %>% 
  mutate(tenure_match = if_else(is.na(tenure_match) & tenure != 0, tenure_match_new, tenure_match),
         tenure_match = ifelse(is.na(tenure_match) & tenure == 0, 0, tenure_match)) %>%
  select(-tenure_match_new)

# there does seem to be 159 entries that don't match
# but there are matches that I can identify but didn't get matched (e.g. 주호영, 안상수)
app_match2 %>% filter(is.na(tenure_match))

# code them as 0
app2 <- app_match2 %>% mutate(tenure_match = ifelse(is.na(tenure_match), 0, tenure_match))


##-----------------------------
### Descriptive Stats
## applicants
##-----------------------------

# female applicant trend (pooled)

app2 %>%
  group_by(congress) %>%
  summarize(pct.fem = sum(female == 1, na.rm = TRUE) / n()) %>%
  ggplot(aes(x = congress, y = pct.fem)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(app2$congress), max(app2$congress), by = 1))

# trend (by party)

app2 %>%
  group_by(congress, party) %>%
  summarize(pct.fem = sum(female == 1, na.rm = TRUE) / n(), .groups = "drop") %>%
  ggplot(aes(x = congress, y = pct.fem, color = party)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(app2$congress), max(app2$congress), by = 1))

app2 %>%
  group_by(congress, party) %>%
  summarize(pct.fem = sum(female == 1, na.rm = TRUE) / n(), .groups = "drop") %>%
  ggplot(aes(x = factor(congress), y = pct.fem, fill = party)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_fill_manual(
    values = c("mp" = "#5B9BD5", "ppp" = "#E15759"),
    labels = c("mp" = "MP", "ppp" = "PPP")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Congress", y = "% Female", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("app_trend.png", plot = last_plot(),
       width = 6, height = 4)

# number of female legislators
app2 %>%
  group_by(congress, party) %>%
  summarize(n_fem = sum(female == 1, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(congress), y = n_fem, fill = party)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_fill_manual(
    values = c("mp" = "#5B9BD5", "ppp" = "#E15759"),
    labels = c("mp" = "MP", "ppp" = "PPP")
  ) +
  labs(x = "Congress", y = "Number of Female Legislators", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# district level female applicants (pooled)

app2_d <- app2 %>% group_by(congress, district) %>% summarize(n = n(), 
                                                              n.fem = sum(female, na.rm=T),
                                                              pct.fem = n.fem/n*100)

ggplot(app2_d, aes(x=pct.fem)) + geom_histogram()

ggplot(app2_d, aes(x = pct.fem)) +
  geom_density(fill = "#5B9BD5", alpha = 0.6) +
  labs(x = "% Female", y = "Density") +
  theme_minimal()

ggplot(app2_d, aes(x = n.fem)) + 
  geom_histogram(fill = "#5B9BD5", binwidth=0.5) + 
  scale_x_continuous(breaks = seq((min(app2_d$n.fem, na.rm = TRUE)),
                                  (max(app2_d$n.fem, na.rm = TRUE)), 
                                  by = 1)) +
  labs(x = "Number of Female Applicants", y = "Count") +
  theme_minimal()


# by party

app2 %>% group_by(congress, district, party) %>% summarize(n = n(), 
                                                    n.fem = sum(female, na.rm=T),
                                                    pct.fem = n.fem/n*100, .groups = "drop") %>%
  ggplot(aes(x = n.fem, fill = party)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  labs(x = "Number of Female Applicants", y = "Count", fill = NULL) +
  scale_fill_manual(values = c("mp" = "#5B9BD5", "ppp" = "#E15759"),
                    labels = c("mp" = "MP", "ppp" = "PPP")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("app_trendcount.png", plot = last_plot(),
       width = 6, height = 4)


##-----------------------------
### ELECTION DATA 1981-2024
##-----------------------------

data <- read.csv("clean_data2.csv") %>% select(-X) %>% mutate()

data <- data %>% mutate( #clean_data2.csv i think? but for some reason different outcome
  prior.pri1 = ifelse(congress == 19, 0, prior.pri1),
  prior.pri2 = ifelse(congress == 19, 0, prior.pri2)) %>%
  mutate(ruling = case_when(
    congress == 19 & party == "ppp" ~ 1, 
    congress == 20 & party == "ppp" ~ 1,
    congress == 21 & party == "mp" ~ 1,
    congress == 22 & party == "ppp" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(district = gsub("·", "", .$district)) %>%
  mutate(district = gsub(" ", "", .$district)) %>%
  mutate_at(c("inc", "for.inc"), .funs=as.numeric) # %>% select(-X)

mp22 <- read_excel("/Users/hyoon/Desktop/Yoon2/primary/election data/elec22_cand.xlsx", 
                   col_names = c("province", "district", "cand"),
                   sheet=1) %>%
  mutate(province = case_when(province == "강원도" ~ "gw",
                              province == "경기도" ~ "gg",
                              province == "경상남도" ~ "gn",
                              province == "경상북도" ~ "gb",
                              province == "광주광역시" ~ "gg",
                              province == "대구광역시" ~ "dg",
                              province == "대전광역시" ~ "dj",
                              province == "부산광역시" ~ "bs",
                              province == "서울특별시" ~ "sl",
                              province == "세종" ~ "sj",
                              province == "울산광역시" ~ "us",
                              province == "인천광역시" ~ "ic",
                              province == "전라남도" ~ "jn",
                              province == "전라북도" ~ "jb",
                              province == "제주" ~ "jj",
                              province == "충청남도" ~ "cn",
                              province == "충청북도" ~ "cb")) %>% group_by(district) %>% slice(-1) %>%
  mutate(inc = ifelse(str_detect(cand, "국회의원") & !str_detect(cand, "전 국회의원"), 1, 0))

mp22.n <- mp22 %>% group_by(province, district) %>% summarize(n.app = n(),
                                                              inc = ifelse(any(inc == 1), 1, 0),
                                                              party = "mp") %>% ungroup()

ppp22 <- read_excel("/Users/hyoon/Desktop/Yoon2/primary/election data/elec22_cand.xlsx", 
                    col_names = c("province", "district", "cand"),
                    sheet=2) %>%
  mutate(province = case_when(province == "강원" ~ "gw",
                              province == "경기" ~ "gg",
                              province == "경남" ~ "gn",
                              province == "경북" ~ "gb",
                              province == "광주" ~ "gg",
                              province == "대구" ~ "dg",
                              province == "대전" ~ "dj",
                              province == "부산" ~ "bs",
                              province == "서울" ~ "sl",
                              province == "세종" ~ "sj",
                              province == "울산" ~ "us",
                              province == "인천" ~ "ic",
                              province == "전남" ~ "jn",
                              province == "전북" ~ "jb",
                              province == "제주" ~ "jj",
                              province == "충남" ~ "cn",
                              province == "충북" ~ "cb")) %>% group_by(district) %>% slice(-1) %>%
  mutate(inc = ifelse(str_detect(cand, "국회의원") & !str_detect(cand, "전 국회의원"), 1, 0))

ppp22.n <- ppp22 %>% group_by(province, district) %>% summarize(n.app = n(),
                                                                inc = ifelse(any(inc == 1), 1, 0),
                                                                party = "ppp") %>% ungroup()

n22 <- rbind(mp22.n, ppp22.n) %>% mutate(district = gsub("·", "", .$district)) %>%
  mutate(district = gsub(" ", "", .$district)) %>% mutate(congress = 22)


data <- left_join(data, n22, by=c("congress", "province", "district", "party")) %>%
  mutate(n.app = coalesce(n.app.x, n.app.y),
         inc = coalesce(inc.x, inc.y)) %>%
  select(-c("n.app.x", "n.app.y", "inc.x", "inc.y"))

data <- data %>% mutate(
  mar23 = ifelse(w_margin_1 >= 23, 1, 0)) %>%
  mutate(mar23p = ifelse(is.na(vs_1)==F & is.na(mar23)==T, 0, mar23)) %>%
  mutate(mar23 = ifelse(is.na(mar23)==T, 0, mar23)) %>%
  mutate(vs_1_p = vs_1/100,
         avg_vs4_p = avg_vs4/100,
         avg_vs3_p = avg_vs3/100,
         avg_vs2_p = avg_vs2/100)

data <- data %>% mutate(mar_cont = ifelse(is.na(vs_1)==F & is.na(w_margin_1)==T, 0, w_margin_1),
                        mar_cont2 = ifelse(is.na(mar_cont)==T, 0, mar_cont),
                        mar_cont = mar_cont/100,
                        mar_cont2 = mar_cont2/100)


##-------------------------
## add final nominee data (gender)
##-------------------------

mp <- read_xlsx("/Users/hyoon/Desktop/Yoon2/korea data/rep/elec_mp.xlsx") %>% 
  rename(congress = elec) %>% arrange(congress, province, district) %>%
  mutate(n.app = ifelse(is.na(n.app)==T & congress %in% c(19,20), 0, n.app),
         inc = ifelse(is.na(inc)==T& congress %in% c(19,20), 0, inc)) %>% 
  mutate(pri1 = case_when(
    csm %in% c("/단일화경선", "경선", "경선?", "경선(결선)",
               "경선/단일화경선", "경선공천", "단수후보/단일화경선",
               "단일화경선", "야권단일화경선", "전략경선", "전략공천/단일화경선") ~ "1",
    csm %in% c("공천", "단수공천", "단수추천", "단수후보", 
               "야권연대", "원외단수", "전략검토지역", "전략공천", 
               "전략선거구", "현역단수") ~ "0",
    csm %in% c("등록무효", "무공천") ~ NA,
    TRUE ~ csm),
    pri2 = case_when( # 단일화는 비경선
      csm %in% c("경선", "경선?", "경선(결선)",
                 "경선/단일화경선", "경선공천", "전략경선") ~ "1",
      csm %in% c("/단일화경선", "공천", "단수공천", "단수추천",
                 "단수후보", "단수후보/단일화경선", "단일화경선", 
                 "야권단일화경선", "야권연대", "원외단수", "전략검토지역",
                 "전략공천", "전략공천/단일화경선", "전략선거구", "현역단수") ~ "0",
      csm %in% c("등록무효", "무공천") ~ NA,
      TRUE ~ csm)) %>% 
  select(-c("source", "rf")) %>% 
  mutate(across(everything(), ~gsub("\\*", "", .x))) %>%
  mutate_at(c("congress", "n.app", "pri1", "pri2"), .funs = as.numeric) %>%
  mutate(party_kor = case_when(
    congress == 19 ~ "민주통합당",
    congress == 20 ~ "더불어민주당",
    congress == 21 ~ "더불어민주당",
    congress == 22 ~ "더불어민주당")) %>%
  add_column(party = "mp",
             party_k22 = "더불어민주당",
             party_k21 = "더불어민주당",
             party_k20 = "더불어민주당",
             party_k19 = "민주통합당",
             party_k18 = "통합민주당",
             party_k17 = "열린우리당",
             party_k16 = "새천년민주당",
             party_k15 = "새정치국민회의")

ppp <- read_xlsx("/Users/hyoon/Desktop/Yoon2/korea data/rep/elec_ppp.xlsx") %>% 
  rename(congress = elec) %>% arrange(congress, province, district) %>%
  mutate(n.app = ifelse(is.na(n.app)==T & congress %in% c(19,20), 0, n.app),
         inc = ifelse(is.na(inc)==T & congress %in% c(19,20), 0, inc)) %>% 
  mutate(pri1 = case_when(
    csm %in% c("경선", "경선(국민참여)", "경선(단독후보)", "경선(여론조사)",
               "경선공천", "경선단독후보") ~ "1",
    csm %in% c("공천", "단독공천", "단수공천", "단수추천", "여성우선추천", "국민추천제",
               "우선추천", "전략공천", "청년공천", "후보자추천", "후보자추천(전략지역)") ~ "0",
    TRUE ~ csm),
    pri2 = pri1) %>%
  select(-c("source", "ref")) %>% 
  mutate(across(everything(), ~gsub("\\*", "", .x))) %>%
  mutate_at(c("congress", "n.app", "pri1", "pri2"), .funs = as.numeric) %>%
  mutate(nominee = case_when(
    nominee == "X" ~ "NA", 
    nominee == "김경희(여)" ~ "김경희",
    nominee == "김희정(여)" ~ "김희정",
    nominee == "나경원(여)" ~ "나경원",
    TRUE ~ nominee)) %>%
  mutate(party_kor = case_when(
    congress == 19 ~ "새누리당",
    congress == 20 ~ "새누리당",
    congress == 21 ~ "미래통합당",
    congress == 22 ~ "국민의힘")) %>%
  add_column(party = "ppp",
             party_k22 = "국민의힘",
             party_k21 = "미래통합당",
             party_k20 = "새누리당",
             party_k19 = "새누리당",
             party_k18 = "한나라당",
             party_k17 = "한나라당",
             party_k16 = "한나라당",
             party_k15 = "신한국당")

dat <- bind_rows(ppp, mp)

# match with app2 based on name, congressional year, party to bring gender
dat2 <- dat %>%
  left_join(
    app2 %>%
      arrange(congress, province, party, name) %>%              
      distinct(congress, province, party, name, .keep_all = TRUE) %>%
      select(congress, province, party, name, female, age, incumbent, tenure_match),
    by = c("congress", "province", "party", "nominee" = "name"),
    relationship = "many-to-one")


##-----------------------------
### Add voting data to applicant data
##-----------------------------

# aggregate applicant data to district level
app2_d <- app2 %>% group_by(congress, province, district, party) %>% summarize(n = n(), 
                                                              n.fem = sum(female, na.rm=T),
                                                              pct.fem = n.fem/n*100,
                                                              fem_inc = sum(female==1 & incumbent ==1, na.rm=T),
                                                              inc_race = sum(incumbent == 1, na.rm=T)) %>%
  mutate(inc_race = ifelse(inc_race > 0, 1, 0))


# combine applicant data with voting records data
app_match <- read_excel("/Users/hyoon/Desktop/Yoon2/korea data/rep/app_match.xlsx") %>%
  rename(district_app = district,
         district_data = district_data_match) %>% 
  mutate(district_data = ifelse(is.na(district_data), district_app, district_data)) %>%
  right_join(app2_d, by = c("congress", "province", "district_app"="district")) %>%
  right_join(data, by = c("congress", "province", "district_data"="district", "party")) %>%
  mutate(win = ifelse(is.na(w_margin_1), 0, 1))

# add gender of winner
# choose first obs for duplicates b/c second is 재보궐 winner from leg dataset
winner <- leg %>% filter(congress >= 19 & pr == 0) %>% 
  select(congress, province, district, party, female) %>%
  rename(fem_winner = female) %>% 
  mutate(party = case_when(congress == 19 & party %in% c("민주통합당", "더불어민주당") ~ "mp",
                           congress == 19 & party == "새누리당" ~ "ppp",
                           congress == 20 & party == "더불어민주당" ~ "mp",
                           congress == 20 & party %in% c("새누리당","국민의당","미래통합당") ~ "ppp",
                           congress == 21 & party == "더불어민주당" ~ "mp",
                           congress == 21 & party %in% c("미래통합당","국민의힘") ~ "ppp",
                           TRUE ~ party)) %>%
  rename(party_winner = party) %>% 
  group_by(congress, province, district) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(congress, province, district, party_winner, fem_winner)

election <- app_match %>% mutate(district_data = district_data |> 
                                   str_squish() |> 
                                   stringi::stri_trans_nfc()) %>% 
  left_join(winner %>% mutate(district = district |> 
                                str_squish() |> 
                                stringi::stri_trans_nfc()), 
            by=c("congress", "province", "district_data"="district")) %>% 
  filter(!is.na(district_app))


# distribution of women candidates (pri v. direct)

# pooled
election %>% group_by(congress, pri2) %>% summarize(n = sum(n, na.rm=T),
                                                    n_female = sum(n.fem, na.rm=T),
                                                    pct_female = n_female/n*100, .groups="drop") %>%
  filter(!is.na(pri2)) %>%
  ggplot(aes(x = factor(congress), y = pct_female, fill = factor(pri2))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(
    x = "Congress",
    y = "Percent Female Applicants",
    fill = NULL,
#    title = "Female Applicant % by Nomination Method and Congress"
  ) +
  scale_fill_manual(values = c("0" = "gray70", "1" = "steelblue"),
                    labels = c("Non-primary", "Primary")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# by party
election %>% group_by(congress, pri2, party) %>% summarize(n = sum(n, na.rm=T),
                                                    n_female = sum(n.fem, na.rm=T),
                                                    pct_female = n_female/n*100, .groups="drop") %>%
  filter(!is.na(pri2)) %>%
  ggplot(aes(x = factor(congress), y = pct_female, fill = factor(pri2))) +
     geom_bar(stat = "identity", position = "dodge") +
     facet_wrap(~party) +
     labs(
         x = "Congress",
         y = "Percent Female Applicants",
         fill = "Nomination Method",
         title = "Female Applicant % by Nomination Method, Party, and Congress"
       ) +
     scale_fill_manual(values = c("0" = "gray70", "1" = "steelblue"),
                                             labels = c("Non-primary", "Primary")) +
     theme_minimal()

# victory rate of women (pri v. direct)

election %>% filter(n.fem > 0 & !is.na(pri2)) %>% group_by(pri2) %>% summarize(mean = mean(fem_winner, na.rm=T))

# pooled
election %>%
  filter(n.fem > 0 & !is.na(pri2)) %>%
  group_by(pri2, congress) %>%
  summarize(mean = mean(fem_winner, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = congress, y = mean, color = factor(pri2))) +
  geom_line() +
  geom_point() +
  scale_color_manual(
    values = c("0" = "#999999", "1" = "#0072B2"),
    labels = c("Non-primary", "Primary"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) + 
  labs(
    x = "Congress",
    y = "Share of Female Winners",
#    title = "Female Candidate Success by Nomination Method Over Time"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("election_winrate.png", plot = last_plot(),
       width = 6, height = 4)

# by party

election %>%
  filter(n.fem > 0 & !is.na(pri2)) %>%
  group_by(pri2, congress, party) %>%
  summarize(mean = mean(fem_winner, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(congress), y = mean, color = factor(pri2), group = pri2)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ party, labeller = labeller(party = c(
    "mp" = "MP",
    "ppp" = "PPP"
  ))) +
  scale_color_manual(
    values = c("0" = "#999999", "1" = "#0072B2"),
    labels = c("Non-primary", "Primary"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Congress",
    y = "Share of Female Winners",
#    title = "Female Candidate Success by Nomination Method and Party"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
  )

ggsave("election_winratebyparty.png", plot = last_plot(),
       width = 6, height = 4)

# leg: legislator data
# app2: applicant data
# dat: has final nominee data

# add characteristics of final nominee
election <- election %>% left_join(dat2 %>% select(congress, province, district, party, nominee, female, age, incumbent, tenure_match),
                                   by=c("congress", "province", "district_data" = "district", "party")) 

# save file
#library(writexl)
#write_xlsx(election, "election_data_export.xlsx")

# filled in missing nominee, female, demographic data on election_data_export.xlsx
# duplicates: 2020 종로구, 원주시갑, 성남시분당구갑, 안성시, 창원시의창구, 수성구을, 중구남구, 종로구, 전주시을, 청주시상당구
# missing few districts (no nominees for ppp): 2020 세종갑/광주/전남/전북
# added data on demographics of the final nominees by party

election2 <- read_excel("election_data_export.xlsx") %>% 
  group_by(congress, province, district_data, party) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(year, province, district_data, party, nominee,
         female2, age2, incumbent2, tenure_match2) %>% 
  right_join(election, by = c("year", "province", "district_data", "party", "nominee"))


##-----------------------------
### Descriptive Stats- where do they run
##-----------------------------

# using "election" dataset
# open/incumbent (inc_race)
# female/male incumbent (fem_inc)
# safe/swing (avg_vs4, vs_1)
# dem/rep (party)
# metro/suburban
# ruling/non-ruling (ruling)
# past method (prior.pri2)
# past electoral performance (past female winner, past number of applicants, district demographics)


# binary female applicants districts
election %>% mutate(fem_district = ifelse(n.fem > 0, 1, 0),
                    dem_party = ifelse(party == "mp", 1, 0)) %>%
  group_by(fem_district) %>% 
  summarise(
    inc_race = mean(inc_race, na.rm=TRUE),
    fem_inc = mean(fem_inc, na.rm=TRUE),
    avg_vs4_p = mean(avg_vs4_p, na.rm=TRUE),
    vs_1_p = mean(vs_1_p, na.rm=TRUE),
    dem_party = mean(dem_party, na.rm=TRUE),
    prior.pri2 = mean(prior.pri2, na.rm=TRUE),
    ruling = mean(ruling, na.rm=TRUE),
    n = n()
  )

# continuous female applicants districts  
election %>% mutate(dem_party = ifelse(party == "mp", 1, 0)) %>%
  group_by(n.fem) %>% 
  summarise(
    inc_race = mean(inc_race, na.rm=TRUE),
    fem_inc = mean(fem_inc, na.rm=TRUE),
    avg_vs4_p = mean(avg_vs4_p, na.rm=TRUE),
    vs_1_p = mean(vs_1_p, na.rm=TRUE),
    dem_party = mean(dem_party, na.rm=TRUE),
    prior.pri2 = mean(prior.pri2, na.rm=TRUE),
    ruling = mean(ruling, na.rm=TRUE),
    n = n()
  )

# plot district characteristics

# by number of applicants (continuous)
election %>%
  mutate(dem_party = ifelse(party == "mp", 1, 0)) %>%
  group_by(n.fem) %>%
  summarise(
    inc_race   = mean(inc_race, na.rm = TRUE),
    fem_inc    = mean(fem_inc, na.rm = TRUE),
    avg_vs4_p  = mean(avg_vs4_p, na.rm = TRUE),
    vs_1_p     = mean(vs_1_p, na.rm = TRUE),
    dem_party  = mean(dem_party, na.rm = TRUE),
    prior.pri2 = mean(`prior.pri2`, na.rm = TRUE),
    ruling     = mean(ruling, na.rm = TRUE)
  ) %>%
  pivot_longer(-n.fem, names_to = "variable", values_to = "mean_value") %>%
  mutate(variable = recode(variable,
                           inc_race   = "Incumbent race",
                           fem_inc    = "Female incumbent present",
                           avg_vs4_p  = "Average 4 vote share",
                           vs_1_p     = "Previous vote share",
                           dem_party  = "MP (1) vs PPP (0)",
                           prior.pri2 = "Primary in previous election",
                           ruling     = "Ruling party district"
  )) %>%
  ggplot(aes(x = factor(n.fem), y = mean_value, fill = factor(n.fem))) +
  geom_col(width = 0.7, show.legend = TRUE) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Number of female applicants in district",
    y = "Mean value of district characteristic",
    title = "District characteristics by number of female applicants",
    fill = "n.fem"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

# by number of applicants (binary)
election %>%
  mutate(
    fem_district = ifelse(n.fem > 0, 1, 0),
    dem_party = ifelse(party == "mp", 1, 0)
  ) %>%
  group_by(fem_district) %>%
  summarise(across(
    c(inc_race, fem_inc, 
      #avg_vs4_p,dem_party,
      vs_1_p,
      prior.pri2, ruling),
    \(x) mean(x, na.rm = TRUE)
  )) %>%
  pivot_longer(
    -fem_district,
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  mutate(variable = recode(variable,
                           inc_race   = "Incumbent race",
                           fem_inc    = "Fem. incumbent race",
#                           avg_vs4_p  = "Average 4 vote share",
                           vs_1_p     = "Previous vote share",
#                           dem_party  = "MP (1) vs PPP (0)",
                           prior.pri2 = "Past primary",
                           ruling     = "Ruling party district"
  )) %>%
  mutate(variable = factor(variable,
                      levels = c(
                        "Ruling party district",
                        "Incumbent race",
                        "Fem. incumbent race",
                        "Previous vote share",
                        "Past primary"
                      ))) %>%
  ggplot(aes(x = factor(fem_district), y = mean_value, fill = factor(fem_district))) +
  geom_col(width = 0.7, show.legend = TRUE) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(
    values = c("gray70", "steelblue"),
    labels = c("No female applicant", "≥1 female applicant")
  ) +
  labs(
    x = "District type",
    y = "Mean value",
    fill = "",
#    title = "District characteristics by presence of female applicants"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),  # removes repetitive x labels (0/1)
    axis.ticks.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

ggsave("district_character.png", plot = last_plot(),
       width = 6, height = 4)

##-----------------------------
### Effect of nomination method
##-----------------------------

# DV: fem_winner or win == 1 & female == 1
# IV: pri2
# population of interest --> female running districts


# incumbent applicant (whether they exist, whether it's female)
# type of district

election %>%
  filter(n.fem > 0) %>% group_by(pri2) %>% summarize(fem_winner = mean(fem_winner, na.rm=T))

# determine what to examine: pri v. non among women or women v. men or pri/non among women v. among men
# think about whether representation isn't actually bad -> pct of applicants v. actual representation
# see if i can get demographic info for districts
# see if i can get exact voteshare of female winners
# sample too small? only 340 districts 

# 82 districts with female nominees
election %>% filter(female == 1) %>% count()

# 43 districts with female winners
election %>% filter(female == 1 & win == 1) %>% count()

# pri2 == 1 10/25 = 0.4 districts with primary, female nominee. rate of winning
election %>% filter(female == 1 & pri2 == 1) %>% count()
election %>% filter(female == 1 & win == 1 & pri2 == 1) %>% count()

# pri2 == 0 33/57 = 0.57 districts with direct, female nominee. rate of winning
election %>% filter(female == 1 & pri2 == 0) %>% count()
election %>% filter(female == 1 & win == 1 & pri2 == 0) %>% count()

# men primary 181/405 = 0.44
election %>% filter(female == 0 & pri2 == 1) %>% count()
election %>% filter(female == 0 & win == 1 & pri2 == 1) %>% count()

# men non-primary 248/551 = 0.45
election %>% filter(female == 0 & pri2 == 0) %>% count()
election %>% filter(female == 0 & win == 1 & pri2 == 0) %>% count()

# compare voteshare
elec <- read.csv("elec.csv") %>% filter(congress >= 19) 
election <- election %>% left_join(elec %>% select(congress, province, district, vs_fst, w_margin),
                                   by= c("congress", "province",
                                         "district_data" = "district"))
# vs_fst is voteshare of the winner
# w_margin is winning margin of the district (first - second)
# voteshare of winner: vs_fst
# voteshare of nominee: vs

# margin comparison for primary v. non-primary
election %>% filter(female == 1 & win == 1) %>% group_by(pri2) %>% summarize(avg = mean(w_margin))



########################################
######### MAIN RESULTS
########################################

#####################
## summary stats
#####################

election2 %>%
  select(year, win, pri2, female2, incumbent2, age2, tenure_match2) %>% 
  as.data.frame(.) %>% 
  stargazer(.,
            type = "latex",
            float = FALSE,          # THIS is the correct argument to remove \begin{table}
            summary = TRUE,
            covariate.labels = c("Year", "Win General", "Primary", "Female",  
                                 "Incumbent", "Age", "Tenure"),
            digits = 3,
            summary.stat = c("n", "mean", "sd", "min", "max"),
            out = "/Users/hyoon/Desktop/dissertation/fem_summary.tex")


#####################
## main results
#####################

# library(fixest)
# 
# main <- feols(win ~ pri2 + inc + vs_1_p + tenure_match2 | party^congress,
#   data = election2 %>% filter(!is.na(female2)),
#   cluster = ~district_data)
# 
# main_int <- feols(win ~ pri2 + female2 + female2:pri2 + inc + vs_1_p + tenure_match2 | party^congress, 
#                   data = election2, 
#                   cluster = ~district_data)
# 
# main_fem <- feols(win ~ pri2 + inc + vs_1_p + tenure_match2 | party^congress,
#                   data = election2 %>% filter(female2 == 1),
#                   cluster = ~district_data)
# 
# var_dict = c(
#   "female2" = "Female",
#   "pri2" = "Primary",
#   "female2:pri2" = "Primary $\\times$ Female",
#   "pri2:female2" = "Primary $\\times$ Female", # Catches fixest's internal flip
#   "inc" = "Incumbent",
#   "vs_1_p" = "Prior Vote Share",
#   "tenure_match2" = "Tenure"
# )
# 
# etable(main, main_int, main_fem,
#        tex = TRUE,
#        file = "/Users/hyoon/Desktop/dissertation/main_result.tex",
#        replace = TRUE,
#        depvar = FALSE,
#        headers = c("Main", "Interaction", "Women Only"),
#        dict = var_dict,
#        # THE FIX: The '%' forces etable to use the original variable names for sorting.
#        # This absolutely guarantees: Primary -> Female -> Interaction
#        order = c("%^pri2$", "%^female2$", "%female2:pri2", "%pri2:female2"),
#        fitstat = c("n"),
#        digits = 3
# )
# 
# #####################
# ## main result + district
# #####################
# 
# main2 <- feols(win ~ pri2 + inc + vs_1_p + tenure_match2 | party^congress + district_data,
#               data = election2 %>% filter(!is.na(female2)),
#               cluster = ~district_data)
# 
# main_int2 <- feols(win ~ pri2 + female2 + female2:pri2 + inc + vs_1_p + tenure_match2 | party^congress + district_data, 
#                   data = election2, 
#                   cluster = ~district_data)
# 
# main_fem2 <- feols(win ~ pri2 + inc + vs_1_p + tenure_match2 | party^congress + district_data,
#                   data = election2 %>% filter(female2 == 1),
#                   cluster = ~district_data)
# 
# etable(main2, main_int2, main_fem2,
#        tex = TRUE,
#        file = "/Users/hyoon/Desktop/dissertation/main_result2.tex",
#        replace = TRUE,
#        depvar = FALSE,
#        headers = c("Main", "Interaction", "Women Only"),
#        dict = var_dict,
#        # THE FIX: The '%' forces etable to use the original variable names for sorting.
#        # This absolutely guarantees: Primary -> Female -> Interaction
#        order = c("%^pri2$", "%^female2$", "%female2:pri2", "%pri2:female2"),
#        fitstat = c("n"),
#        digits = 3
# )

#####################
## plot main results
#####################
# 
# library(marginaleffects)
# library(ggplot2)
# library(tidyverse)
# library(fixest)
# 
# # main
# 
# # 1. Define the Analytic Set
# plot_dat <- election2 %>%
#   drop_na(win, female2, pri2, inc, vs_1_p, tenure_match2, party, congress)
# 
# # 2. Run the Model
# main_int <- feols(win ~ pri2 + female2 + female2:pri2 + inc + vs_1_p + tenure_match2 | party^congress,
#                   data = plot_dat,
#                   cluster = ~district_data)
# 
# # 3. Calculate Predicted Probabilities (The Levels)
# preds <- predictions(main_int, 
#                      newdata = datagrid(female2 = c(0, 1), pri2 = c(0, 1))) %>%
#   as_tibble() %>%
#   mutate(
#     plot_type = ifelse(pri2 == 0, "Direct", "Primary"),
#     sex_label = ifelse(female2 == 1, "Female", "Male")
#   ) %>%
#   select(sex_label, plot_type, estimate, conf.low, conf.high)
# 
# # 4. Calculate Marginal Effects (The Difference)
# comps <- comparisons(main_int, 
#                      variables = "pri2", 
#                      by = "female2") %>%
#   as_tibble() %>%
#   mutate(
#     plot_type = "Difference",
#     sex_label = ifelse(female2 == 1, "Female", "Male")
#   ) %>%
#   select(sex_label, plot_type, estimate, conf.low, conf.high)
# 
# # 5. Combine the Data and Set the X-Axis Order
# combined_plot_df <- bind_rows(preds, comps) %>%
#   mutate(
#     # This forces ggplot to strictly follow Direct > Primary > Difference
#     plot_type = factor(plot_type, levels = c("Direct", "Primary", "Difference"))
#   )
# 
# # 6. Generate the Plot with your specific aesthetics
# ggplot(combined_plot_df, aes(x = plot_type, y = estimate, color = sex_label, shape = plot_type)) +
#   # The Zero Line 
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   
#   # Points and Error Bars
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12) +
#   
#   # The Two Panels
#   facet_wrap(~ sex_label) +
#   
#   # Your Original Color Scheme
#   scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) +
#   
#   # Distinct shapes to separate probabilities from the penalty
#   scale_shape_manual(values = c("Direct" = 16, 
#                                 "Primary" = 16, 
#                                 "Difference" = 18)) +
#   
#   # Clean Percentage Y-Axis
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   
#   labs(
#     x = NULL, # Removes the x-axis title since the labels are self-explanatory
#     y = "Marginal Effect"
#   ) +
#   
#   # Your Original Theme Choices
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "none",
#     strip.text = element_text(face = "bold", size = 14),
#     axis.text.x = element_text(hjust = 0.5) # Centered text looks better when not angled
#   )
# 
# ggsave("/Users/hyoon/Desktop/dissertation/fem_int.png", plot = last_plot(),
#        width = 6, height = 4)
# 
# # with district FE
# 
# # 1. Define the Analytic Set
# # (I remember you prefer defining your model sets cleanly and separately first!)
# plot_dat_robust <- election2 %>%
#   drop_na(win, female2, pri2, inc, vs_1_p, tenure_match2, party, congress, district_data)
# 
# # 2. Run the Robustness Model (Adding District FE)
# main_int_robust <- feols(win ~ pri2 + female2 + female2:pri2 + inc + vs_1_p + tenure_match2 | party^congress + district_data,
#                          data = plot_dat_robust,
#                          cluster = ~district_data)
# 
# # 3. Calculate Predicted Probabilities (The Levels)
# preds_robust <- predictions(main_int_robust, 
#                             newdata = datagrid(female2 = c(0, 1), pri2 = c(0, 1))) %>%
#   as_tibble() %>%
#   mutate(
#     plot_type = ifelse(pri2 == 0, "Direct", "Primary"),
#     sex_label = ifelse(female2 == 1, "Female", "Male")
#   ) %>%
#   select(sex_label, plot_type, estimate, conf.low, conf.high)
# 
# # 4. Calculate Marginal Effects (The Difference)
# comps_robust <- comparisons(main_int_robust, 
#                             variables = "pri2", 
#                             by = "female2") %>%
#   as_tibble() %>%
#   mutate(
#     plot_type = "Difference",
#     sex_label = ifelse(female2 == 1, "Female", "Male")
#   ) %>%
#   select(sex_label, plot_type, estimate, conf.low, conf.high)
# 
# # 5. Combine the Data and Set the X-Axis Order
# combined_plot_df_robust <- bind_rows(preds_robust, comps_robust) %>%
#   mutate(
#     # This forces ggplot to strictly follow Direct > Primary > Difference
#     plot_type = factor(plot_type, levels = c("Direct", "Primary", "Difference"))
#   )
# 
# # 6. Generate the Plot with your specific aesthetics
# ggplot(combined_plot_df_robust, aes(x = plot_type, y = estimate, color = sex_label, shape = plot_type)) +
#   # The Zero Line 
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   
#   # Points and Error Bars
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12) +
#   
#   # The Two Panels
#   facet_wrap(~ sex_label) +
#   
#   # Your Original Color Scheme
#   scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) +
#   
#   # Distinct shapes to separate probabilities from the penalty
#   scale_shape_manual(values = c("Direct" = 16, 
#                                 "Primary" = 16, 
#                                 "Difference" = 18)) +
#   
#   # Clean Percentage Y-Axis
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   
#   labs(
#     x = NULL, # Removes the x-axis title since the labels are self-explanatory
#     y = "Marginal Effect"
#   ) +
#   
#   # Your Original Theme Choices
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "none",
#     strip.text = element_text(face = "bold", size = 14),
#     axis.text.x = element_text(hjust = 0.5) # Centered text looks better when not angled
#   )
# 
# ggsave("/Users/hyoon/Desktop/dissertation/fem_int2.png", plot = last_plot(),
#        width = 6, height = 4)



#####################
## new analysis
#####################

# library(fixest)
# library(dplyr)
# library(ggplot2)
# library(marginaleffects)
# 
# # ---------------------------------------------------------
# # 1. H1: The Strategic Shield (Subsample: Direct Nomination Only)
# # ---------------------------------------------------------
# # Testing for gender parity when the party has centralized control.
# # If H1 holds, 'female2' should be non-significant.
# h1_shield <- feols(win ~ female2 + vs_1_p | party_year, 
#                    data = filter(master_analytic_set, pri2 == 0),
#                    cluster = ~district_data)
# 
# # ---------------------------------------------------------
# # 2. H2: The Selection Trap (Main Interaction with Quality Controls)
# # ---------------------------------------------------------
# # Testing if the primary penalty persists even when controlling for 
# # observable candidate quality (Age, Tenure, Incumbency).
# h2_trap <- feols(win ~ female2 * pri2 + vs_1_p + incumbent2 + tenure_match2 + age2 | party_year, 
#                  data = master_analytic_set,
#                  cluster = ~district_data)
# 
# # this is sig
# h2_trap2 <- feols(win ~ female2 * pri2 + incumbent2 + tenure_match2 | party_year, 
#              data = master_analytic_set,
#              cluster = ~district_data)
# 
# # ---------------------------------------------------------
# # 3. ABM Mechanism: Incumbent vs. Newcomer Split
# # ---------------------------------------------------------
# # Testing the 'Uncertainty' logic. The gendered primary penalty 
# # should be concentrated among those without an incumbency 'signal.'
# 
# # Model for Newcomers (Non-Incumbents)
# h3_newcomers <- feols(win ~ female2 * pri2 + vs_1_p | party_year, 
#                       data = filter(master_analytic_set, incumbent2 == 0),
#                       cluster = ~district_data)
# 
# # Model for Incumbents
# h3_incumbents <- feols(win ~ female2 * pri2 + vs_1_p | party_year, 
#                        data = filter(master_analytic_set, incumbent2 == 1),
#                        cluster = ~district_data)
# 
# # ---------------------------------------------------------
# # 4. Consolidate and Generate Tables
# # ---------------------------------------------------------
# model_list <- list(
#   "H1: Direct Only"  = h1_shield,
#   "H2: Full Control" = h2_trap,
#   "H3: Newcomers"    = h3_newcomers,
#   "H3: Incumbents"   = h3_incumbents
# )
# 
# etable(model_list, 
#        headers = names(model_list),
#        dict = c(female2 = "Female", pri2 = "Primary", "female2:pri2" = "Female x Primary"),
#        order = c("Female", "Primary", "Female x Primary"),
#        drop = "vs_1_p")
# 
# # Calculate the 'Effect of Primary' separately for Men and Women
# primary_contrast <- comparisons(
#   h2_trap2, 
#   variables = "pri2", 
#   by = "female2"
# ) %>%
#   as_tibble() %>%
#   mutate(Gender = ifelse(female2 == 1, "Female", "Male"))
# 
# # Plot the contrast
# ggplot(primary_contrast, aes(x = Gender, y = estimate, color = Gender)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1) +
#   scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Electoral Cost of Primary Nomination",
#     subtitle = "Marginal Effect of Primary vs. Direct Nomination",
#     y = "Change in Win Probability",
#     x = ""
#   ) +
#   theme(legend.position = "none", plot.title = element_text(face = "bold"))


# Setup and Libraries

library(fixest)          
library(dplyr)           
library(marginaleffects) 
library(ggplot2)         
library(scales)

# Define the Master Analytic Set
# We filter for major party nominees and ensure quality proxies are non-missing
master_analytic_set <- election2 %>%
  filter(!is.na(nominee)) %>%
  filter(party %in% c("mp", "ppp")) %>%
  filter(!is.na(female2) & !is.na(pri2)) %>%
  # Drop rows missing our key quality proxies
  drop_na(age2, tenure_match2, incumbent2) %>%
  mutate(
    # Create the interacted fixed effect for the regression
    party_year = paste(party, congress, sep = "_")
  )

# Split the analytic set to test the mechanism of Institutional Exposure
data_newcomers <- master_analytic_set %>% filter(incumbent2 == 0)
data_incumbents <- master_analytic_set %>% filter(incumbent2 == 1)

# ---------------------------------------------------------
# 3. Stage 1: The Selection Hurdle (Quality Models)
# ---------------------------------------------------------
# Proving that female primary survivors are at least as qualified as men
# selection_models <- list(
#   "Tenure"     = feols(tenure_match2 ~ female2 * pri2 + vs_1_p | party_year, 
#                        data = master_analytic_set, cluster = ~district_data),
#   "Age"        = feols(age2 ~ female2 * pri2 + vs_1_p | party_year, 
#                        data = master_analytic_set, cluster = ~district_data),
#   "Incumbency" = feols(incumbent2 ~ female2 * pri2 + vs_1_p | party_year, 
#                        data = master_analytic_set, cluster = ~district_data)
# )
# 
# feols(tenure_match2 ~ female2 * pri2 + vs_1_p | party_year, 
#       data = master_analytic_set, cluster = ~district_data)
# feols(incumbent2 ~ female2 * pri2 + vs_1_p | party_year, 
#       data = master_analytic_set, cluster = ~district_data)

# interaction positive, null

# qual_ten <- feols(tenure_match2 ~ female2 * pri2 | party_year, 
#       data = master_analytic_set, cluster = ~district_data)
# qual_age <- feols(age2 ~ female2 * pri2 | party_year, 
#                   data = master_analytic_set, cluster = ~district_data)
# qual_inc <- feols(incumbent2 ~ female2 * pri2 | party_year, 
#       data = master_analytic_set, cluster = ~district_data)


# ---------------------------------------------------------
# 1. Direct Nomination
# ---------------------------------------------------------

# H1: Direct Only (Verification of the Shield)
# better
h1_shield <- feols(win ~ female2 + incumbent2 + tenure_match2 + age2 | party_year, 
      data = filter(master_analytic_set, pri2 == 0),
      cluster = ~district_data)

## linear predicted probability plot

# 1. Run the plotting-safe model
# Swapping the fixed effect for a factor to preserve the intercept variance
h1_plot_model <- feols(win ~ female2 + factor(party_year), 
                       data = filter(master_analytic_set, pri2 == 0),
                       cluster = ~district_data)

# 2. Calculate average predicted probabilities
pred_h1 <- avg_predictions(h1_plot_model, by = "female2") %>%
  as_tibble()

# 3. Generate the Plot with a CI Ribbon (Band)
ggplot(pred_h1, aes(x = female2, y = estimate)) +
  # Add the CI Band (Ribbon)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "steelblue", alpha = 0.2) +
  
  # Add the line connecting the estimates
  geom_line(color = "black", linewidth = 0.5) +
  
  # Add the point estimates on top
#  geom_point(color = "darkblue", size = 4) +
  
  # Format the axes
  scale_x_continuous(breaks = c(0, 1), labels = c("Male", "Female")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  # Clean up labels (no titles/subtitles to leave room for LaTeX captions)
  labs(
    x = "",
    y = "Predicted Probability"
  ) +
  
  # Apply a clean, publication-ready theme
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold", size = 13),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  )

# diff plot (w marginal effect)

# 1. Run the plotting-safe model
h1_plot_model <- feols(win ~ female2 + + incumbent2 + tenure_match2 + age2 + factor(party_year), 
                       data = filter(master_analytic_set, pri2 == 0),
                       cluster = ~district_data)

# 2. Extract Levels (Predicted Probabilities)
pred_levels_h1 <- avg_predictions(h1_plot_model, by = "female2") %>%
  as_tibble() %>%
  mutate(Gender = ifelse(female2 == 1, "Female", "Male"),
         x_cat = Gender) %>%
  select(Gender, x_cat, estimate, conf.low, conf.high)

# 3. Extract the Marginal Effect (The Shield/Difference)
pred_diff_h1 <- avg_comparisons(h1_plot_model, variables = "female2") %>%
  as_tibble() %>%
  mutate(Gender = "Difference",
         x_cat = "Difference") %>%
  select(Gender, x_cat, estimate, conf.low, conf.high)

# 4. Combine and Plot
plot_data_h1 <- bind_rows(pred_levels_h1, pred_diff_h1) %>%
  mutate(x_cat = factor(x_cat, levels = c("Male", "Female", "Difference")))

ggplot(plot_data_h1, aes(x = x_cat, y = estimate, color = x_cat)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred", "Difference" = "grey50")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "", y = "Predicted Probability / Marginal Effect") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("/Users/hyoon/Desktop/dissertation/shield.png", plot = last_plot(),
      width = 6, height = 4)


# table

etable(h1_shield, 
       tex = TRUE,
       file = "/Users/hyoon/Desktop/dissertation/shield.tex",
       replace = TRUE,
       depvar = FALSE,
       dict = c(female2 = "Female", win = "Win Probability",
                incumbent2 = "Incumbent", tenure_match2 = "Tenure",
                age2 = "Age"),
       headers = list("Nomination" = "Direct Only"),
       fitstat = c("n"),
       digits = 3)

# ---------------------------------------------------------
# 2. Quality
# ---------------------------------------------------------

# Tenure Plotting Model
qual_tenure_plot <- feols(tenure_match2 ~ female2 * pri2 + factor(party_year), 
                          data = master_analytic_set, cluster = ~district_data)

# Age Plotting Model
qual_age_plot <- feols(age2 ~ female2 * pri2 + factor(party_year), 
                       data = master_analytic_set, cluster = ~district_data)

# Incumbency Plotting Model
qual_inc_plot <- feols(incumbent2 ~ female2 * pri2 + factor(party_year), 
                       data = master_analytic_set, cluster = ~district_data)

# 1. Calculate Average Predicted Levels for Each Model
# Using avg_predictions() instead of predictions() fixes the missing CI issue
# by calculating the margins over the observed data distribution.

pred_tenure <- avg_predictions(qual_tenure_plot, by = c("female2", "pri2")) %>%
  as_tibble() %>% 
  mutate(Outcome = "Tenure")

pred_age <- avg_predictions(qual_age_plot, by = c("female2", "pri2")) %>%
  as_tibble() %>% 
  mutate(Outcome = "Age")

pred_inc <- avg_predictions(qual_inc_plot, by = c("female2", "pri2")) %>%
  as_tibble() %>% 
  mutate(Outcome = "Incumbency")

# 2. Combine and Clean Up Labels (Your original code remains the same!)
qual_plot_df <- bind_rows(pred_tenure, pred_age, pred_inc) %>%
  mutate(
    plot_type = factor(ifelse(pri2 == 0, "Direct", "Primary"), levels = c("Direct", "Primary")),
    sex_label = ifelse(female2 == 1, "Female", "Male"),
    Outcome = factor(Outcome, levels = c("Incumbency", "Tenure", "Age"))
  )

pd <- position_dodge(width = 0.4)

ggplot(qual_plot_df, aes(x = plot_type, y = estimate, color = sex_label)) +
  # Points and error bars only (geom_line is completely removed)
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, position = pd, linewidth = 0.8) +
  
  # The 3 panels, scaled independently
  facet_wrap(~ Outcome, scales = "free_y") +
  
  # Your established color scheme
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) +
  
  # Adding distinct shapes makes it even easier to read without the lines
  scale_shape_manual(values = c("Male" = 16, "Female" = 17)) + 
  
  labs(
        x = "",
        y = "Predicted Mean",
    color = "Gender"
  ) +
  
  # Clean, minimal theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "gray90", color = NA),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("/Users/hyoon/Desktop/dissertation/quality.png", plot = last_plot(),
       width = 6, height = 4)

# Combine results into a single table

qual_tenure <- feols(tenure_match2 ~ female2 * pri2 | party^year, 
                          data = master_analytic_set, cluster = ~district_data)

# Age Plotting Model
qual_age <- feols(age2 ~ female2 * pri2 | party^year, 
                       data = master_analytic_set, cluster = ~district_data)

# Incumbency Plotting Model
qual_inc <- feols(incumbent2 ~ female2 * pri2 | party^year, 
                       data = master_analytic_set, cluster = ~district_data)

etable(qual_tenure, qual_age, qual_inc, 
       tex = TRUE,
       file = "/Users/hyoon/Desktop/dissertation/quality.tex",
       replace = TRUE,
       depvar = FALSE,
       headers = c("Tenure", "Age", "Incumbency"),
       dict = c(female2 = "Female", pri2 = "Primary"),
       fitstat = c("n"),
       digits = 3)


# just show direct nomination

# Filter to keep only Direct Nomination (pri2 == 0)
direct_only_df <- qual_plot_df %>% 
  filter(pri2 == 0)

# Create the plot
ggplot(direct_only_df, aes(x = sex_label, y = estimate, color = sex_label)) +
  # Points and error bars
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  
  # Panels for each Quality metric
  facet_wrap(~ Outcome, scales = "free_y") +
  
  # Established color scheme
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) +
  
  labs(
#    title = "Candidate Quality under Direct Nomination",
#    subtitle = "Comparing Female and Male Baseline Characteristics",
    x = "",
    y = "Predicted Mean Value",
    color = "Gender"
  ) +
  
  # Clean, minimal theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", # Removed legend since X-axis labels handle it
    strip.text = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "gray95", color = NA),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

# Save the focused plot
ggsave("/Users/hyoon/Desktop/dissertation/quality_direct.png", 
       width = 7, height = 4)


# table

qual_tenure_dir <- feols(tenure_match2 ~ female2 | party^year, 
                         data = master_analytic_set[master_analytic_set$pri2 == 0, ], 
                         cluster = ~district_data)

qual_age_dir <- feols(age2 ~ female2 | party^year, 
                      data = master_analytic_set[master_analytic_set$pri2 == 0, ], 
                      cluster = ~district_data)

qual_inc_dir <- feols(incumbent2 ~ female2 | party^year, 
                      data = master_analytic_set[master_analytic_set$pri2 == 0, ], 
                      cluster = ~district_data)

# The fix: Simplify the etable call to ensure no 'missing' or 'extra' arguments
etable(qual_tenure_dir, qual_age_dir, qual_inc_dir, 
       tex = TRUE,
       file = "/Users/hyoon/Desktop/dissertation/quality_direct.tex",
       replace = TRUE,
       # Explicitly label each model under the "Direct Nomination Only" umbrella
       headers = list("Nomination" = "Direct Only"), 
       dict = c(female2 = "Female", 
                tenure_match2 = "Tenure Match", 
                age2 = "Age", 
                incumbent2 = "Incumbent"),
       fitstat = ~ n,
       digits = 3)

# ---------------------------------------------------------
# 3. Full
# ---------------------------------------------------------

# H2: Full Model with Quality Controls (Persistence of Penalty)
h2_trap <- feols(win ~ female2 * pri2 + incumbent2 + tenure_match2 + age2 | 
                   party_year, data = master_analytic_set, cluster = ~district_data)

# by method
# average marginal effect

# 1. Use your plotting-safe version of h2_trap
# (Ensure fixed effects are treated as + factor(party_year))
h2_plot_model <- feols(win ~ female2 * pri2 + incumbent2 + tenure_match2 + age2 + 
                         factor(party_year), 
                       data = master_analytic_set, 
                       cluster = ~district_data)

# 2. Calculate the Marginal Effect of Gender for each Nomination Type
h2_ame <- avg_comparisons(h2_plot_model, variables = "female2", by = "pri2") %>%
  mutate(Nomination = ifelse(pri2 == 0, "Direct", "Primary"))

# 3. Plot
ggplot(h2_ame, aes(x = Nomination, y = estimate, color = Nomination)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1.2) +
  scale_color_manual(values = c("Direct" = "steelblue", "Primary" = "darkred")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.4, 0.4)) +
  labs(
    x = "",
    y = "Marginal Effect of Being Female"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", axis.text.x = element_text(face = "bold"))

ggsave("/Users/hyoon/Desktop/dissertation/ame.png", plot = last_plot(),
       width = 6, height = 4)


# by gender difference - cross gender

# 1. Run the Plotting Model for H2 (Full Model with covariates)
# Using + factor(party_year) to preserve the intercept variance
h2_plot_model <- feols(win ~ female2 * pri2 + incumbent2 + tenure_match2 + age2 + factor(party_year), 
                       data = master_analytic_set, 
                       cluster = ~district_data)

# 2. Extract Levels and Marginal Effects
# Predicted Levels (Direct & Primary)
pred_levels_h2 <- avg_predictions(h2_plot_model, by = c("female2", "pri2")) %>%
  as_tibble() %>%
  mutate(Gender = ifelse(female2 == 1, "Female", "Male"),
         x_cat = ifelse(pri2 == 0, "Direct", "Primary")) %>%
  select(Gender, x_cat, estimate, conf.low, conf.high)

# Marginal Effects (Difference)
pred_diff_h2 <- avg_comparisons(h2_plot_model, variables = "pri2", by = "female2", comparison = "difference") %>%
  as_tibble() %>%
  mutate(Gender = ifelse(female2 == 1, "Female", "Male"),
         x_cat = "Difference") %>%
  select(Gender, x_cat, estimate, conf.low, conf.high)

# Combine into Master Dataset
data_h2_full <- bind_rows(pred_levels_h2, pred_diff_h2) %>%
  mutate(Gender = factor(Gender, levels = c("Female", "Male")),
         x_cat = factor(x_cat, levels = c("Direct", "Primary", "Difference")))

# 3. Generate the Clean Plot (No Titles/Subtitles)
ggplot(data_h2_full, aes(x = x_cat, y = estimate, color = Gender)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
  facet_wrap(~ Gender) +
  scale_color_manual(values = c("Female" = "darkred", "Male" = "steelblue")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  # Removed title and subtitle arguments
  labs(x = "", y = "Predicted Probability / Marginal Effect") +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 12, color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  )

ggsave("/Users/hyoon/Desktop/dissertation/full.png", plot = last_plot(),
       width = 6, height = 4)

# subsample (primary effect by gender)

# 1. Run Separate Models for Women and Men
# This isolates the 'Primary Effect' for each group
mod_women <- feols(win ~ pri2 + incumbent2 + tenure_match2 + age2 | party_year, 
                   data = filter(master_analytic_set, female2 == 1), 
                   cluster = ~district_data)

mod_men <- feols(win ~ pri2 + incumbent2 + tenure_match2 + age2 | party_year, 
                 data = filter(master_analytic_set, female2 == 0), 
                 cluster = ~district_data)

# 2. Extract the Effect of the Primary for both
eff_women <- avg_comparisons(mod_women, variables = "pri2") %>%
  as_tibble() %>% mutate(Gender = "Women")

eff_men <- avg_comparisons(mod_men, variables = "pri2") %>%
  as_tibble() %>% mutate(Gender = "Men")

# 3. Combine and Visualize
subsample_df <- bind_rows(eff_women, eff_men)

ggplot(subsample_df, aes(x = Gender, y = estimate, color = Gender)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1.2) +
  scale_color_manual(values = c("Women" = "darkred", "Men" = "steelblue")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Candidate Group",
    y = "Effect of Primary Election (vs. Direct)\non General Election Win Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", axis.text.x = element_text(face = "bold"))

# table

h2_trap <- feols(win ~ female2 * pri2 + incumbent2 + tenure_match2 + age2 | party_year, 
                 data = master_analytic_set, 
                 cluster = ~district_data)

list_h2 <- list("Full" = h2_trap)

etable(list_h2,
       tex = TRUE,
       file = "/Users/hyoon/Desktop/dissertation/full.tex",
       replace = TRUE,
       depvar = FALSE,
       headers = c("Interaction"),
       dict = c(female2 = "Female", 
                pri2 = "Primary", 
                "female2:pri2" = "Female $\\times$ Primary",
                incumbent2 = "Incumbent",
                tenure_match2 = "Tenure",
                age2 = "Age",
                party_year = "Party-Year"),
       fitstat = c("n"), 
       digits = 3,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))


# ---------------------------------------------------------
# 4. Incumbency
# ---------------------------------------------------------

# Newcomers (The Universal Hurdle)-primaries are costly in general
h3_newcomers <- feols(win ~ female2 * pri2 | party_year, 
                      data = data_newcomers, cluster = ~district_data)

# Incumbents (The Selection Trap / Shield Stripping)
h4_incumbents <- feols(win ~ female2 * pri2 + tenure_match2 + age2 | party_year, 
                       data = data_incumbents, cluster = ~district_data)


# one plot

# 1. Extract effects for Newcomers (H3)
# We use factor() for the fixed effects to ensure proper CI estimation
mod_h3 <- feols(win ~ female2 * pri2 + factor(party_year), 
                data = data_newcomers, cluster = ~district_data)

ame_h3 <- avg_comparisons(mod_h3, variables = "female2", by = "pri2") %>%
  as_tibble() %>%
  mutate(Group = "Newcomers (H3: Universal Hurdle)")

# 2. Extract effects for Incumbents (H4)
mod_h4 <- feols(win ~ female2 * pri2 + + tenure_match2 + age2 + factor(party_year), 
                data = data_incumbents, cluster = ~district_data)

ame_h4 <- avg_comparisons(mod_h4, variables = "female2", by = "pri2") %>%
  as_tibble() %>%
  mutate(Group = "Incumbents (H4: Selection Trap)")

# 3. Combine and Plot
plot_df <- bind_rows(ame_h3, ame_h4) %>%
  mutate(Nomination = ifelse(pri2 == 0, "Direct (Shield)", "Primary (Trap)"))

ggplot(plot_df, aes(x = Nomination, y = estimate, color = Nomination)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
  
  # Facet by Newcomer vs Incumbent
  facet_wrap(~ Group) +
  
  scale_color_manual(values = c("Direct (Shield)" = "steelblue", "Primary (Trap)" = "darkred")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "", y = "Marginal Effect of Being Female\n(Win Probability relative to Men)") +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = NA),
    axis.text.x = element_text(face = "bold")
  )

# separate plot by incumbency status

h3_plot_model <- feols(win ~ female2 * pri2 + factor(party_year), 
                       data = data_newcomers, cluster = ~district_data)

h4_plot_model <- feols(win ~ female2 * pri2 + tenure_match2 + age2 + factor(party_year), 
                       data = data_incumbents, cluster = ~district_data)

# This function calculates levels and differences for any model
get_plot_data <- function(model) {
  # A. Predicted Levels (Direct & Primary)
  pred_levels <- avg_predictions(model, by = c("female2", "pri2")) %>%
    as_tibble() %>%
    mutate(Gender = ifelse(female2 == 1, "Female", "Male"),
           x_cat = ifelse(pri2 == 0, "Direct", "Primary")) %>%
    select(Gender, x_cat, estimate, conf.low, conf.high)
  
  # B. Marginal Effects (Difference)
  pred_diff <- avg_comparisons(model, variables = "pri2", by = "female2", comparison = "difference") %>%
    as_tibble() %>%
    mutate(Gender = ifelse(female2 == 1, "Female", "Male"),
           x_cat = "Difference") %>%
    select(Gender, x_cat, estimate, conf.low, conf.high)
  
  # C. Combine
  bind_rows(pred_levels, pred_diff) %>%
    mutate(Gender = factor(Gender, levels = c("Female", "Male")),
           x_cat = factor(x_cat, levels = c("Direct", "Primary", "Difference")))
}

# Extract data for H3 and H4
data_h3 <- get_plot_data(h3_plot_model)
data_h4 <- get_plot_data(h4_plot_model)

make_trap_plot <- function(plot_data) {
  ggplot(plot_data, aes(x = x_cat, y = estimate, color = Gender)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
    facet_wrap(~ Gender) +
    scale_color_manual(values = c("Female" = "darkred", "Male" = "steelblue")) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    
    # SOLUTION: Remove title and subtitle arguments here. 
    # Only keep the axis labels.
    labs(x = "", y = "Predicted Probability / Marginal Effect") +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 15),
      axis.text.x = element_text(size = 12, color = "gray30"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray90", fill = NA)
    )
}

# Generate the clean plots:
make_trap_plot(data_h3)

ggsave("/Users/hyoon/Desktop/dissertation/newcomers.png", plot = last_plot(),
       width = 6, height = 4)

make_trap_plot(data_h4)

ggsave("/Users/hyoon/Desktop/dissertation/inc.png", plot = last_plot(),
       width = 6, height = 4)


# table

h3_newcomers <- feols(win ~ female2 * pri2 | party_year, 
                      data = data_newcomers, cluster = ~district_data)

h4_incumbents <- feols(win ~ female2 * pri2 + tenure_match2 + age2 | party_year, 
                       data = data_incumbents, cluster = ~district_data)

list_h3 <- list("Newcomers" = h3_newcomers)
list_h4 <- list("Incumbents" = h4_incumbents)

# Generate Table 1: H3 (Newcomers)
etable(list_h3,
       tex = TRUE,
       file = "/Users/hyoon/Desktop/dissertation/newcomers.tex",
       replace = TRUE,
       depvar = FALSE,
       dict = c(female2 = "Female", 
                pri2 = "Primary", 
                "female2:pri2" = "Female $\\times$ Primary",
                party_year = "Party-Year"),
       fitstat = c("n"),
       digits = 3,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
)
       
# Generate Table 2: H4 (Incumbents)
etable(list_h4,
       tex = TRUE,
       file = "/Users/hyoon/Desktop/dissertation/inc.tex",
       replace = TRUE,
       depvar = FALSE,
       headers = list("Sample" = "Incumbent Only"), 
       dict = c(female2 = "Female", 
                pri2 = "Primary", 
                "female2:pri2" = "Female $\\times$ Primary",
                tenure_match2 = "Tenure",
                age2 = "Age",
                party_year = "Party-Year"),
       fitstat = c("n"),
       digits = 3,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
)



###### compiled table
etable(h1_shield, mod_women, list_h2, list_h4,
       tex=TRUE,
       file = "/Users/hyoon/Desktop/dissertation/combined.tex",
       replace = TRUE,
       depvar = FALSE,
       headers = c("Direct Only", "Women Only", "Interaction", "Incumbents"),
       dict = c(female2 = "Female", 
                pri2 = "Primary", 
                "female2:pri2" = "Female $\\times$ Primary",
                party_year = "Party-Year"),
       fitstat = c("n"),
       digits = 3,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
)


# ---------------------------------------------------------
# 5. Summary Tables
# ---------------------------------------------------------
# Combine outcome models for comparison
outcome_list <- list(
  "H1: Direct Only"  = h1_shield,
  "H2: Full Control" = h2_trap,
  "H3: Newcomers"    = h3_newcomers,
  "H4: Incumbents"   = h4_incumbents
)

etable(outcome_list, 
       headers = names(outcome_list),
       dict = c(female2 = "Female", pri2 = "Primary", 
                "female2:pri2" = "Female x Primary"),
       order = c("Female", "Primary", "Female x Primary"))

# ---------------------------------------------------------
# 6. Visualization: The "Wipeout" of the Incumbency Shield
# ---------------------------------------------------------
# Plotting predicted probabilities to visualize the 19-point drop
pred_inc <- predictions(h4_incumbents, 
                        newdata = datagrid(female2 = c(0, 1), pri2 = c(0, 1))) %>%
  as_tibble() %>%
  mutate(Gender = ifelse(female2 == 1, "Female", "Male"),
         Nomination = ifelse(pri2 == 1, "Primary", "Direct"))

ggplot(pred_inc, aes(x = Nomination, y = estimate, color = Gender, group = Gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) +
  theme_minimal(base_size = 14) +
  labs(title = "The Incumbency Trap",
       subtitle = "Predicted Win Probability for Incumbents",
       y = "Pr(Win)", x = "Nomination Method") +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))


# quality check

library(fixest)
library(dplyr)

# Stage 1: Do women clear a higher bar to get nominated?
# We regress quality metrics on the interaction to see if primary survivors are "better."
selection_quality <- list(
  "Tenure"     = feols(tenure_match2 ~ female2 * pri2  | party_year, 
                       data = master_analytic_set, cluster = ~district_data),
  "Age"        = feols(age2 ~ female2 * pri2  | party_year, 
                       data = master_analytic_set, cluster = ~district_data),
  "Incumbency" = feols(incumbent2 ~ female2 * pri2  | party_year, 
                       data = master_analytic_set, cluster = ~district_data)
)

# Generate the Selection Table
etable(selection_quality, 
       headers = c("Tenure", "Age", "Incumbency"),
       dict = c(female2 = "Female", pri2 = "Primary", "female2:pri2" = "Female x Primary"))



## additional comments


# female2 ~ pri2

feols(female2 ~ pri2 + age2 + tenure_match2 + incumbent2 | party_year, data = master_analytic_set, cluster = ~district_data)
feols(female2 ~ pri2 + age2 + tenure_match2 + incumbent2 | party_year, data = master_analytic_set  %>% filter(party=="mp"), cluster = ~district_data)
feols(female2 ~ pri2 + age2 + tenure_match2 + incumbent2 | party_year, data = master_analytic_set  %>% filter(party=="ppp"), cluster = ~district_data)

feols(tenure_match2 ~ pri2 | party_year, data = master_analytic_set, cluster = ~district_data)
feols(incumbent2 ~ pri2 | party_year, data = master_analytic_set, cluster = ~district_data)
feols(age2 ~ pri2 | party_year, data = master_analytic_set, cluster = ~district_data)


# summary stats by treatment

master_analytic_set %>%
  group_by(pri2) %>%  
  summarise(
    n = n(),
    mean_female = mean(female2, na.rm = TRUE),
    mean_age = mean(age2, na.rm = TRUE),
    mean_tenure_match = mean(tenure_match2, na.rm = TRUE),
    mean_incumbent = mean(incumbent2, na.rm = TRUE)
  )

library(dplyr)
library(tidyr)
library(ggplot2)

# --- Pre-treatment covariates ---
covariates <- c("female2", "age2", "tenure_match2", "incumbent2")

# --- Compute means by treatment ---
means <- master_analytic_set %>%
  group_by(pri2) %>%
  summarise(across(all_of(covariates), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# --- Compute pooled SDs ---
sds <- master_analytic_set %>%
  summarise(across(all_of(covariates), ~ sd(.x, na.rm = TRUE)))

# --- Compute standardized differences ---
std_diff <- means %>%
  pivot_longer(-pri2, names_to = "covariate", values_to = "mean") %>%
  pivot_wider(names_from = pri2, values_from = mean) %>%
  mutate(
    std_diff = case_when(
      covariate == "female2" ~ (`1` - `0`)/sds$female2,
      covariate == "age2" ~ (`1` - `0`)/sds$age2,
      covariate == "tenure_match2" ~ (`1` - `0`)/sds$tenure_match2,
      covariate == "incumbent2" ~ (`1` - `0`)/sds$incumbent2
    )
  )

# --- Covariate balance plot ---
ggplot(std_diff, aes(x = std_diff, y = covariate)) +
  geom_point(size = 3, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dotted", color = "red") +
  labs(
    x = "Standardized Difference (Primary = 1 vs 0)",
    y = "Covariate",
    title = "Covariate Balance by Selection Method (pri2)"
  ) +
  theme_minimal(base_size = 14)


# plotting

library(broom)

m_all <- feols(female2 ~ pri2 + age2 + tenure_match2 + incumbent2 | party_year, data = master_analytic_set, cluster = ~district_data)
m_mp <- feols(female2 ~ pri2 + age2 + tenure_match2 + incumbent2 | party_year, data = master_analytic_set  %>% filter(party=="mp"), cluster = ~district_data)
m_ppp <- feols(female2 ~ pri2 + age2 + tenure_match2 + incumbent2 | party_year, data = master_analytic_set  %>% filter(party=="ppp"), cluster = ~district_data)

results <- bind_rows(
  tidy(m_all) %>% mutate(model = "Pooled"),
  tidy(m_mp) %>% mutate(model = "MP"),
  tidy(m_ppp) %>% mutate(model = "PPP")
) %>%
  filter(term == "pri2")

results$model <- factor(results$model, levels = c("Pooled", "MP", "PPP"))

ggplot(results, aes(x = model, y = estimate, color = model)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                width = .1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(
    "Pooled" = "black",
    "MP" = "#2C7BB6",   # blue
    "PPP" = "#D7191C"   # red
  )) +
  labs(
    x = "",
    y = "",
    title = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("/Users/hyoon/Desktop/dissertation/selection.png", plot = last_plot(),
       width = 6, height = 4)

etable(
  list("Pooled" = m_all,
       "MP" = m_mp,
       "PPP" = m_ppp),
  tex = TRUE,
  file = "/Users/hyoon/Desktop/dissertation/selection.tex",
  replace = TRUE,
  depvar = FALSE,
  headers = list("Sample" = c("Pooled", "MP", "PPP")),
  dict = c(
    pri2 = "Primary",
    age2 = "Age",
    tenure_match2 = "Tenure",
    incumbent2 = "Incumbent",
    female2 = "Female",
    party_year = "Party-Year"
  ),
  fitstat = c("n"),
  digits = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
)
