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
                            congress == 21 & party == "미래통합당" ~ 1,
                            congress == 20 & party == "새누리당" ~ 1,
                            congress == 19 & party == "새누리당" ~ 1,
                            congress == 18 & party == "한나라당" ~ 1,
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
                            congress == 18 & party == "통합민주당" ~ 0,
                            congress == 17 & party == "열린우리당" ~ 0,
                            congress == 16 & party == "새천년민주당" ~ 0,
                            congress == 15 & party == "새정치국민회의" ~ 0,
                            congress == 14 & party == "민주당" ~ 0,
                            congress == 13 & party == "평화민주당" ~ 0,
                            congress == 12 & party == "신한민주당" ~ 0,
                            congress == 11 & party == "민주한국당" ~ 0,
                            TRUE ~ NA),
         party_cons = as.factor(party_cons))

##------------------
### descriptive
##------------------

## FEMALE

# percentage of women legislators across years by district/pr

fem <- leg %>%
  filter(!is.na(party_cons)) %>%
  group_by(congress, pr, party_cons) %>%
  summarize(female.pct = sum(female == 1, na.rm = TRUE) / n() * 100, .groups = "drop") %>%
  mutate(pr = as.character(pr))

fem_total <- leg %>%
  filter(!is.na(party_cons)) %>%
  group_by(congress, party_cons) %>%
  summarize(pr = "pooled", female.pct = sum(female == 1, na.rm = TRUE) / n() * 100, .groups = "drop")

fem_all <- bind_rows(fem, fem_total)

ggplot(fem_all, aes(x = congress, y = female.pct, color = pr, group = pr)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "red", size = 0.3) + 
  scale_x_continuous(breaks = seq(min(fem_all$congress), max(fem_all$congress), by = 1)) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "darkorange", "pooled" = "gray30"),
    labels = c("0" = "Non-PR", "1" = "PR", "pooled" = "Pooled")
  ) +
  labs(
    x = "Congress",
    y = "Percentage Female",
    color = "PR Status",
    title = "Percentage of Female Legislators by PR Status and Party"
  ) +
  facet_wrap(~ party_cons, labeller = as_labeller(c("0" = "Liberal", "1" = "Conservative"))) +
  theme_minimal()


# perc of female by party
fem_party <- leg %>%
  group_by(congress, party_cons) %>%
  summarize(female.pct = mean(female == 1, na.rm = TRUE) * 100, .groups = "drop")

ggplot(fem_party %>% filter(!is.na(party_cons)), aes(x = congress, y = female.pct, group = party_cons, color = party_cons)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Congress",
    y = "Percentage of Women Legislators",
    title = "Women Legislators by Party and Congress"
  ) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "darkred"),
    labels = c("0" = "Lib", "1" = "Cons")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))

## AGE

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

##------------------
### applicant data
##------------------

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
                              year == 2020 ~ 21)) 

# leg: legislator data 1981-2024 (2024 doesn't have age data)
# app: applicant data 2012-2020 

# applicant in district level

app_d <- app %>% group_by(year, congress, province, district, party) %>% summarize(n = n(), 
                                                         female_pct = sum(female)/n(), 
                                                         female_count = sum(female), 
                                                         young_pct = sum(age <= 40)/n(), 
                                                         young_count = sum(age<=40))

# age distribution
ggplot(app, aes(x = age)) +
  geom_histogram() + facet_grid(year ~ party)

app %>% group_by(party, year) %>% summarize(young.num = sum(age <= 40, na.rm=T),
                                            young.pct = sum(age <= 40, na.rm=T)/n()*100,
                                            female.num = sum(female == 1, na.rm=T),
                                            female.pct = sum(female == 1, na.rm=T)/n()*100)

leg %>% group_by(year) %>% summarize(young.num = sum(age <= 40, na.rm=T),
                                            young.pct = sum(age <= 40, na.rm=T)/n()*100,
                                            female.num = sum(female == 1, na.rm=T),
                                            female.pct = sum(female == 1, na.rm=T)/n()*100) %>%
  ggplot(., aes(x=year, y=female.pct)) + geom_point() + geom_line()

leg %>% group_by(year, pr) %>% summarize(female.pct = sum(female==1, na.rm=T)/n()*100) %>% 
  ggplot(., aes(x=year, y=female.pct, color=as.factor(pr), group=as.factor(pr))) + geom_point() + geom_line()


leg %>% group_by(year, pr) %>% summarize(young.pct = mean(age)) %>% 
  ggplot(., aes(x=year, y=young.pct, color=as.factor(pr), group=as.factor(pr))) + geom_point() + geom_line()


##------------------
### election data
##------------------

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

##------------------
### match districts
##------------------

data_match <- data %>% distinct(congress, province, district)
app_match <- app_d %>% distinct(congress, province, district)

#library(writexl)
#write_xlsx(app_match, "app_match.xlsx")
#anti_join(app_match, data_match, by = c("congress", "province", "district")) %>% print(n=Inf)
#manually matched districts for data_match, app_match as district_data_match

#attempt to fuzzy match the district using linktransformer in python
#data_fuz <- read.csv("df_lm_matched.csv")

app_d <- read_excel("/Users/hyoon/Desktop/Yoon2/korea data/rep/app_match.xlsx") %>% 
  mutate(district_data_match = ifelse(is.na(district_data_match), district, district_data_match)) %>% 
  right_join(app_d, by=c("year", "congress", "province", "district"))

merge <- left_join(data, app_d,
          by = c("congress", "province", "district"="district_data_match", "party")) %>%
  mutate(win = ifelse(is.na(w_margin_1), 0, 1))



