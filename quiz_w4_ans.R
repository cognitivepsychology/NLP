options(tibble.width = Inf) # 티블의 너비 무한대로 조정하기.
options(scipen = 999) # 지수 사용 금지
options(max.print = 1500) # 최대 데이터 출력 개수 1500개.

## tidy 텍스트 다루기 실습 ##

# (1) AI 허브에서 한국어 텍스트 분야 중 논문자료 요약 데이터를 내려받으세요. 링크(https://aihub.or.kr/aihubdata/data/view.do?currMenu=115&topMenu=100&aihubDataSe=realm&dataSetSn=90)를 통해 접속할 수 있습니다.
# (2) jsonlite 패키지와 tidyverse의 map 함수를 사용하여 모든 JSON 데이터셋 중 Training 폴더 내 training_논문 하위폴더 중 "논문요약_0206_0.json" 파일을 R로 불러들인 뒤 article.json이라는 객체명에 할당해주세요.
# (3) attributes 함수를 사용하여 article.json 객체의 구조를 확인해주세요.
# (4) article.json에서 필요한 데이터는 data 섹션에 들어 있습니다. 그중에서도 행이 리스트 형식으로 구성된 summary_entire의 orginal_text 섹션(논문초록 전문)을 목표 텍스트로 사용하고자 합니다. for문을 사용하여 해당 섹션을 추출한 뒤, doc_type:author 열(총 8개 열) 옆에 summary라는 이름의 열로 덧붙여주세요.
# (5) 주제가 사회과학이면서 "교육" 분야 학회에서 출판된 논문 1000편, 주제가 사회과학이면서 "경영, 무역, 마케팅, 경제. 회계, 기업, 금융, 생산, 산업" 분야 학회에서 출판된 논문 1000편, 주제가 인문학이면서 "문학, 드라마, 소설" 분야 학회에서 출판된 논문 전편을 추출해주세요.
# (6) 상기 세 개의 티블에 "field"라는 열을 추가하여 각각 "교육", "경제", "문학"이라는 값을 넣어주세요.
# (6) 상기 세 개의 티블에 대해 RcppMecab 패키지를 사용하여 형태소 단위로 토큰화 & 정규화한 뒤 하나의 티블로 합쳐주세요.
# (7) 4주차 수업자료를 참고하여 교육과 경제 분야, 교육과 문학 분야 간 형태소 출현비율을 비교해주세요. 교육 분야는 경제 및 문학 분야와 비교하여 어떤 형태소의 출현비율이 상대적으로 높습니까?
# (8) 4주차 수업자료를 참고하여 분야 간 형태소 출현비율 상관계수를 계산해주세요. 교육과 어느 분야 간의 상관이 상대적으로 더 높습니까?
# (9) 4주차 수업자료를 참고하여 분야 간 형태소 출현비율 비교 결과를 시각화해주세요. 그리고 시각화 결과를 간단하게 해석해주세요.

# jsonlite 패키지 불러오기.
library(jsonlite)

# 단일 json 파일 데이터 프레임으로 불러들이기. 

article.json <- fromJSON("E:/연구용 텍스트 자료/AI 허브/논문자료 요약/Training/training_논문/논문요약_0206_0.json", simplifyDataFrame =T)  

# 데이터의 구조 살펴보기.
attributes(article.json)

# data 섹션만 추출하여 티블 형식으로 변환하기.

article.2 <- article.1$data %>%
  as_tibble()

# for문을 사용하여 summary_entire의 original_text 섹션만 추출하기.

article.3 <- list()
for(i in 1:nrow(article.2)){
  article.3[[i]] <- map_dfr(article.2$summary_entire[[i]][1], function(x) as_tibble(x))
}

# 리스트 형식으로 추출된 결과물을 티블 형식으로 변환하기.

article.3.1 <- map_dfr(article.3, function(x) as_tibble(x)) %>%
  rename(summary = value) # 열 이름 summary로 바꾸기.

# doc_type:author 열(총 8개 열) 옆에 summary 열 부착한 뒤, 분석에 불필요한 summary_entire와 summary_section 열 제거하기.

article.4 <- bind_cols(article.2, article.3.1) %>%
  select(-summary_entire, -summary_section)

# 사회과학-교육, 사회과학-경제, 인문학-문학 관련 학회 출판 논문 초록 추출하기.

article.edu <- article.4 %>%
  filter(ipc == "사회과학" & str_detect(issued_by, "교육")) %>%
  .[1:1000,] %>%
  mutate(field = "교육")

article.biz <- article.4 %>%
  filter(ipc == "사회과학" & str_detect(issued_by, "경영|무역|마케팅|경제|회계|기업|금융|생산|산업")) %>%
  .[1:1000,] %>%
  mutate(field = "경제") 

article.human <- article.4 %>%
  filter(ipc == "인문학" & str_detect(issued_by, "문학|드라마|소설")) %>%
  mutate(field = "문학")

# RcppMeCab 패키지를 사용한 형태소 분석을 통해 정규화 수행하기 및 불용어 목록 제거하기.

load("stop_words_korean.rda") # 한국어 불용어 목록 불러오기.
library(RcppMeCab)

# 교육 관련 논문 초록 형태소 단위로 정규화하기.

article.edu.tagged <- article.edu %>%
  unnest_tokens(word, summary, token = RcppMeCab::pos) %>% # 토큰화 빙식으로 RcppMeCab 패키지의 형태소 분석 함수인 pos를 투입.
  separate(word, into = c("word", "pos"), sep = "/") %>% # "/"를 기준으로 word 열과 pos 열로 나누기.
  filter(!str_detect(pos, "j[a-z]{1,3}|e[a-z]{1,3}|s[a-z]{1,3}")) %>% # 조사, 어미, 기호 제외.
  unnest_tokens(word, word, token = "words", strip_punct = T) %>% # 형태소 단위로 토큰화.
  anti_join(stop_words_korean) # 불용어 목록 단어 제거하기.
  
# 경제 관련 논문 초록 형태소 단위로 정규화하기.

article.biz.tagged <- article.biz %>%
  unnest_tokens(word, summary, token = RcppMeCab::pos) %>% # 토큰화 빙식으로 RcppMeCab 패키지의 형태소 분석 함수인 pos를 투입.
  separate(word, into = c("word", "pos"), sep = "/") %>% # "/"를 기준으로 word 열과 pos 열로 나누기.
  filter(!str_detect(pos, "j[a-z]{1,3}|e[a-z]{1,3}|s[a-z]{1,3}")) %>% # 조사, 어미, 기호 제외.
  unnest_tokens(word, word, token = "words", strip_punct = T) %>% # 형태소 단위로 토큰화.
  anti_join(stop_words_korean)

# 문학 관련 논문 초록 형태소 단위로 정규화하기.

article.human.tagged <- article.human %>%
  unnest_tokens(word, summary, token = RcppMeCab::pos) %>% # 토큰화 빙식으로 RcppMeCab 패키지의 형태소 분석 함수인 pos를 투입.
  separate(word, into = c("word", "pos"), sep = "/") %>% # "/"를 기준으로 word 열과 pos 열로 나누기.
  filter(!str_detect(pos, "j[a-z]{1,3}|e[a-z]{1,3}|s[a-z]{1,3}")) %>% # 조사, 어미, 기호 제외.
  unnest_tokens(word, word, token = "words", strip_punct = T) %>% # 형태소 단위로 토큰화.
  anti_join(stop_words_korean)

# 세 가지 논문초록 하나의 티블로 합치기.

article.tidy <- bind_rows(article.edu.tagged,
                          article.biz.tagged,
                          article.human.tagged)

# 교육 관련 논문과 경제/문학 간 형태소 출현비율 비교하기.
  
article.freq <- article.tidy %>%
  count(field, word) %>% # 분야별 word(형태소) 빈도 구하기.
  group_by(field) %>% # 분야별로 grouping하기.
  mutate(proportion = n / sum(n)) %>%  # 형태소 출현 비율 구하기.
  select(-n) %>% # n 열 삭제하기.
  pivot_wider(names_from = field, values_from = proportion) %>% # 각 분야가 하나의 열이 되고 비율이 값이 되도록 데이터프레임 변형하기
  pivot_longer(c(`경제`, `문학`), # 교육 vs. 경제 & 문학을 비교하기 위해 교육 열을 따로 두기.
               names_to = "field", values_to = "proportion") 
article.freq

# 고빈도 형태소 출현비율 비교하기.
article.freq %>%
  arrange(desc(`교육`)) 

# 분야 간 형태소 출현비율 상관 계산하기.

cor.test(data = article.freq[article.freq$field == "경제",], # 교육 vs. 경제 비교
         ~ proportion + `교육`)

cor.test(data = article.freq[article.freq$field == "문학",], # 교육 vs. 문학 비교
         ~ proportion + `교육`)

# 분석결과 시각화하기.

library(scales)
ggplot(article.freq, aes(x = proportion, y = `교육`, 
                       color = abs(`교육` - proportion))) + 
  geom_abline(color = "gray40", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + 
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") + 
  facet_wrap(~ field, ncol = 2) +
  labs(y = "교육", x = NULL) 

