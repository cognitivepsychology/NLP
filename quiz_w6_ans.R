options(tibble.width = Inf) # 티블의 너비 무한대로 조정하기.
options(scipen = 999) # 지수 사용 금지
options(max.print = 1500) # 최대 데이터 출력 개수 1500개.

## topicmodels 패키지로 토픽 모델링 시행하기 ##

# (1) JSON 파일인 "논문요약_0206_1.json"에 포함된 논문초록을 RcppMeCab의 pos 함수로 토큰화해주세요.

## 1) doc_type:author 열(총 8개 열)과 summary_entire의 original_text 섹션 열로 구성된 티블을 생성해주세요.
## 2) ipc(분야)에 상관없이 문화, 예술, 음악, 미술, 체육, 스포츠 관련 학회 출판 논문의 초록을 추출한 뒤 article.culture 객체에 할당헤주세요.
## 3) 문서 ID와 논문초록을 티블 형식으로 추출해주세요.
## 4) tidytext 패키지를 사용하여 RcppMeCab의 pos 함수로 토큰화한 뒤 고유명사와 일반명사만 추출해주세요.

library(tidyverse)
library(tidytext)

# jsonlite 패키지 불러오기.
library(jsonlite)

# 단일 json 파일 데이터 프레임으로 불러들이기. 
article.json <- fromJSON("texts/quiz/논문요약_0206_1.json", simplifyDataFrame =T)  

# 데이터의 구조 살펴보기.
attributes(article.json)

# data 섹션만 추출하여 티블 형식으로 변환하기.
article.2 <- article.json$data %>%
  as_tibble()
article.2 

# for문을 사용하여 summary_entire의 original_text 섹션만 추출하기.
article.3 <- list()
for(i in 1:nrow(article.2)){
  article.3[[i]] <- map_dfr(article.2$summary_entire[[i]][1], function(x) as_tibble(x))
}

# 리스트 형식으로 추출된 결과물을 티블 형식으로 변환하기.
article.3.1 <- map_dfr(article.3, function(x) as_tibble(x)) %>%
  rename(summary = value) # 열 이름 summary로 바꾸기.
article.3.1

# doc_type:author 열(총 8개 열) 옆에 summary 열 부착한 뒤, 분석에 불필요한 summary_entire와 summary_section 열 제거하기.
article.4 <- bind_cols(article.2, article.3.1) %>%
  select(-summary_entire, -summary_section)
article.4

# ipc(분야)에 상관없이 문화, 예술, 음악, 미술, 체육, 스포츠 관련 학회 출판 논문 초록 추출한 뒤 article.culture 객체에 할당하기.

article.culture <- article.4 %>%
  filter(str_detect(issued_by, "문화|예술|음악|미술|체육|스포츠")) %>%
  select(2, 9) %>% # doc_id와 summary 열 추출하기.
  rename(text = summary) # tm의 DataframeSource는 열 이름이 doc_id와 text여야 하므로 열 이름 바꿔주기.
article.culture

# 형태소 분석 패키지 RcppMeCab 불러오기.
library(RcppMeCab)

# RcppMeCab 패키지의 pos 함수로 논문초록 텍스트 토큰화하기.
article.culture.mecab <- article.culture %>% 
  unnest_tokens(word, text, token = pos, drop = F) %>%
  filter(str_detect(word, "/nng|/nnp"), 
         str_length(word) > 1) %>%
  separate(word, into = c("word", "pos"), sep = "/") %>%
  group_by(doc_id) %>%
  summarise(text = str_flatten(word, " ")) %>%
  ungroup() 
article.culture.mecab


# (2) 토큰화된 논문초록을 tm 패키지를 사용하여 DTM 객체로 만들어주세요.

# tm 말뭉치 생성하기.
library(tm)
abstract_corpus <- VCorpus(DataframeSource(article.culture.mecab))

# 말뭉치를 DocumentTermMatrix 객체로 만들기.
abstract_corpus.dtm <- DocumentTermMatrix(abstract_corpus)
inspect(abstract_corpus.dtm)

# (3) 4개 주제로 이루어진 LDA 모형을 생성해주세요. 이때 seed는 1010으로 고정해주세요.

# 4-주제 LDA 모형 생성하기. 이때 seed는 1010으로 고정하여 재현가능성 확보하기.
abstract_lda <- LDA(abstract_corpus.dtm, k = 4, control = list(seed = 1010)) 

# 오류 발생 시 모든 단어의 빈도가 0인 행을 제거하기.
ui <- unique(abstract_corpus.dtm$i)
abstract_corpus.dtm.1 <- abstract_corpus.dtm[ui,]

# 4-주제 LDA 모형 재생성하기.
abstract_lda <- LDA(abstract_corpus.dtm.1, k = 4, control = list(seed = 1010))
abstract_lda

# (4) 단어-주제 확률 티블 및 시각화를 통해 탐색하기.

## 1) 베타값이 포함된 단어-주제 확률 티블을 생성하여 abstract_beta 객체에 할당해주세요.
## 2) 첫 번째 단어 "가계부"는 주제 1, 2, 3, 4 중 어느 것에 할당될 확률이 가장 높습니까? 
## 3) 주제별 최빈 단어 10개를 티블로 확인해주세요.
## 4) 주제별 최빈 단어 10개를 ggplot2 패키지로 시각화해주세요.
## 5) 주제별 최빈 단어로 비추어보았을 때 주제 1, 2, 3, 4는 각각 무엇과 관련된 것 같습니까? 간단히 해석해주세요.

# 단어-주제 확률(특정 단어가 특정 주제에 할당될 확률) 티블을 생성하여 abstract_beta 객체에 할당하기.
# 첫 번째 단어 "가계부"는 주제 1, 2, 3, 4 중 어느 것에 할당될 확률이 가장 높습니까? => 주제 1.
abstract_beta <- tidy(abstract_lda, matrix = "beta")
abstract_beta

# 주제별 최빈 단어 10개 확인하기: 티블 버전.
abs_top_terms <- abstract_beta %>%
  group_by(topic) %>% # 주제별로 grouping하기.
  slice_max(beta, n = 10) %>% # 상위빈도 10개 단어만 서브세팅.
  ungroup() %>% # grouping 풀기.
  arrange(topic, -beta) # 주제 오름차순, 베타 내림차순으로 배열하기.
abs_top_terms

# 주제별 최빈 단어 10개 확인하기: 시각화 버전.
abs_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% # reorder_within(재배열할 벡터, 재배열의 기준이 될 벡터, 다면출력에 사용될 벡터)
  ggplot(aes(beta, term, fill = factor(topic))) + # topic 값이 숫자로 되어 있으므로 factor 형식으로 바꾸기.
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() # y축을 내림차순으로 재배열하기.

# 주제별 최빈 단어로 비추어보았을 때 주제 1, 2, 3, 4는 각각 무엇과 관련된 것 같습니까? 간단히 해석해주세요.

# (5) 문서-주제 확률 티블을 통해 탐색하기.

## 1) 감마값이 포함된 문서-주제 확률 티블을 생성하여 abstract_gamma 객체에 할당해주세요.
## 2) 문서 A201008208708에 가장 자주 나오는 단어 10개를 내림차순으로 보여주세요.

# 문서-주제 확률(특정 문서가 특정 주제에 할당될 확률) 티블을 생성하여 abstract_gamma 객체에 할당하기.
abstract_gamma <- tidy(abstract_lda, matrix = "gamma")
abstract_gamma

# 문서 A201008208708에 가장 자주 나오는 단어 10개를 내림차순으로 나타내기.
tidy(abstract_corpus.dtm.1) %>%
  filter(document == "A201008208708") %>%
  slice_max(count, n = 10) %>%
  arrange(desc(count))
  
