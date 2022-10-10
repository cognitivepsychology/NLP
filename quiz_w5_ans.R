options(tibble.width = Inf) # 티블의 너비 무한대로 조정하기.
options(scipen = 999) # 지수 사용 금지
options(max.print = 1500) # 최대 데이터 출력 개수 1500개.

## tidy 텍스트로 감성분석 시행하기 ##

library(tidyverse)
library(tidytext)

# (1) KNU 감정사전을 R로 불러와 정규화해주세요. 4주차 수업자료의 sentiment.json.2를 사용하시면 됩니다.

# JSON 파일을 R로 반입하는 패키지 jsonlite 불러들이기.
library(jsonlite)

# 감성사전 JSON 파일 R로 반입하기.
sentiment.json <- fromJSON("texts/sentiment_dic/SentiWord_info.json", simplifyDataFrame = T)

# 극성을 긍정, 부정, 중립 세 범주로 변환하기.
sentiment.json.1 <- sentiment.json %>% 
  mutate(sentiment = case_when(polarity >= 1 ~ "POS",
                               polarity < 0 ~ "NEG",
                               TRUE ~ "NEU")
  ) %>%
  as_tibble()

# 2-그램 감정어 제거하기 및 용언 맨끝의 종결어미 "-다" 제거하기.
sentiment.json.2 <- sentiment.json.1 %>% 
  filter(!str_detect(word, " ")) %>% # 2-그램 감정어 제거하기.
  mutate(word = str_replace_all(word, "다$", "")) # 용언 맨끝의 "-다" 제거하기.

# (2) rvest 패키지를 사용하여 네이버 영화의 최신 개봉작 "늑대사냥"과 "탑건: 매버릭"의 네티즌 & 관람객 평점 각 200페이지씩 스크래핑한 뒤 티블로 변환해주세요. 

# 늑대사냥 첫 페이지: https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=211161&target=after&page=1
# 탑건 첫 페이지: https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=81888&page=1

library(rvest)

# 늑대사냥 스크래핑하기.

wolf.review <- list() # 늑대사냥 리뷰를 append할 빈 리스트 생성하기.
for(page in 1:200){ ## 늑대사냥 리뷰 200페이지 가져오기.
  # 스크래핑할 영화 평점 사이트 주소 가져오기.
  url <- paste('https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=211161&target=after&page=', page, sep='')
  wolf.review[[page]] <- url %>% # 스크래핑할 영화 평점 사이트 주소 가져오기.
    read_html() %>% # read_html 함수를 사용하여 html 페이지를 review 변수에 저장하기.
    html_nodes('.list_netizen') %>% # html_nodes 함수를 사용하여 list_netizen 클래스 저장하기.
    html_nodes('.title') %>% # html_nodes 함수를 사용하여 title 클래스 저장하기.
    html_text2 %>% # html_text2 함수를 사용하여 텍스트 부분만 추출하기.
    as_tibble() %>% # 티블 형식으로 변환하기.
    separate(col = "value", into = c("title", "score", "NA", "review"),  sep = "\n") %>% # "\n" 단위로 열 분리하기.
    mutate(review = str_replace_all(review, "신고$", ""), # 영화평 맨 끝에 붙은 "신고" 없애기.
           score = str_replace_all(score, "별점 - 총 10점 중", ""), # 숫자 별점만 남기기.
           score = as.numeric(score)) %>% # 캐릭터 변수인 별점을 수치 변수로 변환하기.
    select(-`NA`) # 자료가 없는 NA 열 제거하기.
}
wolf.review.df <- map_dfr(wolf.review, as_tibble) # 리스트 형식의 리뷰 데이터를 티블 형식으로 변환하기.

# 탑건 스크래핑하기.

top.review <- list() # 탑건 리뷰를 append할 빈 리스트 생성하기.
for(page in 1:200){ ## 탑건 리뷰 200페이지 가져오기.
  # 스크래핑할 영화 평점 사이트 주소 가져오기.
  url <- paste('https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=81888&page=', page, sep='')
  top.review[[page]] <- url %>% # 스크래핑할 영화 평점 사이트 주소 가져오기.
    read_html() %>% # read_html 함수를 사용하여 html 페이지를 review 변수에 저장하기.
    html_nodes('.list_netizen') %>% # html_nodes 함수를 사용하여 list_netizen 클래스 저장하기.
    html_nodes('.title') %>% # html_nodes 함수를 사용하여 title 클래스 저장하기.
    html_text2 %>% # html_text2 함수를 사용하여 텍스트 부분만 추출하기.
    as_tibble() %>% # 티블 형식으로 변환하기.
    separate(col = "value", into = c("title", "score", "NA", "review"),  sep = "\n") %>% # "\n" 단위로 열 분리하기.
    mutate(review = str_replace_all(review, "신고$", ""), # 영화평 맨 끝에 붙은 "신고" 없애기.
           score = str_replace_all(score, "별점 - 총 10점 중", ""), # 숫자 별점만 남기기.
           score = as.numeric(score)) %>% # 캐릭터 변수인 별점을 수치 변수로 변환하기.
    select(-`NA`) # 자료가 없는 NA 열 제거하기.
}
top.review.df <- map_dfr(top.review, as_tibble) # 리스트 형식의 리뷰 데이터를 티블 형식으로 변환하기.

# (3) 두 영화의 평점 티블을 하나로 합쳐 wolf.top.review라는 이름의 객체에 할당주세요.

wolf.top.review <- bind_rows(wolf.review, top.review.df)

# (4) KNU 감정사전과 inner_join 함수를 사용하여 감성분석을 시행해주세요. 

## 1) 토큰화 방법으로는 RcppMeCab의 pos를 사용해주세요. 
## 2) 음절 길이가 1 이상인 감정어만 추출해주세요. 
## 3) title을 기준으로 grouping을 해주세요.
## 4) 끝으로 감정어 빈도 상위 20개 행만 서브세팅해주세요.

# RcppMeCab의 pos 함수로 토큰화하는 경우

wolf.top.review.1 <- wolf.top.review %>%
  unnest_tokens(word, review, token = RcppMeCab::pos) %>% # 토큰화 빙식으로 RcppMeCab 패키지의 형태소 분석 함수인 pos를 투입.
  separate(word, into = c("word", "pos"), sep = "/") %>% # word 문자열의 "/"을 기준으로 word 열과 pos 열로 나누기.
  inner_join(sentiment.json.2, by = c("word" = "word_root")) %>% # inner_join 함수를 사용하여 rt.whole의 word 열과 감성사전(sentiment.json.2)의 word_root 열을 병합하기. 이때 감정어 사전 수록 단어와 일치하는 단어만 남기고 나머지는 모두 제거.
  group_by(title) %>% # keyword(윤석열, 문재인)별로 grouping하기.
  count(word, polarity , sort = T) %>% # polarity별 word의 빈도 분석 후 내림차순으로 배열.
  filter(str_length(word) > 1) %>% # word의 음절 길이가 1 이상인 것만 추출하기.
  mutate(word = reorder(word, n)) %>% # word 행을 n의 크기에 따라 배열하기.
  slice_head(n = 20) %>% # 상위 20개 행 서브세팅하기.
  ungroup()

# KoNLP의 SimplePos22 함수로 토큰화하는 경우(RcppMeCab의 pos 함수가 제대로 실행되지 않을 때)

wolf.top.review.1 <- wolf.top.review %>%
  unnest_tokens(word, review, token = KoNLP::SimplePos22) %>% # 토큰화 빙식으로 KoNLP 패키지의 형태소 분석 함수인 SimplePos22를 투입.
  separate_rows(word, sep = "\\+") %>% # "+" 기호를 기준으로 행 단위로 분절하기. 
  separate(word, into = c("word", "pos"), sep = "/") %>% # word 문자열의 "/"을 기준으로 word 열과 pos 열로 나누기.
  inner_join(sentiment.json.2, by = c("word" = "word_root")) %>% # inner_join 함수를 사용하여 rt.whole의 word 열과 감성사전(sentiment.json.2)의 word_root 열을 병합하기. 이때 감정어 사전 수록 단어와 일치하는 단어만 남기고 나머지는 모두 제거.
  group_by(title) %>% # keyword(윤석열, 문재인)별로 grouping하기.
  count(word, polarity , sort = T) %>% # polarity별 word의 빈도 분석 후 내림차순으로 배열.
  filter(str_length(word) > 1) %>% # word의 음절 길이가 1 이상인 것만 추출하기.
  mutate(word = reorder(word, n)) %>% # word 행을 n의 크기에 따라 배열하기.
  slice_head(n = 20) %>% # 상위 20개 행 서브세팅하기.
  ungroup()


# (5) ggplot2 패키지로 polarity(-2, -1, 0, 1, 2) 기준의 감성분석 결과를 시각화해주세요. 

## 1) 감정어 빈도(n)가 가장 높은 단어(음절 수 2개 이상) 10개만 서브세팅해주세요.
## 2) reorder 함수를 사용하여 감정어가 빈도(n) 기준으로 나열되도록 해주세요.
## 3) title을 기준으로 다면출력을 해주세요.

wolf.top.review %>%
  unnest_tokens(word, review, token = RcppMeCab::pos) %>% # 토큰화 빙식으로 RcppMeCab 패키지의 형태소 분석 함수인 pos를 투입.
  separate(word, into = c("word", "pos"), sep = "/") %>% # word 문자열의 "/"을 기준으로 word 열과 pos 열로 나누기.
  inner_join(sentiment.json.2, by = c("word" = "word_root")) %>% # inner_join 함수를 사용하여 rt.whole의 word 열과 감성사전(sentiment.json.2)의 word_root 열을 병합하기. 이때 감정어 사전 수록 단어와 일치하는 단어만 남기고 나머지는 모두 제거.
  group_by(title) %>% # title별로 grouping하기.
  count(word, polarity , sort = T) %>%
  filter(str_length(word) > 1) %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ungroup() %>%
  ggplot(aes(n, word, fill = polarity)) + # polarity(극성)별로 색상을 달리 칠하기.
  geom_col() + # 바의 높이로 관찰값의 크기를 나타내는 함수. 
  scale_fill_hue(h = c(0, 100)) + # 칠하는 색상의 유형을 지정하기.
  facet_wrap(~ title, scales = "free") # title(늑대사냥, 탑건: 매버릭)에 따라 도표를 다면 출력하기.

# (6) ggplot2 패키지로 sentiment(POS, NEG) 기준의 감성분석 결과를 시각화해주세요. 

## 1) 감정어 빈도(n)가 가장 높은 단어(음절 수 2개 이상) 10개만 서브세팅해주세요.
## 2) reorder 함수를 사용하여 감정어가 빈도(n) 기준으로 나열되도록 해주세요.
## 3) title과 sentiment를 기준으로 다면출력을 해주세요.
## 4) 주의: 중립 단어는 분석에 포함되지 않습니다.

wolf.top.review %>%
  unnest_tokens(word, review, token = RcppMeCab::pos) %>% # 토큰화 빙식으로 RcppMeCab 패키지의 형태소 분석 함수인 pos를 투입.
  separate(word, into = c("word", "pos"), sep = "/") %>% # word 문자열의 "/"을 기준으로 word 열과 pos 열로 나누기.
  inner_join(sentiment.json.2, by = c("word" = "word_root")) %>% # inner_join 함수를 사용하여 rt.whole의 word 열과 감성사전(sentiment.json.2)의 word_root 열을 병합하기. 이때 감정어 사전 수록 단어와 일치하는 단어만 남기고 나머지는 모두 제거.
  group_by(title) %>% # title별로 grouping하기.
  count(word, sentiment , sort = T) %>%
  filter(str_length(word) > 1) %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  filter(sentiment != "NEU") %>% # 중립 단어 제거하기.
  ungroup() %>%
  ggplot(aes(n, word, fill = sentiment)) + # sentiment별로 색상을 달리 칠하기.
  geom_col() + # 바의 높이로 관찰값의 크기를 나타내는 함수. 
  facet_wrap(~ title + sentiment, scales = "free_y") + # title과 sentiment별로 다면 출력하기.
  labs(x = "빈도", # x축 레이블 추가하기.
       y = "감정어") # y축 레이블 추가하기.

# (7) 감성분석 결과로 미루어보았을 때 두 영화에 대한 관람객의 평가는 어떤 양상을 나타냅니까? 간단하게 설명해주세요.

# 늑대사냥에 대한 감상평은 부정적 감성 단어 출현 비율이 긍정적 감성 단어에 비해 상당히 높은 반면, 탑건에 대한 감상평은 긍정적 감성 단어 출현 비율이 부정적 감성 단어보다 압도적으로 높다.