options(tibble.width = Inf) # 티블의 너비 무한대로 조정하기.
options(scipen = 999) # 지수 사용 금지
options(max.print = 1500) # 최대 데이터 출력 개수 1500개.

## 1. 웹 스크래핑 실습 1: 네이버 평점 리뷰(https://movie.naver.com/movie/board/review/list.naver)

# (1) rvest 패키지를 사용하여 네이버 리뷰 사이트 총 200페이지에서 영화(movie), 리뷰 제목(title), 작성자명(author), 작성일(date)을 추출해주세요.

# (2) 추출된 데이터를 티블 형식(객체명 naver_review)으로 저장하세요. 열 수는 4개, 행 수는 3000개가 되어야 합니다.# (2) 힌트: 영화, 리뷰 제목 그리고 작성자 & 작성일을 추출하기 위해 총 세 번의 for문을 개별 실행해야 합니다. 

url <- 'https://movie.naver.com/movie/board/review/list.naver?&page=1'

title <- url %>% # 스크래핑할 영화 리뷰 사이트 주소 가져오기.
  read_html() %>% # read_html 함수를 사용하여 html 페이지 저장하기.
  html_nodes('.list_table_1') %>% # html_nodes 함수를 사용하여 .list_table_1 클래스 저장하기.
  html_nodes('.title') %>% # html_nodes 함수를 사용하여 title 클래스 저장하기.
  html_text2 %>% # html_text2 함수를 사용하여 텍스트 부분만 추출하기.
  as_tibble() %>%
  mutate(value = str_replace_all(value, "\r \r \r \r \r \r \r \r // 운영자 추천 아이콘 \r|\r \r 운영자 추천 아이콘 |\r \r ", "")) %>% # 구분기호 없애기.
  rename(title = value) # 열 이름 바꾸기.

movie <- url %>% # 스크래핑할 영화 평점 사이트 주소 가져오기.
  read_html() %>% # read_html 함수를 사용하여 html 페이지 저장하기.
  html_nodes('.list_table_1') %>% # html_nodes 함수를 사용하여 .list_table_1 클래스 저장하기.
  html_nodes('.movie') %>% # html_nodes 함수를 사용하여 movie 클래스 저장하기.
  html_text2 %>%
  as_tibble()

author_date <- url %>% # 스크래핑할 영화 평점 사이트 주소 가져오기.
  read_html() %>% # read_html 함수를 사용하여 html 페이지 저장하기.
  html_nodes('.list_table_1') %>% # html_nodes 함수를 사용하여 .list_table_1 클래스 저장하기.
  html_nodes('.author') %>% # html_nodes 함수를 사용하여 author 클래스 저장하기.
  html_text2 %>%
  as_tibble() %>%
  mutate(value = str_replace_all(value, "\r \r \r\n\r \r \r|\r \r \r \r \r \r \r \r", "")) %>% # 구분기호 없애기.
  separate(col = "value", into = c("author", "date"),  sep = " ")

naver_review.1 <- bind_cols(title, movie, author_date)

naver_review.title <- list() # 네이버 리뷰 제목을 append할 빈 리스트 생성하기.
for(page in 1:200){ ## 네이버 리뷰 100페이지 가져오기.
  # 스크래핑할 영화 리뷰 사이트 주소 가져오기.
  url <- paste('https://movie.naver.com/movie/board/review/list.naver?&page=', page, sep='')
  naver_review.title[[page]] <- url %>% # 스크래핑할 영화 리뷰 사이트 주소 가져오기.
    read_html() %>% # read_html 함수를 사용하여 html 페이지 저장하기.
    html_nodes('.list_table_1') %>% # html_nodes 함수를 사용하여 .list_table_1 클래스 저장하기.
    html_nodes('.title') %>% # html_nodes 함수를 사용하여 title 클래스 저장하기.
    html_text2 %>% # html_text2 함수를 사용하여 텍스트 부분만 추출하기.
    as_tibble() %>%
    mutate(value = str_replace_all(value, "\r \r \r \r \r \r \r \r // 운영자 추천 아이콘 \r|\r \r 운영자 추천 아이콘 |\r \r ", "")) %>% # 구분기호 없애기.
    rename(title = value) # 열 이름 바꾸기.
}
naver_review.title.df <- map_dfr(naver_review.title, as_tibble) # 리스트 형식의 리뷰 데이터를 티블 형식으로 변환하기.

naver_review.movie <- list() # 네이버 리뷰 영화명을 append할 빈 리스트 생성하기.
for(page in 1:200){ ## 네이버 리뷰 100페이지 가져오기.
  # 스크래핑할 영화 리뷰 사이트 주소 가져오기.
  url <- paste('https://movie.naver.com/movie/board/review/list.naver?&page=', page, sep='')
  naver_review.movie[[page]] <- url %>% # 스크래핑할 영화 평점 사이트 주소 가져오기.
    read_html() %>% # read_html 함수를 사용하여 html 페이지 저장하기.
    html_nodes('.list_table_1') %>% # html_nodes 함수를 사용하여 .list_table_1 클래스 저장하기.
    html_nodes('.movie') %>% # html_nodes 함수를 사용하여 movie 클래스 저장하기.
    html_text2 %>%
    as_tibble() %>%
    rename(movie = value) 
}
naver_review.movie.df <- map_dfr(naver_review.movie, as_tibble) # 리스트 형식의 리뷰 데이터를 티블 형식으로 변환하기.

naver_review.author_date <- list() # 네이버 리뷰 작성자 및 작성일을 append할 빈 리스트 생성하기.
for(page in 1:200){ ## 네이버 리뷰 100페이지 가져오기.
  # 스크래핑할 영화 리뷰 사이트 주소 가져오기.
  url <- paste('https://movie.naver.com/movie/board/review/list.naver?&page=', page, sep='')
  naver_review.author_date[[page]] <- url %>% # 스크래핑할 영화 평점 사이트 주소 가져오기.
    read_html() %>% # read_html 함수를 사용하여 html 페이지 저장하기.
    html_nodes('.list_table_1') %>% # html_nodes 함수를 사용하여 .list_table_1 클래스 저장하기.
    html_nodes('.author') %>% # html_nodes 함수를 사용하여 author 클래스 저장하기.
    html_text2 %>%
    as_tibble() %>%
    mutate(value = str_replace_all(value, "\r \r \r\n\r \r \r|\r \r \r \r \r \r \r \r", "")) %>% # 구분기호 없애기.
    separate(col = "value", into = c("author", "date"),  sep = " ")
}
naver_review.author_date.df <- map_dfr(naver_review.author_date, as_tibble) # 리스트 형식의 리뷰 데이터를 티블 형식으로 변환하기.

naver_review <- bind_cols(naver_review.title.df, naver_review.movie.df, naver_review.author_date.df) # 4개의 열 합쳐서 한 개의 티블로 만들기.


## 2. rtweet 패키지를 사용하여 "여왕"과 "장례식" 키워드가 포함된 트윗 1000개를 수집해주세요. 이때 리트윗은 제외해주세요.

# 한글 문자열을 utf8로 인코딩해주는 패키지 불러오기.
library(base64enc)

# rtweet 패키지 불러오기.
library(rtweet)

# "여왕", "장례식" 키워트 최신 트윗 1000건 수집하기.
keyword_ko <- enc2utf8("여왕 장례식") # 띄어쓰기는 "AND"로 인식됨.
rt <-  search_tweets(keyword_ko, n = 1000, include_rts = FALSE, 
                     retryonratelimit = FALSE) # 리트윗은 제외. 
View(rt)


## 3. 공공 데이터 포털에서 오픈 API로 JSON 파일을 내려받아 티블 형식으로 저장하세요.

# (1) 공공 데이터 포털에서 한국건강가정진흥원이 제공하는 한국어교육 운영기관 현황정보를 검색하세요. 링크(https://www.data.go.kr/tcs/dss/selectFileDataDetailView.do?publicDataPk=15025593)를 통해 접속하셔도 됩니다.
# (2) 오픈 API 활용신청을 하여 인증키를 받은 뒤 "한국건강가정진흥원 한국어교육 운영기관 현황" JSON 데이터셋을 내려받으세요. 이때 페이지당 데이터의 수는 1000으로 설정해주세요(참고로, 실제 전체 데이터의 수는 1000개가 채 되지 않습니다).
# (3) jsonlite 패키지를 사용하여 JSON 데이터셋을 R로 불러들인 뒤 json이라는 객체명에 할당해주세요.
# (4) attributes 함수를 사용하여 json 객체의 구조를 확인해주세요.
# (5) json 객체에서 필요한 데이터만 추출하여 티블 형식으로 변환한 뒤 json.data라는 객체명으로 저장해주세요.

# jsonlite 패키지 불러오기.
library(jsonlite)

# json 파일을 데이터 프레임으로 불러들이기. 
json <- fromJSON("response_1663680434389.json", simplifyDataFrame =T)  
attributes(json.data) # 데이터의 구조 살펴보기.
json.data <- json$data %>% # 티블로 변환하기.
  as_tibble()
