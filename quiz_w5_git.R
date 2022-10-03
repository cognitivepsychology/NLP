options(tibble.width = Inf) # 티블의 너비 무한대로 조정하기.
options(scipen = 999) # 지수 사용 금지
options(max.print = 1500) # 최대 데이터 출력 개수 1500개.

## tidy 텍스트로 감성분석 시행하기 ##

# (1) KNU 감정사전을 R로 불러와 정규화해주세요. 4주차 수업자료의 sentiment.json.2를 사용하시면 됩니다.

# (2) rvest 패키지를 사용하여 네이버 영화의 최신 개봉작 "늑대사냥"과 "탑건: 매버릭"의 네티즌 & 관람객 평점 각 200페이지씩 스크래핑한 뒤 티블로 변환해주세요. 

## 1) 늑대사냥 첫 페이지: https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=211161&target=after&page=1
## 2) 탑건 첫 페이지: https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=81888&page=1

# (3) 두 영화의 평점 티블을 하나로 합쳐 wolf.top.review라는 이름의 객체에 할당주세요.

# (4) KNU 감정사전과 inner_join 함수를 사용하여 감성분석을 시행해주세요. 

## 1) 토큰화 방법으로는 RcppMeCab의 pos를 사용해주세요. 
## 2) 음절 길이가 1 이상인 감정어만 추출해주세요. 
## 3) title을 기준으로 grouping을 해주세요.
## 4) 끝으로 감정어 빈도 상위 20개 행만 서브세팅해주세요.

# (5) ggplot2 패키지로 polarity(-2, -1, 0, 1, 2) 기준의 감성분석 결과를 시각화해주세요. 

## 1) 감정어 빈도(n)가 가장 높은 단어(음절 수 2개 이상) 10개만 서브세팅해주세요.
## 2) reorder 함수를 사용하여 감정어가 빈도(n) 기준으로 나열되도록 해주세요.
## 3) title을 기준으로 다면출력을 해주세요.

# (6) ggplot2 패키지로 sentiment(POS, NEG) 기준의 감성분석 결과를 시각화해주세요. 

## 1) 감정어 빈도(n)가 가장 높은 단어(음절 수 2개 이상) 10개만 서브세팅해주세요.
## 2) reorder 함수를 사용하여 감정어가 빈도(n) 기준으로 나열되도록 해주세요.
## 3) title과 sentiment를 기준으로 다면출력을 해주세요.
## 4) 주의: 중립 단어는 분석에 포함되지 않습니다.

# (7) 감성분석 결과로 미루어보았을 때 두 영화에 대한 관람객의 평가는 어떤 양상을 나타냅니까? 간단하게 설명해주세요.

