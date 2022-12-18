## 1. h2o 패키지의 h2o.prcomp() 함수로 주성분 분석 실행하기.
# 1) 데이터 "abalone.1.rda"를 로딩해주세요. 
# 2) 데이터의 각 열에 대한 설명: Type = 전복의 성(I는 infant), LongestShell= 가장 많이 자란 껍질의 길이, Diameter = 둘레, Height = 껍질 속 알맹이 크기, WholeWeight = 전체 전복 무게, ShuckedWeight = 알맹이 무게, VisceraWeight = 피를 제거한 내장 무게, ShellWeight = 말린 후 껍질 무게, Rings = 현미경을 통해 본 껍질의 원 무늬 개수(나이가 들면서 늘어남).
# 3) h2o.prcomp() 함수를 사용하여 주성분 분석을 수행해주세요. 그 결과는 my_pca 객체에 할당해주세요.
# 4) 수립된 모형의 고유 벡터를 서브세팅하여 PC1의 부하량을 ggplot() 함수로 시각화해주세요. 어느 변수가 가장 기여량이 큽니까?
# 5) 예측변수들이 주성분 PC1과 PC2 부하량에 어떻게 기여하는지 ggplot() 함수로 시각화해주세요. PC1에는 기여량이 크지만 PC2에는 기여량이 적은 변수들은 무엇입니까? 반대로 PC2에는 기여량이 크지만 PC1에는 기여량이 적은 변수들은 무엇입니까?

abalone <- read_csv("abalone.csv")
abalone.1 <- abalone %>%
  mutate(Type = case_when(Type == "M" ~ "1",
                          Type == "F" ~ "2",
                          Type == "I" ~ "3")) %>%
  mutate(Type = as.factor(Type))
save(abalone.1, file="abalone.1.rda")

# h2o 패키지 불러온 뒤 초기화하기.
library(tidyverse) 
library(h2o)
h2o.no_progress()  # 진행상황 바 끄기. 
h2o.init(max_mem_size = "5g") # 초기화하기.
abalone.h2o <- as.h2o(abalone.1)

# PCA 수행하기.
my_pca <- h2o.prcomp(
  training_frame = abalone.h2o,
  pca_method = "GramSVD",
  k = ncol(abalone.h2o), 
  transform = "STANDARDIZE", 
  impute_missing = TRUE,
  max_runtime_secs = 1000
)

# PC1의 부하량과 원래 예측변수 간의 관계 시각화하기.
my_pca@model$eigenvectors %>%  # 모형의 고유 벡터를 서브세팅함.
  as.data.frame() %>% # 데이터프레임 형식으로 변환.
  mutate(feature = row.names(.)) %>% # 행 이름을 feature(예측변수)라는 이름의 칼럼으로 만듦.
  ggplot(aes(pc1, reorder(feature, pc1))) + # x축은 PC1, y축은 feature(단 PC1의 크기에 따라 내림차순)으로 그래프 그리기.
  geom_point() # 데이터 포인트를 점으로 나타내기.

# PC1과 PC2 부하량과 원래 예측변수 간의 관계 비교하기.
my_pca@model$eigenvectors %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = feature)) +
  geom_text()


## 2. 주성분 수 선택하기.
# 1) 고윳값 준거를 바탕으로 주성분의 수를 선택해주세요. 몇 개의 주성분이 선택되었습니까?

# 고윳값 계산하기.
eigen <- my_pca@model$importance["Standard deviation", ] %>% # 표준편차만 서브세팅하기.
  unlist() %>% # 벡터로 변환하기.
  .^2 # 제곱하기.

# 고윳값의 합이 1과 같거나 1보다 큰 주성분 찾기.
which(eigen >= 1) # PC1 한 개!

# 2) 주성분에 의해 설명되는 총 분산의 비율 준거(90퍼센트)를 바탕으로 주성분의 수를 선택해주세요. 몇 개의 주성분이 선택되었습니까?

# PC, PVE, CVE 칼럼으로 구성된 데이터프레임으로 생성하기.
pve_cvd <- data.frame(
  PC  = my_pca@model$importance %>% seq_along(),
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),
  CVE = my_pca@model$importance %>% .[3,] %>% unlist()
) %>%
  gather(metric, variance_explained, -PC) 

# 총 분산의 90퍼센트 이상을 설명하는 데 필요한 주성분의 수.
min(which(pve_cvd$variance_explained >= 0.90)) # PC1, 2, 3 => 3개!

# 3) 스크리 플롯 준거를 바탕으로 주성분의 수를 선택해주세요. 몇 개의 주성분이 선택되었습니까?

# PC와 PVE 칼럼으로 구성된 데이터프레임 생성하기.
scree_pc <- data.frame(
  PC = my_pca@model$importance %>% seq_along,
  PVE = my_pca@model$importance %>% .[2,] %>% unlist()
)

# 스크리 플롯 생성하기.
scree_pc %>%
  ggplot(aes(PC, PVE, group = 1, label = PC)) + # x축 = PC, y축 = PVE, group = 선을 하나만 생성, label = PC 번호를 레이블로 취함.
  geom_point() +
  geom_line() +
  geom_text(nudge_y = -.002) # 아랫쪽으로 .002만큼 떨어진 곳에 텍스트 넣기. => 2개의 주성분 선택!

## 3. cluster 패키지의 agnes() 함수를 사용하여 응집형 위계적 군집분석 실행하기.
# 1) 데이터 "USArrests.1.rda"를 로딩해주세요.
# 2) 데이터의 각 열에 대한 설명: 1973년 미국의 50개 주에서 일어난 인구 10만 명당 살인(Murder), 폭행(Assault), 성폭행(Rape), 시내지역 거주 인구의 퍼센트(UrbanPop). 참고로, 각 열의 값들은 표준화되어 있습니다.
# 3) agnes() 함수를 사용하여 위계적 군집분석을 시행해주세요. 그 결과는 hc라는 객체에 할당해주세요. 관찰값 간 거리는 manhattan 거리, 군집 간 거리는 최장 연결법으로 설정해주세요.
# 4) 응집계수를 계산해주세요. 응집계수는 얼마입니까?
# 5) 완전탐색을 통해 "average", "single", "complete", "ward" 등 네 가지 군집화 방법의 응집계수를 비교해주세요. 어느 방법의 응집계수가 가장 높습니까?

USArrests.1 <- USArrests %>%
  na.omit() %>%
  scale()
save(USArrests.1, file="USArrests.1.rda")

library(cluster)

# 난수 고정하기.
set.seed(123)

# 최장 연결법을 사용하여 위계적 군집분석 수행하기.
hc <- agnes(USArrests.1, stand = T, metric = "manhattan", method = "complete")

# 응집계수.
hc$ac # 0.8777498

# 완전탐색을 통해 평가할 군집화 방법
m <- c( "average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward") # 벡터 m의 각 요소에 이름 부여하기.

# 응집계수 계산용 함수.
ac <- function(x) {
  agnes(USArrests.1, method = x)$ac
}

# 각 군집화 방법의 응집계수 비교하기.
map_dbl(m, ac) # ward가 0.9346210로 가장 높음.

## 4. factoextra 패키지의 fviz_nbclust() 함수를 사용하여 최적의 군집개수 찾아내기.
# 1) USArrests.1 데이터에 대해 팔꿈치 방법, 실루엣 방법, 갭 통계치를 사용하여 각각 최적의 군집개수를 시각화해주세요. 이떄 생성 가능한 최대 군집개수는 20으로 설정해주세요.
# 2) gridExtra 패키지의 grid.arrange() 함수를 사용하여 1번 플롯 세 개를 다면출력해주세요.
# 3) 실루엣 방법과 갭 통계치를 사용한 경우, 최적의 군집개수는 각각 몇 개입니까?

library(factoextra)
library(gridExtra)

# 최적의 군집 개수 찾기 및 시각화.
p1 <- fviz_nbclust(USArrests.1, FUN = hcut, method = "wss", # 팔꿈치 방법 사용.
                   k.max = 20) +
  ggtitle("(A) 팔꿈치 방법")
p2 <- fviz_nbclust(USArrests.1, FUN = hcut, method = "silhouette", # 실루엣 방법 사용.
                   k.max = 20) +
  ggtitle("(B) 실루엣 방법")
p3 <- fviz_nbclust(USArrests.1, FUN = hcut, method = "gap_stat", # 갭 통계치 사용.
                   k.max = 20) +
  ggtitle("(C) 갭 통계치")

# 최적의 군집 개수 플롯 3개 다면출력.
grid.arrange(p1, p2, p3, nrow = 1) # 실루엣 방법 2개, 갭 통계치 3개.

## 5. factoextra 패키지의 fviz_dend() 함수를 사용하여 덴드로그램 그리기.
# 1) 4번의 hc 객체를 fviz_dend()에 투입하여 덴드로그램을 그려주세요.

fviz_dend(hc)

# 2) factoextra 패키지의 hcut() 함수를 사용하여 하위군집들이 어떻게 형성되어 있는지 확인해주세요. 이때 데이터는 USArrests.1, 군집화 방법은 agnes, 군집개수는 4개를 설정해주세요. 그 결과는 sub_grp이라는 객체에 할당해주세요.

# hcut() 함수로 덴드로그램 자르기.
sub_grp <- hcut(USArrests.1, hc_func = "agnes", k = 4, stand = T) # 군집 수 4개로 설정.

# 3) factoextra 패키지의 fviz_dend() 함수에 sub_grp를 투입하여 덴드로그램을 그려주세요. 이때 군집 주위에 사각형 테두리를 넣고 그 안을 채워주세요.

fviz_dend(
  sub_grp, 
  rect = TRUE,
  rect_fill = TRUE
)
