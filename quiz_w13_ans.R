## 1. caret 패키지의 train() 함수로 서포트 벡터 머신 모형 훈련시키기.
# 1) 데이터"email.rda"를 로딩해주세요. 
# 2) 난수를 123으로 고정해주세요.
# 3) tidymodels 패키지의 함수들을 사용하여 데이터(email)를 훈련용과 검증용으로 나눠주세요. 
# 4) 훈련용 데이터를 train.email, 검증용 데이터를 test.email 객체에 할당해주세요.
# 5) 훈련용 데이터의 비율은 0.75로 고정하고, 결과변수인 "spam"을 바탕으로 층화표집을 수행해주세요.
# 6) tidymodels 패키지의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 7) 재표집 전략: 교차 타당도를 10회 시행해주세요.
# 8) 서포트 백터 머신 알고리즘은 "svmRadial"을 설정해주세요.
# 9) caret 패키지 train() 함수의 preProcess 논항을 사용하여 자료를 표준화해주세요.
# 10) sigma와 C의 무작위 조합은 20으로 설정해주세요.
# 11) 가장 정확도가 높은 C는 무엇이고, 해당 정확도는 얼마입니까?

library(tidyverse) # 데이터 조작 및 시각화용.
library(tidymodels) # 데이터 전처리 및 모형 훈련용.
library(caret) # 모형 훈련용.

load("email.rda")
set.seed(123)
split.email <- initial_split(email, prop = 0.75, 
                             strata = "spam")
train.email <- training(split.email)
test.email <- testing(split.email)

# 재표집 전략 규정하기.
cv_method <- trainControl(
  method = "cv", 
  number = 10)

# caret 패키지로 SVM 모형 생성하기.
set.seed(123)  
email_svm <- train(
  spam ~ ., 
  data = train.email,
  method = "svmRadial",   
  preProcess = c("center", "scale"), # tidymodels로 전처리할 경우 오류 발생!
  trControl = cv_method,
  tuneLength = 20 # sigma와 C의 무작위 조합 = 20가지.
)

# SVM 모형 생성 결과 확인하기.
email_svm$bestTune # C = 256.00 / 정확도 = 0.9265235

## 2. caret 패키지의 train() 함수로 서포트 벡터 머신 모형 훈련시키기: 층위확률 논항 추가하기.
# 1) 데이터"email.1.rda"를 로딩해주세요.
# 2) tidymodels 패키지의 함수들을 사용하여 데이터(email.1)를 훈련용과 검증용으로 나눠주세요. 난수는 123으로 고정해주세요.
# 3) 훈련용 데이터를 train.email, 검증용 데이터를 test.email 객체에 할당해주세요.
# 4) 훈련용 데이터의 비율은 0.75로 고정하고, 결과변수인 "spam"을 바탕으로 층화표집을 수행해주세요.
# 5) SVM은 새로운 관찰값이 주어졌을 때 spam과 정상 email 중 어느 층위에 떨어질 것인지를 예측해주는 층위확률 예측값을 자동으로 제공해주지 않습니다. 이에 따라 필요한 두 개의 매개변수를 tranControl() 함수에 추가해주세요. 재표집 전략은 1번과 같습니다.
# 6) 난수는 123으로 고정해주세요.
# 7) 정확도 측정치는 "ROC"을 사용해주세요.
# 8) caret 패키지 train() 함수의 preProcess 논항을 사용하여 영분산 변수를 제거하고, 자료를 표준화해주세요.
# 9) 서포트 백터 머신 알고리즘은 "svmRadial"을 설정해주세요.
# 10) sigma와 C의 무작위 조합은 20으로 설정해주세요.
# 11) 가장 정확도가 높은 C는 무엇이고, 해당 정확도는 얼마입니까? (계산시간이 다소 걸립니다.)
# 12) vip 패키지를 사용하여 상위 10개 변수들의 중요도를 시각화해주세요. 이때 난수는 123, method = "permute", metric = "auc", nsim = 5로 설정해주세요. 가장 중요한 변수는 무엇입니까?

# SVM을 위한 재표집 전략 및 층위확률 예측값 계산 전략 수립.
ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,                 
  summaryFunction = twoClassSummary  # AUC/ROC 구하는 데 필요.
)

# SVM 모형 수립.
set.seed(123)  
email_svm_auc <- train(
  spam ~ ., 
  data = train.email.1,
  method = "svmRadial",
  preProcess = c("zv", "center", "scale"), 
  metric = "ROC",       
  trControl = ctrl,
  tuneLength = 20 # sigma와 C의 무작위 조합 = 20가지.
)

# pred_wrapper의 논항으로 들어갈 함수: 실제값 "spam"에 대한 예측값 계산.
prob_yes <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, "spam"]
}

# 예측변수 중요도 시각화: Vip().
set.seed(123) # 난수 고정.
vip::vip(email_svm_auc, method = "permute", nsim = 5, train = train.email.1, 
         target = "spam", metric = "auc", reference_class = "spam", 
         pred_wrapper = prob_yes) # line_breaks가 가장 높은 중요도를 지님.

## 3. caretEnsemble 패키지의 caretList() 함수로 총 4개의 모형 동시에 수립하기.
# 1) email.1 데이터셋의 훈련용 데이터를 바탕으로 다음과 같은 4개의 모형을 수립해주세요. 
# 2) 첫 번째는 "glm"(로지스틱 회귀 모형), 두 번째는 "rpart"(의사나무 결정 모형)입니다.
# 3) 세 번째 모형와 네 번째 모형은 "ranger"(랜덤 포레스트 모형) 모형입니다. 자료 전처리의 경우, 세 번째 모형은 영분산 근접 변수 제거를, 네 번째 모형은 영분산 근접 변수 제거 + PCA 방법을 사용해주세요.
# 4) 세 번째와 네 번째 모형은 완전탐색을 사용하여 최적의 mtry, min.node.size를 찾아주세요. 이때 완전탐색을 위한 expand.grid() 함수에 mtry = floor(n_features * c(.25, .333, .5)), min.node.size = c(1, 3, 5, 9), splitrule = "gini" 논항을 투입해주세요. 참고로, n_features는 예측변수의 수를 가리킵니다.
# 5) 힌트: 첫 번째와 두 번쨰는 사용자 조정 초매개변수가 투입되지 않으므로 method 논항을, 세 번째와 네 번째는 사용자 조정 초매개변수가 투입되므로 tuneList 논항을 사용해야 합니다.
# 6) 재표집 전략: 부트스트래핑 25회, 최적의 초매개변수 기반 예측 저장, 층위별 확률 계산, 층위가 2개인 분류모형 수행평가 측정치 계산.
# 7) 4개 모형 간 예측값의 상관을 계산해주세요. 어느 모형와 어느 모형 간의 상관이 가장 높습니까?

library(caretEnsemble)

my_control <- trainControl(
  method="boot", # 재표집 방법 = 부트스트래핑.
  number=25, # 부트스트래핑 횟수 = 25회.
  savePredictions="final", # 개별 재표집에 대한 예측들을 얼마나 많이 저장할 것인가? "final" = 최적의 초매개변수들에 기반한 예측들을 저장하겠다는 뜻.
  classProbs=TRUE, # 각 층위별 확률들을 계산할 것인가? 
  summaryFunction=twoClassSummary # 수행평가 metric을 계산하는 함수. twoClassSummary = 층위가 2개인 분류모형 수행평가 metric 계산.
)

n_features <- ncol(train.email.1) - 1
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.25, .333, .5)),
  min.node.size = c(1, 3, 5, 9), 
  splitrule = "gini"
)

model_list_custom <- caretList(
  spam ~ ., data = train.email.1,
  trControl = my_control,
  metric = "ROC",
  methodList = c("glm", "rpart"),
  tuneList=list(
    rf1 = caretModelSpec(method = "ranger", tuneGrid = hyper_grid, proProcess = "nzv"),
    rf2 = caretModelSpec(method = "ranger", tuneGrid = hyper_grid, preProcess = c("nzv", "pca"))
  )
)
save(model_list_custom, file='model_list_custom.rda')

modelCor(resamples(model_list_custom))

# 4. caretEnsemble 패키지의 caretStack() 함수를 사용하여 모형 쌓기
# 1) GBM(gradient boosting machine) 알고리즘을 사용하여 모형을 쌓아주세요.
# 2) 모형을 쌓을 때 3번 모형 리스트를 사용해주세요.
# 3) 무선탐색은 10회로 설정해주세요.
# 4) 수행평가 지표는 "ROC"으로 설정해주세요.
# 5) 재표집 전략: 재표집 전략: 부트스트래핑 10회, 최적의 초매개변수 기반 예측 저장, 층위별 확률 계산, 층위가 2개인 분류모형 수행평가 측정치 계산.
# 6) 예측값 구하기: email.test.1 데이터를 바탕으로 검증용 데이터에 대해 예측값을 구해주세요.
# 7) caTools 패키지의 colAUC() 함수를 이용하여 네 모형과 앙상블 모형의 AUC를 구해주세요.
# 8) 네 개의 개별 모형과 앙상블 모형의 중 어느 모형의 AUC가 가장 높습니까?

library(gbm)
library(caTools)

model_preds <- map(model_list_custom, predict, newdata=test.email.1, type="prob") %>% # model_list를 기반으로 검증용 데이터에 대해 예측값 구하기.
  map(function(x) x[,"spam"]) %>% # 결과변수 층위가 spam인 경우의 예측값만 서브세팅하기.
  as_tibble()

gbm_ensemble <- caretStack(
  model_list_custom,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

model_preds$ensemble <- predict(gbm_ensemble, newdata=test.email.1, type="prob")
colAUC(model_preds, test.email.1$spam)


 