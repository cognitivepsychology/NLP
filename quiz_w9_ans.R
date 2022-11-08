## 1. tidymodels(rsample) 패키지의 함수들을 사용하여 데이터를 훈련용과 검증용으로 나눠주세요.
# 1) 난수를 123으로 고정해주세요.
# 2) 데이터"email.rda"를 다운받아 로딩해주세요.
# 3) 훈련용 데이터를 train.email, 검증용 데이터를 test.email 객체에 할당해주세요.
# 4) 훈련용 데이터의 비율은 0.75로 고정하고, 결과변수인 "spam"을 바탕으로 층화표집을 수행해주세요.

load("texts/week9/email.rda")
set.seed(123)
split.email <- initial_split(email, prop = 0.75, 
                             strata = "spam")
train.email <- training(split.email)
test.email <- testing(split.email)


## 2. caret 패키지의 train() 함수를 사용하여 모형을 생성해주세요.
# 1) 결과변수 spam을 모든 예측변수로 예측해주세요.
# 2) 훈련용 데이터는 train.email를 사용해주세요.
# 3) 알고리즘 엔진은 "knn"(K-최근접 이웃)으로 설정해주세요. 
# 4) 수립된 모형은 model.email 객체에 할당해주세요.

model.email <- train(
  spam ~ ., 
  data = train.email, 
  method = "knn"
)

## 3. caret 패키지의 trainControl() 함수를 사용하여 3겹 교차 타당도 확인을 3회 반복해주세요. 그리고 해당 결과를 cross.validation이라는 객체에 할당해주세요.

cross.validation <- trainControl(method = "repeatedcv", # CV의 유형.
                                 number = 3, # 겹의 수.
                                 repeats = 3) # 반복의 횟수.

## 4. 초매개변수 k는 완전탐색을 통해 최적의 매개변수를 찾아주세요.
# 1) expand.grid() 함수를 사용하여 탐색범위를 2에서 30(2, 3, 4,..30)까지로 설정해주세요. 
# 2) 해당 결과를 grid.search라는 객체에 할당해주세요.

grid.search <- expand.grid(k = seq(2, 20, by = 1)) # k의 범위를 2-20까지로 설정.

## 5. 최종 모형을 수립하는 단계입니다. 2의 모형에 재표집 전략, 탐색전략 그리고 손실함수 지표를 추가해주세요.
# 1) 손실함수 지표는 "Accuracy"로 설정해주세요.
# 2) 손실함수 지표에 따르면 k(이웃의 수)는 최소한 몇 개로 설정할 때 가장 모형의 적합도가 높습니까?

email_fit <- train(spam ~ ., # 결과변수 spamd을 email 데이터셋의 모든 예측변수로 예측.
                   data = train.email, # 훈련용 데이터셋.
                   method = "knn", # 모형생성 기법.
                   trControl = cross.validation, # 재표집 전략.
                   tuneGrid = grid.search, # 초매개변수 조정을 위한 탐색전략.
                   metric = "Accuracy")

## 6. train.email 데이터의 모든 등간척도 예측변수를 step_YeoJohnson() 함수를 사용하여 변형해주세요.
# 1) tidymodels의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 2) step_YeoJohnson() 함수를 사용하여 모든 등간척도 예측변수를 변형해주세요.
# 3) 최종 결과를 email_recipe_yj이라는 객체에 할당해주세요.

email_recipe_yj <- recipe(spam ~ ., data = train.email) %>% # recipe 함수에 예측모형 투입.
  step_YeoJohnson(all_numeric_predictors()) 

## 7. 훈련용 데이터의 모든 결과변수와 예측변수 결측값을 대체해주세요.
# 1) 데이터 "email.mis.rda"를 다운받아 로딩해주세요. 여기에는 5퍼센트의 무작위 결측값이 포함되어 있습니다.
# 2) tidymodels 패키지의 함수들을 사용하여 데이터(email.mis)를 훈련용과 검증용으로 나눠주세요. 훈련용의 비율은 0.75로 지정하고, 결과변수 spam을 바탕으로 층화표집을 수행해주세요. 훈련용 데이터는 train.mis 객체에, 검증용 데이터는 test.mis 객체에 할당해주세요.
# 3) tidymodels의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 4) 모든 등간척도 예측변수 결측값을 tidymodels 패키지 함수를 사용하여 중앙값으로 대체해주세요.
# 5) 모든 명목척도 예측변수 및 결과변수 결측값을 tidymodels 패키지 함수를 사용하여 최빈값으로 대체해주세요.

load("email.mis.rda")
split.mis <- initial_split(email.mis, prop = 0.75, 
                           strata = "spam")
train.mis  <- training(split.mis)
test.mis   <- testing(split.mis)

# 결과변수 로그화하기.
email_recipe <- recipe(spam ~ ., data = train.mis) %>% # recipe 함수에 예측모형 투입.
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal())

