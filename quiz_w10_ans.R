## 1. 훈련용 데이터의 모든 예측변수 결측값을 대체해주세요.
# 1) 데이터 "email.mis.rda"를 로딩해주세요. 여기에는 5퍼센트의 무작위 결측값이 포함되어 있습니다.
# 2) tidymodels 패키지의 함수들을 사용하여 데이터(email.mis)를 훈련용과 검증용으로 나눠주세요. 훈련용의 비율은 0.75로 지정하고, 결과변수 spam을 바탕으로 층화표집을 수행해주세요. 훈련용 데이터는 train.mis 객체에, 검증용 데이터는 test.mis 객체에 할당해주세요.
# 3) tidymodels의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 4) 모든 예측변수 결측값을 tidymodels 패키지의 k-최근접 이웃 기반 대체 함수를 사용하여 대체해주세요. 이떄 이웃의 수는 5로 지정해주세요.
# 5) 모든 예측변수 결측값을 tidymodels 패키지의 나무 구조 기반 대체 함수를 사용하여 대체해주세요.

load("email.mis.rda")
split.mis <- initial_split(email.mis, prop = 0.75, 
                           strata = "spam")
train.mis  <- training(split.mis)
test.mis   <- testing(split.mis)

# k-최근접 이웃 기반 대체 함수를 사용하여 결측값 대체하기.
recipe(spam ~ ., data = train.mis) %>% # recipe 함수에 예측모형 투입.
  step_impute_knn(all_predictors(), neighbors = 5)

# 나무 구조 기반 대체 함수를 사용하여 결측값 대체하기.
recipe(spam ~ ., data = train.mis) %>% # recipe 함수에 예측모형 투입.
  step_impute_bag(all_predictors(), neighbors = 5)

## 2. 정보성이 없는 영분산 또는 근접 변수를 탐지하고 제거해주세요.
# 1) 난수를 123으로 고정해주세요.
# 2) 데이터"email.rda"를 로딩해주세요.
# 3) tidymodels 패키지의 함수들을 사용하여 데이터(email)를 훈련용과 검증용으로 나눠주세요. 
# 4) 훈련용 데이터를 train.email, 검증용 데이터를 test.email 객체에 할당해주세요.
# 5) 훈련용 데이터의 비율은 0.75로 고정하고, 결과변수인 "spam"을 바탕으로 층화표집을 수행해주세요.
# 6) tidymodels 패키지의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 7) caret 패키지의 nearZeroVar() 함수를 사용하여 영분산 근접 변수를 티블 형식으로 제시해주세요. 총 몇 개의 예측변수가 영분산 변수(zeroVar) 및 영분산 근접 변수(nzv)로 탐지되었습니까?
# 8) tidymodels 패키지의 함수들을 사용해 영분산 변수와 영분산 근접 변수를 제거해주세요.

load("email.rda")
set.seed(123)
split.email <- initial_split(email, prop = 0.75, 
                             strata = "spam")
train.email <- training(split.email)
test.email <- testing(split.email)

# nearZeroVar()로 영분산 근접 변수 탐지하기.
nearZeroVar(train.email, saveMetrics = TRUE) %>% 
  rownames_to_column() %>% 
  filter(zeroVar|nzv)

# tidymodels 패키지의 함수들을 사용해 영분산 변수와 영분산 근접 변수 제거하기.
recipe(spam ~ ., data = train.email) %>% 
  step_zv() %>%
  step_nzv()

## 3. Boruta 패키지를 통해 잡음을 야기하거나 정보성이 적은 변수를 제거해주세요.
# 1) 난수를 123으로 고정해주세요.
# 2) Boruta() 함수를 사용하여 email 데이터셋에 대해 Boruta 알고리즘을 수행해주세요. 이때 예측모형은 모든 예측변수로 spam을 예측하는 것입니다. doTrace 논항은 3으로 지정해주세요.
# 3) getSelectedAttributes() 함수를 사용하여 어떤 변수들이 최종 선정되었는지 확인해주세요. 판단을 보류한 변수는 제외해주세요.

library(Boruta)
set.seed(123)
boruta.train.email <- Boruta(spam ~ ., data = train.email, doTrace = 3)
getSelectedAttributes(boruta.train.email, withTentative = F)

## 4. train.email 데이터셋에 포함된 모든 등간척도 예측변수를 정규화해주세요.
# 1) tidymodels 패키지의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 2) 모든 등간척도 예측변수를 정규화해주세요.

recipe(spam ~ ., data = train.email) %>%
  step_normalize(all_numeric_predictors())

## 5. train.email 데이터셋에 포함된 모든 범주변수를 더미 인코딩으로 변환해주세요.
# 1) tidymodels 패키지의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 2) 모든 범주변수를 더미 인코딩으로 변환해주세요.

recipe(spam ~ ., data = train.email) %>%
  step_dummy(all_nominal())

## 6. train.email 데이터에 대해 부분 최소 제곱으로 차원을 축소해주세요.
# 1) tidymodels 패키지의 recipe() 함수에 우리의 예측모형(모든 예측변수로 spam을 예측)을 투입해주세요.
# 2) 모든 등간척도 예측변수를 정규화해주세요.
# 3) tidymodels 패키지 함수를 사용하여 모든 등간척도 예측변수의 차원을 5개로 축소해주세요. 이때 결과변수는 spam으로 지정해주세요.

recipe(spam ~ ., data = train.email) %>%
  step_normalize(all_numeric_predictors()) %>% # 모든 등간척도 예측변수 정규화하기.
  step_pls(all_numeric_predictors(), outcome = "spam", num_comp = 5) 

