# 모형수립 과정에 사용되는 주요 패키지.
library(tensorflow)
library(keras)
library(reticulate)
library(tidyverse) # 데이터 조작 및 시각화용.
library(tidymodels)
library(tidytext)
library(RcppMeCab)
options(tibble.width = Inf)


#file_path <- "E:/연구용 텍스트 자료/네이버 쇼핑 리뷰/naver_shopping.txt"
#shopping_revew <- read_tsv(file_path, col_names = F)

#shopping_revew.1 <- shopping_revew %>%
  #rename(label = X1, document = X2) %>%
  #filter(label >=4 | label  <= 2) %>%
  #mutate(label = case_when(label >=4 ~ 1,
                           #label <=2 ~ 0)) %>%
  #unique()
#save(shopping_revew.1, file="shopping_revew.1.rda")

### 1. 텍스트 이분분류 딥 러닝 실행하기: 자체 임베딩 사용하기.
## 1) 네이버 쇼핑 리뷰 데이터셋인 "shopping_revew.1.rda"를 로딩해주세요.
## 2) 데이터를 0.7:0.3으로 나누어주세요.
## 3) 데이터에 대해 1차 정제를 실행해주세요.
## 4) 데이터에 대해 2차 정제를 실행해주세요.
## 5) 리뷰의 최대 단어 수(sequence_length)는 25개로 설정해주세요. 그리고 단어집합의 수록단어 수(vocab_size)는 빈도 2 이상인 단어의 수 + 1로 설정해주세요.
## 6) 데이터에 대해 3차 정제를 실행해주세요.
## 7) 훈련용 & 검증용 데이터 모두 순서를 무선화해주세요. 이때 텍스트와 레이블에 대해 난수를 똑같이 고정하는 것을 잊지 마세요.
## 8) 훈련용 & 검증용 데이터 벡터를 정수 시퀀스로 변환하고, 패딩을 실행해주세요.
## 9) 모형을 수립하고 훈련을 실행해주세요. 이때 에포크는 5로 지정합니다. 가장 성능이 좋았던 모형의 검증 정확도는 얼마입니까?
## 10) sentiment_predict 함수에 다음의 네 리뷰 문장을 넣어 긍정과 부정 감성을 예측해주세요. 각각 긍정/부정 확률이 몇 퍼센트로 예측됩니까?
# A. "이 상품 진짜 좋아요... 저는 강추합니다. 대박"
# B. "진짜 배송도 늦고 개짜증나네요. 뭐 이런 걸 상품이라고 만드나?"
# C. "판매자님... 너무 짱이에요.. 대박나삼"
# D. "ㅁㄴㅇㄻㄴㅇㄻㄴㅇ리뷰쓰기도 귀찮아"

# 데이터 나누기.
load("shopping_revew.1.rda")
set.seed(123) # 재현 가능성을 위해 난수 고정.
split <- initial_split(shopping_revew.1, prop = 0.7, strata = "label")
shopping_train <- training(split)
shopping_test <- testing(split) 
save(shopping_train, file="shopping_train.rda")
save(shopping_test, file="shopping_test.rda")


## 데이터 1차 정제 ##

# 각종 기호 및 외국어 제거하기: 훈련용 셋.
shopping_train.1 <- shopping_train %>%
  mutate(document = str_replace_all(document, "\\W" ," ")) %>%
  mutate(document = str_replace_all(document, "[^ㄱ-ㅎ가-힣0-9 ]", "")) %>%
  filter(document != "") %>%
  mutate(id = seq(1:nrow(shopping_train)))
save(shopping_train.1, file="shopping_train.1.rda")

# 각종 기호 및 외국어 제거하기: 검증용 셋. 
shopping_test.1 <- shopping_test %>%
  mutate(document = str_replace_all(document, "\\W" ," ")) %>%
  mutate(document = str_replace_all(document, "[^ㄱ-ㅎ가-힣0-9 ]", "")) %>%
  filter(document != "") %>%
  mutate(id = seq(1:nrow(shopping_test)))
save(shopping_test.1, file="shopping_test.1.rda")


## 데이터 2차 정제 ##

# shopping 데이터셋 텍스트 토큰화하기: 훈련용 셋.
shopping_train.1.tokenize <- shopping_train.1 %>%
  mutate(document = enc2utf8(document)) %>%
  unnest_tokens(word, document, token = RcppMeCab::pos) # 백그라운드 실행 권장.
save(shopping_train.1.tokenize, file="shopping_train.1.tokenize.rda")

# shopping 데이터셋 텍스트 토큰화하기: 검증용 셋.
shopping_test.1.tokenize <- shopping_test.1 %>%
  mutate(document = enc2utf8(document)) %>%
  unnest_tokens(word, document, token = RcppMeCab::pos) # 백그라운드 실행 권장.
save(shopping_test.1.tokenize, file="shopping_test.1.tokenize.rda")

# 형태소 태그 제거하기: 훈련용 셋.
set.seed(123)
shopping_train.1.tokenize.stopword <- shopping_train.1.tokenize %>%
  mutate(word = str_remove_all(word, "[ㄱ-ㅎ가-힣0-9]{1,100}/e[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/j[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/vcp|[ㄱ-ㅎ가-힣0-9]{1,100}/xs[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/nnb[a-z]{0,3}")) %>%
  mutate(word = str_remove_all(word, "/.*")) %>%
  filter(word != "") %>%
  filter(str_detect(word, "^\\+.*") == F) 
save(shopping_train.1.tokenize.stopword, file="shopping_train.1.tokenize.stopword.rda")

# 형태소 태그 제거하기: 검증용 셋.
shopping_test.1.tokenize.stopword <- shopping_test.1.tokenize %>%
  mutate(word = str_remove_all(word, "[ㄱ-ㅎ가-힣0-9]{1,100}/e[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/j[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/vcp|[ㄱ-ㅎ가-힣0-9]{1,100}/xs[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/nnb[a-z]{0,3}")) %>%
  mutate(word = str_remove_all(word, "/.*")) %>%
  filter(word != "") %>%
  filter(str_detect(word, "^\\+.*") == F) 
save(shopping_test.1.tokenize.stopword, file="shopping_test.1.tokenize.stopword.rda")

# 타입 개수 계산하기.
shopping_train.1.tokenize.stopword$word %>%
  unique() %>%
  length() # 타입 개수 = 35653개

# 빈도 2회 이상인 타입 추출하기.
shopping_train.1.tokenize.stopword.1 <- shopping_train.1.tokenize.stopword %>%
  count(word) %>%
  filter(n > 3) # 빈도 2회 이상인 타입 13,443개.

# 가장 긴 리뷰는 단어가 47개.

max.len <- shopping_train.1 %>%
  mutate(len = str_count(document, "\\w+")) %>%
  arrange(desc(len)) %>%
  filter(len != 99266)

# 단어 개수 분포 시각화하기.
max.len %>%
  ggplot(aes(len)) +
  geom_histogram(bins = 50) 
  
# 단어 수가 25개 이하인 리뷰가 전체의 96.6퍼센트.
max.len %>%
  count(len < 25)
128164/(128164 + 4506) # 0.966036

# 단어집합 수록 단어의 수 & 리뷰의 최대 길이.
vocab_size <- nrow(shopping_train.1.tokenize.stopword.1) + 1 # 13444개.
sequence_length <- 25 # 리뷰의 최대 길이로 정하기!


## 데이터 3차 정제 ##

# 훈련용 데이터 #

# 불용어 제거하기: 훈련용 텍스트 데이터셋.
shopping_train.1.tokenize.stopword.2 <- shopping_train.1.tokenize.stopword %>%
  inner_join(shopping_train.1.tokenize.stopword.1[, 1], by = "word") 
save(shopping_train.1.tokenize.stopword.2, file="shopping_train.1.tokenize.stopword.2.rda")

# 리뷰별로 단어들 합치기 1.
shopping_train.1.tokenize.stopword.list <- split(shopping_train.1.tokenize.stopword.2$word, shopping_train.1.tokenize.stopword.2$id)
save(shopping_train.1.tokenize.stopword.list, file="shopping_train.1.tokenize.stopword.list.rda")
glimpse(shopping_train.1.tokenize.stopword.list)

# 리뷰별로 단어들 합치기 2.
shopping_train.1.tokenize.stopword.list.1 <- map(shopping_train.1.tokenize.stopword.list, function(x) str_c(x, collapse = " "))
shopping_train.1.tokenize.stopword.list.1[1:10]

# 리뷰 순서 다시 무작위로 배열하기: 텍스트.
set.seed(123)
shopping_train.1.tokenize.stopword.list.1 <- sample(shopping_train.1.tokenize.stopword.list.1, length(shopping_train.1.tokenize.stopword.list.1))
save(shopping_train.1.tokenize.stopword.list.1, file="shopping_train.1.tokenize.stopword.list.1.rda")
glimpse(shopping_train.1.tokenize.stopword.list.1)
        
# 정제된 훈련용 텍스트 데이터셋 매트릭스로 변환하기.
shopping_train.1.tokenize.stopword.matrix <- do.call(plyr::rbind.fill.matrix, shopping_train.1.tokenize.stopword.list.1)
save(shopping_train.1.tokenize.stopword.matrix, file="shopping_train.1.tokenize.stopword.matrix.rda")

# 리뷰별로 레이블(1, 0) 부착하기.
shopping_train.1.tokenize.label.list <- split(shopping_train.1.tokenize.stopword.2$label, shopping_train.1.tokenize.stopword.2$id) %>%
  map(function(x) unique(x))

# 리뷰 순서 다시 무작위로 배열하기: 레이블.
set.seed(123)
shopping_train.1.tokenize.label.list <- sample(shopping_train.1.tokenize.label.list, length(shopping_train.1.tokenize.label.list))
save(shopping_train.1.tokenize.label.list, file="shopping_train.1.tokenize.label.list.rda")
glimpse(shopping_train.1.tokenize.label.list)

# 훈련용 레이블 데이터셋 매트릭스로 변환하기.
shopping_train.1.tokenize.label.matrix <- shopping_train.1.tokenize.label.list %>%
  as.matrix()
save(shopping_train.1.tokenize.label.matrix, file="shopping_train.1.tokenize.label.matrix.rda")

# 검증용 데이터 #

# 리뷰별로 단어들 합치기 1.
shopping_test.1.tokenize.stopword.list <- split(shopping_test.1.tokenize.stopword$word, shopping_test.1.tokenize.stopword$id)
save(shopping_test.1.tokenize.stopword.list, file="shopping_test.1.tokenize.stopword.list.rda")
glimpse(shopping_test.1.tokenize.stopword.list)

# 리뷰별로 단어들 합치기 2.
shopping_test.1.tokenize.stopword.list.1 <- map(shopping_test.1.tokenize.stopword.list, function(x) str_c(x, collapse = " "))

# 리뷰 순서 다시 무작위로 배열하기.
set.seed(123)
shopping_test.1.tokenize.stopword.list.1 <- sample(shopping_test.1.tokenize.stopword.list.1, length(shopping_test.1.tokenize.stopword.list.1))
shopping_test.1.tokenize.stopword.list.1[1:10]
save(shopping_test.1.tokenize.stopword.list.1, file="shopping_test.1.tokenize.stopword.list.1.rda")

# 검증용 텍스트 데이터셋 매트릭스로 변환하기.
shopping_test.1.tokenize.stopword.matrix <- do.call(plyr::rbind.fill.matrix, shopping_test.1.tokenize.stopword.list.1)
save(shopping_test.1.tokenize.stopword.matrix, file="shopping_test.1.tokenize.stopword.matrix.rda")

# 리뷰별로 레이블(1, 0) 부착하기.
shopping_test.1.tokenize.label.list <- split(shopping_test.1.tokenize.stopword$label, shopping_test.1.tokenize.stopword$id) %>%
  map(function(x) unique(x))

# 리뷰 순서 다시 무작위로 배열하기: 레이블.
set.seed(123)
shopping_test.1.tokenize.label.list <- sample(shopping_test.1.tokenize.label.list, length(shopping_test.1.tokenize.label.list))
save(shopping_test.1.tokenize.label.list, file="shopping_test.1.tokenize.label.list.rda")
glimpse(shopping_test.1.tokenize.label.list)

# 검증용 레이블 데이터셋 매트릭스로 변환하기.
shopping_test.1.tokenize.label.matrix <- shopping_test.1.tokenize.label.list %>%
  as.matrix()
save(shopping_test.1.tokenize.label.matrix, file="shopping_test.1.tokenize.label.matrix.rda")

# 훈련용 & 검증용 데이터셋 벡터로 변환하기 #

# 벡터로 변환하기: 훈련용 텍스트 셋.
shopping_train.1.tokenize.stopword.vector <- as.vector(shopping_train.1.tokenize.stopword.matrix)
save(shopping_train.1.tokenize.stopword.vector, file="shopping_train.1.tokenize.stopword.vector.rda")
is.vector(shopping_train.1.tokenize.stopword.vector)

# 벡터로 변환하기: 훈련용 레이블 셋.
shopping_train.1.tokenize.label.vector <- as.vector(shopping_train.1.tokenize.label.matrix)
save(shopping_train.1.tokenize.label.vector, file="shopping_train.1.tokenize.label.vector.rda")

# 벡터로 변환하기: 검증용 텍스트 셋.
shopping_test.1.tokenize.stopword.vector <- as.vector(shopping_test.1.tokenize.stopword.matrix)
save(shopping_test.1.tokenize.stopword.vector, file="shopping_test.1.tokenize.stopword.vector.rda")

# 벡터로 변환하기: 검증용 레이블 셋.
shopping_test.1.tokenize.label.vector <- as.vector(shopping_test.1.tokenize.label.matrix)
save(shopping_test.1.tokenize.label.vector, file="shopping_test.1.tokenize.label.vector.rda")


## 정수 시퀀스 변환 및 패딩하기 ##

# text_tokenizer 함수를 이용하여 단어마다 고유의 숫자 부여하기.
tokenizer_vec <- text_tokenizer(num_words = vocab_size) %>%
  fit_text_tokenizer(shopping_train.1.tokenize.stopword.vector)

# texts_to_sequences 함수를 이용하여 텍스트를 정수 시퀀스로 변환하기.
shopping_train.1.tokenize.sequences <- texts_to_sequences(tokenizer_vec, shopping_train.1.tokenize.stopword.vector)
shopping_test.1.tokenize.sequences <- texts_to_sequences(tokenizer_vec, shopping_test.1.tokenize.stopword.vector)

# 단어 길이가 채 25개가 안 되는 경우 0으로 패딩하여 길이를 맞춰줌.
shopping_train_pad <- pad_sequences(shopping_train.1.tokenize.sequences, maxlen = sequence_length)
shopping_test_pad <- pad_sequences(shopping_test.1.tokenize.sequences, maxlen = sequence_length)


## 모형 수립 및 훈련하기 ##

# 입력층 설정하기.
inputs <- layer_input(c(NA), dtype = "int64")

# 임베딩층 설정하기.
embedded <- inputs %>%
  layer_embedding(input_dim = vocab_size, # 자체 텍스트로 훈련한 임베딩 사용.
                  output_dim = 256,
                  mask_zero = TRUE)

# 출력층 설정하기.
outputs <- embedded %>%
  bidirectional(layer_lstm(units = 32)) %>%
  layer_dropout(0.5) %>%
  layer_dense(1, activation = "sigmoid")

# 모형 설정하기.
model <- keras_model(inputs, outputs)

# 모형 컴파일링하기.
model %>% compile(optimizer = "rmsprop", # 기본 옵티마이저. RNN 옵티마이저로 많이 사용.
                  loss = "binary_crossentropy",
                  metrics = "accuracy")
model

# 콜백 설정하기.
callbacks <- list(callback_model_checkpoint("embeddings_bidir_lstm_with_masking.keras",
                                            save_best_only = TRUE)  # 가장 정확도가 높은 모형 저장.                
)

# 모형 학습하기.
history <- model %>% keras::fit(
  x=shopping_train_pad, y=shopping_train.1.tokenize.label.vector %>% as_vector(),
  validation_split = 0.2, # 훈련용 셋의 20퍼센트를 validation 셋으로 사용.
  epochs = 2, # 편의상 에포크를 2회만 적용.
  callbacks = callbacks
)

model <- load_model_tf("embeddings_bidir_lstm_with_masking.keras") # 가장 정확도 높은 모형 띄우기.

# 검증 정확도 확인하기.
cat(sprintf("검증 정확도: %.3f\n",
            evaluate(model, 
                     x=shopping_test_pad, 
                     y=shopping_test.1.tokenize.label.vector %>% as_vector())["accuracy"])) # 0.911.


sentiment_predict <- function(new_sentence){
  # 텍스트 사전 정제하기.
  new_sentence_1 <- new_sentence %>%
    as_tibble() %>% 
    dplyr::rename(word = value) %>%
    mutate(word = str_replace_all(word, "\\W" ," ")) %>%
    mutate(word = str_replace_all(word, "[^ㄱ-ㅎ가-힣0-9 ]", "")) %>%
    filter(word != "") %>%
    
    # 텍스트 토큰화하기.  
    mutate(word = enc2utf8(word)) %>%
    unnest_tokens(word, word, token = RcppMeCab::pos) %>%
    mutate(word = str_remove_all(word, "[ㄱ-ㅎ가-힣0-9]{1,100}/e[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/j[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/vcp|[ㄱ-ㅎ가-힣0-9]{1,100}/xs[a-z]{1,3}|[ㄱ-ㅎ가-힣0-9]{1,100}/nnb[a-z]{0,3}")) %>%
    mutate(word = str_remove_all(word, "/.*")) %>%
    filter(word != "") %>%
    filter(str_detect(word, "^\\+.*") == F) %>%
    .[1] %>%
    as_vector() %>% # 벡터로 변환하기.
    str_c(collapse = " ") # 하나의 문자열로 합치기.
  
  # 정수 인코딩하기.
  encoded <- texts_to_sequences(tokenizer_vec, new_sentence_1) # 기존 모형 수립에 사용한 tokenizer_vec 객체여야 함!
  
  # 패딩하기.
  pad_new <- pad_sequences(encoded, maxlen = sequence_length) 
  
  # 예측하기.
  score <- predict(model, pad_new) %>% 
    as.vector() %>%
    .[1]
  
  # 리뷰 긍부정 여부 확인 멘트 제공하기
  if(score > 0.5){
    print(sprintf("%.2f퍼센트의 확률로 긍정 리뷰입니다.", score*100 ))
  } else {
    print(sprintf("%.2f퍼센트의 확률로 부정 리뷰입니다.", (1-score)*100 ))
  }
}

sentiment_predict("이 상품 진짜 좋아요... 저는 강추합니다. 대박")
sentiment_predict("진짜 배송도 늦고 개짜증나네요. 뭐 이런 걸 상품이라고 만드나?")
sentiment_predict("판매자님... 너무 짱이에요.. 대박나삼")
sentiment_predict("ㅁㄴㅇㄻㄴㅇㄻㄴㅇ리뷰쓰기도 귀찮아")


### 2. 텍스트 이분분류 딥 러닝 실행하기: FastText 임베딩 사용하기.
## 1) 훈련용 & 검증용 데이터는 1번에서 사용한 데이터를 그대로 사용해주세요.
## 2) 한국어 FastText 단어 임베딩을 만들어주세요. 모형은 cc.ko.300.bin 파일을 사용해주세요. 그리고 임베딩 차원은 300으로 지정해주세요.
## 3) 모형을 수립하고 훈련을 실행해주세요. 이때 에포크는 5로 지정합니다. 가장 성능이 좋았던 모형의 검증 정확도는 얼마입니까?
## 4) sentiment_predict 함수에 다음의 네 리뷰 문장을 넣어 긍정과 부정 감성을 예측해주세요. 각각 긍정/부정 확률이 몇 퍼센트로 예측됩니까?
# A. "이 상품 진짜 좋아요... 저는 강추합니다. 대박"
# B. "진짜 배송도 늦고 개짜증나네요. 뭐 이런 걸 상품이라고 만드나?"
# C. "판매자님... 너무 짱이에요.. 대박나삼"
# D. "ㅁㄴㅇㄻㄴㅇㄻㄴㅇ리뷰쓰기도 귀찮아"

## 한국어 FastTest 단어 임베딩 만들기 ##

# 한국어 FastText 단어 임베딩 만들기.
library(fastrtext)
model_path_ko <- 'E:/연구용 텍스트 자료/FastText/bin_vector/cc.ko.300.bin' # wiki.ko.bin은 불러올 떄 오류가 나므로 cc.ko.300.bin 사용 권장.
model_ko <- load_model(model_path_ko) # 임베딩 모형 로딩.
word_index_1 <- tokenizer_vec$word_index # 훈련용 데이터셋의 단어 인덱스 추출하기.
word_index_2 <- names(word_index_1) # 훈련용 데이터셋의 단어 목록 저장하기.
word_index_2 <- append(word_index_2, "[UKN]", after=0) # 훈련용 데이터셋에는 있지만 임베딩 모형에는 없는 단어는 "[UKN]"으로 처리. 

embedding_fasttext <- get_word_vectors(model_ko, words = word_index_2) # 한국어 단어 임베딩 벡터 획득하기.
rownames(embedding_fasttext) <- NULL # 불필요한 행 이름 제거하기.
class(embedding_fasttext) # embedding_fasttext의 클래스 확인하기.
is.matrix(embedding_fasttext)
embedding_fasttext[1:2, ]

embedding_dim <- 300
vocab_size
sequence_length


## 모형 수립 및 학습하기 ##

# 임베딩층 설정하기.
embedding_layer <- layer_embedding(
  input_dim = vocab_size,
  output_dim = embedding_dim,
  embeddings_initializer = initializer_constant(embedding_fasttext),
  trainable = FALSE,
  mask_zero = TRUE
)

# 입력층 설정하기.
inputs <- layer_input(shape(NA), dtype = "int64")

# 입력을 임베딩 레이어 통과시키기.
embedded <- embedding_layer(inputs)

# 출력층 설정하기.
outputs <- embedded %>%
  bidirectional(layer_lstm(units = 32)) %>%
  layer_dropout(0.5) %>%
  layer_dense(1, activation = "sigmoid")

# 모형 설정하기.
model <- keras_model(inputs, outputs)

# 모형 컴파일링하기.
model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = "accuracy")
model

# 콜백 설정하기.
callbacks <- list(
  callback_model_checkpoint("fasttext_embeddings_sequence_model.keras",
                            save_best_only = TRUE)
)

# 모형 학습하기.
history_ft <- model %>% keras::fit(
  x=shopping_train_pad, y=shopping_train.1.tokenize.label.vector %>% as_vector(),
  validation_split = 0.2,
  epochs = 2,
  callbacks = callbacks
)

# 가장 정확도 높은 모형 띄우기.
model <- load_model_tf("fasttext_embeddings_sequence_model.keras")

# 검증 정확도 확인하기.
cat(sprintf("검증 정확도: %.3f\n", 
            evaluate(model, 
                     x=shopping_test_pad, 
                     y=shopping_test.1.tokenize.label.vector %>% as_vector())["accuracy"])) # 0.894

## 3. 파이썬에서 FastText 임베딩 사용하여 비단어 임베딩 벡터 구하고 유사단어 찾기.
# 1) 파이썬에서 fasttext 패키지를 불러오세요.
# 2) 모형은 "wiki.ko.bin"를 사용해주세요.
# 3) "항지", "정굴", "판밀" 등 세 비단어의 임베딩 벡터를 구해주세요.
# 4) 세 비단어의 유사단어를 찾아주세요.

#  파이썬에서 FastText 패키지를 사용하여 모르는 한국어 단어의 유사단어 찾기.
ko_model = fasttext.load_facebook_model('E:/연구용 텍스트 자료/FastText/bin_vector/wiki.ko.bin') # 기존 한국어 위키백과로 학습한 모형 불러오기.
b = ko_model.wv.most_similar("판밀", topn=5) # "판밀"과 가장 유사한 5개 단어 찾기.
print(b)
c = ko_model.wv.get_vector("항지") # "항지"의 벡터 구하기.
print(c)

nonword = ["항지", "정굴", "판밀"] # 3개의 비단어 벡터.
for i in range(len(nonword)):
  b = ko_model.wv.most_similar(nonword[i], topn=5) # 3개의 비단어와 각각 가장 유사한 5개 단어 찾기.
print(b)