library(readr)
library(ggwordcloud)
library(stringr)
library(tidytext)
library(KoNLP)
library(dplyr)
library(textclean)
library(ldatuning)
library(tidygraph)
library(tidyr)
library(widyr)

install.packages("tidygraph")
#csv 불러오기
raw_tada <- read_csv("C:/Users/lys74/Desktop/NLP/tada.csv")%>%
    mutate(id = row_number())
#전처리
tada <- raw_tada %>%
    filter(str_count(reply, " ") >= 1) %>% # 띄어쓰기 1개 이상 추출
    mutate(reply_raw = str_squish(replace_html(reply)), # 원문 보유
           reply = str_replace_all(reply, "[^가-힣]", " "), # 한글만 남기기
           reply = str_squish(reply))
#명사추출
word_noun <- tada %>%
    unnest_tokens(input = reply,
                  output = word,
                  token = extractNoun,
                  drop = F)
# 단어 빈도 구하기
frequency <- word_noun %>%
    count(word, sort = T) %>% # 단어 빈도 구해 내림차순 정렬
    filter(str_count(word) > 1) # 두 글자 이상만 남기기
# 상위 단어 추출
frequency <- frequency %>% head(50)
#워드클라우드
ggplot(frequency, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234) +
    scale_radius(limits = c(1, NA), range = c(3, 15)) +
    scale_color_gradient(low = "#31609e", high = "#b52155") +
    theme_minimal()


# 토큰화
pos_tada <- tada %>%
    unnest_tokens(input = reply, output = word_pos,
                  token = SimplePos22, drop = F)
#댓글에서 명사, 동사, 형용사를 추출하고 “/로 시작하는 모든 문자” 를 '다'로 바꾸기
separate_pos_tada <- pos_tada %>%
    separate_rows(word_pos, sep = "[+]") %>% # 품사 태그 분리
    filter(str_detect(word_pos, "/n|/pv|/pa")) %>% # 품사 추출
    mutate(word = ifelse( str_detect(word_pos, "/pv|/pa"), # "/pv", "/pa" 추출
                          str_replace(word_pos, "/.*$", "다"), # "~다"로 바꾸기
                          str_remove(word_pos, "/.*$"))) %>% # 태그 제거
    filter(str_count(word) >= 2) %>% # 2글자 이상 추출
    arrange(id)
separate_pos_tada %>% select(word)

#댓글을 바이그램으로 토큰화한 다음 바이그램 단어쌍을 분리
#(한 댓글이 하나의 행을 구성하도록 결합)
line_comment <- separate_pos_tada %>%
    group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
#(바이그램으로 토큰화)
bigram_comment <- line_comment %>%
    unnest_tokens( input = sentence, output = bigram, token = "ngrams", n = 2)
#(바이그램 분리하기)
bigram_seprated <- bigram_comment %>%
    separate(bigram, c("word1", "word2"), sep = " ")
#(단어쌍 빈도 구하기)
pair_bigram <- bigram_seprated %>%
    count(word1, word2, sort = T) %>% na.omit()

#단어쌍 빈도를 구한 다음 네트워크 그래프 데이터 만들기
#난수 고정, 빈도가 10 이상인 단어쌍만 사용, 연결 중심성과 커뮤니티를 나타낸 변수 추가
set.seed(1234)
graph_bigram <- pair_bigram %>%
    filter(n >= 10) %>%
    as_tbl_graph(directed = F) %>%
    mutate( centrality = centrality_degree(), # 중심성
            group = as.factor(group_infomap())) # 커뮤니티


install.packages("ggraph")
library(ggraph)

#바이그램을 이용해 네트워크 그래프 만들기
# 레이아웃을 "fr"로 설정
# 연결 중심성에 따라 노드 크기를 정하고,
# 커뮤니티별로 노드 색깔이 다르게 설정,
# 노드의 범례 삭제
# 텍스트가 노드 밖에 표시되게 설정하고, 텍스트의 크기를 5로 설정
# edge 색상 = "blue", edge alpha = 0.5
set.seed(1234)
ggraph(graph_bigram, layout = "fr") + # 레이아웃
    geom_edge_link(color = "blue", # 엣지 색깔
                   alpha = 0.5) + # 엣지 명암
    geom_node_point(aes( size = centrality, # 노드 크기
                         color = group), # 노드 색깔
                    show.legend = F) + # 범례 삭제
    scale_size(range = c(5, 15)) + # 노드 크기 범위
    geom_node_text(aes(label = name), # 텍스트 표시
                   repel = T, # 노드밖 표시
                   size = 5, # 텍스트 크기
                   family = "nanumgothic") + # 폰트
    theme_graph() # 배경 삭제

#reply 에서 명사 추출(단어의 길이 2 이상)
noun_tada <- tada %>% distinct(reply, .keep_all = T) %>% # 중복 댓글 제거
    filter(str_count(reply, boundary("word")) >= 3) %>% # 짧은 댓글 제거
    unnest_tokens(input = reply, output = word, token = extractNoun, drop = F) # 명사 추출
%>% filter(str_count(word) > 1)

#중복 단어 제거하고 빈도가 100 회 이하 단어 추출
unique_noun_tada <- noun_tada %>%
    group_by(id) %>% # 중복 단어 제거
    distinct(word, .keep_all = T) %>%
    ungroup() %>%
    add_count(word) %>% # 빈도 100회 이하 단어만
    filter(n <= 100) %>% select(id, word)

#댓글별 단어 빈도 구한 후
count_word <- unique_noun_tada %>%
    count(id, word, sort = T)

# DTM 만들기
dtm_tada <- count_word %>%
    cast_dtm(document = id, term = word, value = n)

#토픽 수를 2~20 개로 바꿔가며 LDA 모델을 만든 후
models_tada <- FindTopicsNumber(dtm = dtm_tada,
                                topics = 2:20,
                                return_models = T,
                                control = list(seed = 1234))
# 성능 지표 그래프
FindTopicsNumber_plot(models_tada)

# 토픽 수가 8개인 모델 추출
lda_model <- models_tada %>%
    filter(topics == 8) %>%
    pull(LDA_model) %>% # 모델 추출
    .[[1]] # list 추출
#LDA 모델의 beta 를 이용해 각 토픽에 등장할 확률이 높은 상위 10 개 단어를 추출
term_topic <- tidy(lda_model, matrix = "beta")
term_topic %>% 
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% print(n=10)

#토픽별 주요 단어를 나타낸 막대 그래프 만들기(아래 그림, 그래프 제목 추가)
ggplot(term_topic, aes( x = reorder_within(term, beta, topic),
                            y = beta,
                            fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free", ncol = 4) +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))
