library(stringr)
library(tidytext)
library(KoNLP)
library(dplyr)
library(textclean)
library(tidyr)
library(ggplot2)
library(readr)
dic <- read_csv("C:/Users/lys74/Desktop/NLP/knu_sentiment_lexicon.csv")

## �Ҽ� �ҷ��� �� ��ó��

#����
raw_bombom <- readLines("C:/Users/lys74/Desktop/NLP/����.txt", encoding = "UTF-8")
bombom <- raw_bombom %>%
    str_replace_all("[^��-�R]", " ") %>% # �ѱ۸� �����
    str_squish() %>%
    as_tibble() %>%
    mutate(book="bombom")
#��� ���� ��
raw_luckyday <- readLines("C:/Users/lys74/Desktop/NLP/���������.txt", encoding = "UTF-8")
luckyday <- raw_luckyday %>%
    str_replace_all("[^��-�R]", " ") %>% # �ѱ۸� �����
    str_squish() %>%
    as_tibble() %>%
    mutate(book="luckyday")
#�޹� �� �� ����
raw_buckwheat <- readLines("C:/Users/lys74/Desktop/NLP/��в��ʹ���.txt", encoding = "UTF-8")
buckwheat <- raw_buckwheat %>%
    str_replace_all("[^��-�R]", " ") %>% # �ѱ۸� �����
    str_squish() %>%
    as_tibble() %>%
    mutate(book="buckwheat")

## ����&���� �������� ��ūȭ�ϰ� �������� ���ϱ�

#���� �������� ��ūȭ
sentences_bombom <- raw_bombom %>% 
    str_squish() %>%   # ���ӵ� ���� ����
    as_tibble() %>%    # tibble�� ��ȯ
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

sentences_luckyday <- raw_luckyday %>% 
    str_squish() %>%   # ���ӵ� ���� ����
    as_tibble() %>%    # tibble�� ��ȯ
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

sentences_buckwheat <- raw_buckwheat %>% 
    str_squish() %>%   # ���ӵ� ���� ����
    as_tibble() %>%    # tibble�� ��ȯ
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

#����� ���⸦ �������� ��ūȭ
space_bombom <- sentences_bombom %>% 
    unnest_tokens(input = sentence, output = word, token = "words", drop = F)
space_bombom <- space_bombom %>% filter(str_count(word) > 1) #�� ���� �ܾ� ����
space_bombom <- space_bombom %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity)) #���� ���� �ο�

space_luckyday <- sentences_luckyday %>% 
    unnest_tokens(input = sentence, output = word, token = "words", drop = F)
space_luckyday <- space_luckyday %>% filter(str_count(word) > 1) #�� ���� �ܾ� ����
space_luckyday <- space_luckyday %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity)) #���� ���� �ο�

space_buckwheat <- sentences_buckwheat %>% 
    unnest_tokens(input = sentence, output = word, token = "words", drop = F)
space_buckwheat <- space_buckwheat %>% filter(str_count(word) > 1) #�� ���� �ܾ� ����
space_buckwheat <- space_buckwheat %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity)) #���� ���� �ο�

#���庰 ���� ���� �ջ�
score_bombom <- space_bombom %>%
    group_by(sentence) %>%
    summarise(score = sum(polarity))

score_luckyday <- space_luckyday %>%
    group_by(sentence) %>%
    summarise(score = sum(polarity))

score_buckwheat <- space_buckwheat %>%
    group_by(sentence) %>%
    summarise(score = sum(polarity))

## ���� ���ֺ� �ܾ� �� ���ϰ� ����׷��� �׸���

#����,����,�߸� �з��ϱ�
score_bombom <- score_bombom %>%
    mutate( sentiment = ifelse(score >= 1, "pos",
                               ifelse(score <= -1, "neg", "neu")))
score_luckyday <- score_luckyday %>%
    mutate( sentiment = ifelse(score >= 1, "pos",
                               ifelse(score <= -1, "neg", "neu")))
score_buckwheat <- score_buckwheat %>%
    mutate( sentiment = ifelse(score >= 1, "pos",
                               ifelse(score <= -1, "neg", "neu")))

score_word_bombom <- score_bombom %>%
    unnest_tokens( input = sentence, output = word, token = "words", # �ܾ� ���� ��ūȭ
                   drop = F) %>%
    filter( str_detect(word, "[��-�R]") & # �ѱ� ����
                str_count(word) >= 2) # �� ���� �̻� ����
score_word_luckyday <- score_luckyday %>%
    unnest_tokens( input = sentence, output = word, token = "words", # �ܾ� ���� ��ūȭ
                   drop = F) %>%
    filter( str_detect(word, "[��-�R]") & # �ѱ� ����
                str_count(word) >= 2) # �� ���� �̻� ����
score_word_buckwheat <- score_buckwheat %>%
    unnest_tokens( input = sentence, output = word, token = "words", # �ܾ� ���� ��ūȭ
                   drop = F) %>%
    filter( str_detect(word, "[��-�R]") & # �ѱ� ����
                str_count(word) >= 2) # �� ���� �̻� ����

#���ֺ� �ܾ� ��
frequency_word_bombom <- score_word_bombom %>%
    count(sentiment, word, sort = T)
frequency_word_luckyday <- score_word_luckyday %>%
    count(sentiment, word, sort = T)
frequency_word_buckwheat <- score_word_buckwheat %>%
    count(sentiment, word, sort = T)

#�α� �����
bombom_wide <- frequency_word_bombom %>%
    filter(sentiment != "neu") %>%
    pivot_wider(names_from = sentiment,
                values_from = n, values_fill = list(n = 0))
bombom_wide <- bombom_wide %>%
    mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                    ((neg + 1) / (sum(neg + 1)))))
luckyday_wide <- frequency_word_luckyday %>%
    filter(sentiment != "neu") %>%
    pivot_wider(names_from = sentiment,
                values_from = n, values_fill = list(n = 0))
luckyday_wide <- luckyday_wide %>%
    mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                    ((neg + 1) / (sum(neg + 1)))))
buckwheat_wide <- frequency_word_buckwheat %>%
    filter(sentiment != "neu") %>%
    pivot_wider(names_from = sentiment,
                values_from = n, values_fill = list(n = 0))
buckwheat_wide <- buckwheat_wide %>%
    mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                    ((neg + 1) / (sum(neg + 1)))))
#����,���� top 10
top10_bombom <- bombom_wide %>%
    group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F) #�α� ����� ���� �ܾ� ����
top10_luckyday <- luckyday_wide %>%
    group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F) #�α� ����� ���� �ܾ� ����
top10_buckwheat <- buckwheat_wide %>%
    group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F) #�α� ����� ���� �ܾ� ����


#���� �׷���
ggplot(top10_bombom, aes( x = reorder(word, log_odds_ratio),
                   y = log_odds_ratio,
                   fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))

ggplot(top10_luckyday, aes( x = reorder(word, log_odds_ratio),
                          y = log_odds_ratio,
                          fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))

ggplot(top10_buckwheat, aes( x = reorder(word, log_odds_ratio),
                          y = log_odds_ratio,
                          fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))

## TF-IDF ���� �׷��� �׸���

#3���� �Ҽ� ���ο� ������ ��ġ��
bind_books <- bind_rows(bombom,luckyday,buckwheat) %>% select(book, value)
bind_books <- bind_books %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun)

# �ܾ� �� ���ϱ�
book_frequency <- bind_books %>%
    count(book, word) %>%
    filter(str_count(word) > 1)

book_frequency <- book_frequency %>%
    bind_tf_idf( term = word, # �ܾ�
                 document = book, # �ؽ�Ʈ ���� ����
                 n = n) %>% # �ܾ� ��
    arrange(-tf_idf)

# �ֿ� �ܾ� ����
top10_book <- book_frequency %>%
    group_by(book) %>%
    slice_max(tf_idf, n = 10, with_ties = F)

# �׷��� ���� ���ϱ�
top10_book$book <- factor(top10_book$book,
                          levels = c("����", "���������", "�޹в��ʹ���"))
# ���� �׷��� �����
ggplot(top10_book, aes( x = reorder_within(word, tf_idf, book),
                   y = tf_idf, fill = book)) +
    geom_col(show.legend = F) +
    coord_flip() + facet_wrap(~ book, scales = "free", ncol = 3) +
    scale_x_reordered() + labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))
