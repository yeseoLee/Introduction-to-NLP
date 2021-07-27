library(stringr)
library(tidytext)
library(KoNLP)
library(dplyr)
library(textclean)
library(tidyr)
library(ggplot2)
library(readr)
dic <- read_csv("C:/Users/lys74/Desktop/NLP/knu_sentiment_lexicon.csv")

## ¼Ò¼³ ºÒ·¯¿Â ÈÄ ÀüÃ³¸®

#º½º½
raw_bombom <- readLines("C:/Users/lys74/Desktop/NLP/º½º½.txt", encoding = "UTF-8")
bombom <- raw_bombom %>%
    str_replace_all("[^°¡-ÆR]", " ") %>% # ÇÑ±Û¸¸ ³²±â±â
    str_squish() %>%
    as_tibble() %>%
    mutate(book="bombom")
#¿î¼ö ÁÁÀº ³¯
raw_luckyday <- readLines("C:/Users/lys74/Desktop/NLP/¿î¼öÁÁÀº³¯.txt", encoding = "UTF-8")
luckyday <- raw_luckyday %>%
    str_replace_all("[^°¡-ÆR]", " ") %>% # ÇÑ±Û¸¸ ³²±â±â
    str_squish() %>%
    as_tibble() %>%
    mutate(book="luckyday")
#¸Þ¹Ð ²É ÇÊ ¹«·Æ
raw_buckwheat <- readLines("C:/Users/lys74/Desktop/NLP/¸ð¹Ð²ÉÇÊ¹«·Æ.txt", encoding = "UTF-8")
buckwheat <- raw_buckwheat %>%
    str_replace_all("[^°¡-ÆR]", " ") %>% # ÇÑ±Û¸¸ ³²±â±â
    str_squish() %>%
    as_tibble() %>%
    mutate(book="buckwheat")

## ¹®Àå&¶ç¾î¾²±â ±âÁØÀ¸·Î ÅäÅ«È­ÇÏ°í °¨Á¤Á¡¼ö ±¸ÇÏ±â

#¹®Àå ±âÁØÀ¸·Î ÅäÅ«È­
sentences_bombom <- raw_bombom %>% 
    str_squish() %>%   # ¿¬¼ÓµÈ °ø¹é Á¦°Å
    as_tibble() %>%    # tibble·Î º¯È¯
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

sentences_luckyday <- raw_luckyday %>% 
    str_squish() %>%   # ¿¬¼ÓµÈ °ø¹é Á¦°Å
    as_tibble() %>%    # tibble·Î º¯È¯
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

sentences_buckwheat <- raw_buckwheat %>% 
    str_squish() %>%   # ¿¬¼ÓµÈ °ø¹é Á¦°Å
    as_tibble() %>%    # tibble·Î º¯È¯
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

#¹®Àå°ú ¶ç¾î¾²±â¸¦ ±âÁØÀ¸·Î ÅäÅ«È­
space_bombom <- sentences_bombom %>% 
    unnest_tokens(input = sentence, output = word, token = "words", drop = F)
space_bombom <- space_bombom %>% filter(str_count(word) > 1) #ÇÑ ±ÛÀÚ ´Ü¾î Á¦°Å
space_bombom <- space_bombom %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity)) #°¨Á¤ Á¡¼ö ºÎ¿©

space_luckyday <- sentences_luckyday %>% 
    unnest_tokens(input = sentence, output = word, token = "words", drop = F)
space_luckyday <- space_luckyday %>% filter(str_count(word) > 1) #ÇÑ ±ÛÀÚ ´Ü¾î Á¦°Å
space_luckyday <- space_luckyday %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity)) #°¨Á¤ Á¡¼ö ºÎ¿©

space_buckwheat <- sentences_buckwheat %>% 
    unnest_tokens(input = sentence, output = word, token = "words", drop = F)
space_buckwheat <- space_buckwheat %>% filter(str_count(word) > 1) #ÇÑ ±ÛÀÚ ´Ü¾î Á¦°Å
space_buckwheat <- space_buckwheat %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity)) #°¨Á¤ Á¡¼ö ºÎ¿©

#¹®Àåº° °¨Á¤ Á¡¼ö ÇÕ»ê
score_bombom <- space_bombom %>%
    group_by(sentence) %>%
    summarise(score = sum(polarity))

score_luckyday <- space_luckyday %>%
    group_by(sentence) %>%
    summarise(score = sum(polarity))

score_buckwheat <- space_buckwheat %>%
    group_by(sentence) %>%
    summarise(score = sum(polarity))

## °¨Á¤ ¹üÁÖº° ´Ü¾î ºóµµ ±¸ÇÏ°í ¸·´ë±×·¡ÇÁ ±×¸®±â

#±àÁ¤,ºÎÁ¤,Áß¸³ ºÐ·ùÇÏ±â
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
    unnest_tokens( input = sentence, output = word, token = "words", # ´Ü¾î ±âÁØ ÅäÅ«È­
                   drop = F) %>%
    filter( str_detect(word, "[°¡-ÆR]") & # ÇÑ±Û ÃßÃâ
                str_count(word) >= 2) # µÎ ±ÛÀÚ ÀÌ»ó ÃßÃâ
score_word_luckyday <- score_luckyday %>%
    unnest_tokens( input = sentence, output = word, token = "words", # ´Ü¾î ±âÁØ ÅäÅ«È­
                   drop = F) %>%
    filter( str_detect(word, "[°¡-ÆR]") & # ÇÑ±Û ÃßÃâ
                str_count(word) >= 2) # µÎ ±ÛÀÚ ÀÌ»ó ÃßÃâ
score_word_buckwheat <- score_buckwheat %>%
    unnest_tokens( input = sentence, output = word, token = "words", # ´Ü¾î ±âÁØ ÅäÅ«È­
                   drop = F) %>%
    filter( str_detect(word, "[°¡-ÆR]") & # ÇÑ±Û ÃßÃâ
                str_count(word) >= 2) # µÎ ±ÛÀÚ ÀÌ»ó ÃßÃâ

#¹üÁÖº° ´Ü¾î ºóµµ
frequency_word_bombom <- score_word_bombom %>%
    count(sentiment, word, sort = T)
frequency_word_luckyday <- score_word_luckyday %>%
    count(sentiment, word, sort = T)
frequency_word_buckwheat <- score_word_buckwheat %>%
    count(sentiment, word, sort = T)

#·Î±× ¿ÀÁîºñ
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
#±àÁ¤,ºÎÁ¤ top 10
top10_bombom <- bombom_wide %>%
    group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F) #·Î±× ¿ÀÁîºñ µ¿Á¡ ´Ü¾î Á¦¿Ü
top10_luckyday <- luckyday_wide %>%
    group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F) #·Î±× ¿ÀÁîºñ µ¿Á¡ ´Ü¾î Á¦¿Ü
top10_buckwheat <- buckwheat_wide %>%
    group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
    slice_max(abs(log_odds_ratio), n = 10, with_ties = F) #·Î±× ¿ÀÁîºñ µ¿Á¡ ´Ü¾î Á¦¿Ü


#¸·´ë ±×·¡ÇÁ
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

## TF-IDF ¸·´ë ±×·¡ÇÁ ±×¸®±â

#3°¡Áö ¼Ò¼³ »õ·Î¿î º¯¼ö¿¡ ÇÕÄ¡±â
bind_books <- bind_rows(bombom,luckyday,buckwheat) %>% select(book, value)
bind_books <- bind_books %>%
    unnest_tokens(input = value,
                  output = word,
                  token = extractNoun)

# ´Ü¾î ºóµµ ±¸ÇÏ±â
book_frequency <- bind_books %>%
    count(book, word) %>%
    filter(str_count(word) > 1)

book_frequency <- book_frequency %>%
    bind_tf_idf( term = word, # ´Ü¾î
                 document = book, # ÅØ½ºÆ® ±¸ºÐ ±âÁØ
                 n = n) %>% # ´Ü¾î ºóµµ
    arrange(-tf_idf)

# ÁÖ¿ä ´Ü¾î ÃßÃâ
top10_book <- book_frequency %>%
    group_by(book) %>%
    slice_max(tf_idf, n = 10, with_ties = F)

# ±×·¡ÇÁ ¼ø¼­ Á¤ÇÏ±â
top10_book$book <- factor(top10_book$book,
                          levels = c("º½º½", "¿î¼öÁÁÀº³¯", "¸Þ¹Ð²ÉÇÊ¹«·Æ"))
# ¸·´ë ±×·¡ÇÁ ¸¸µé±â
ggplot(top10_book, aes( x = reorder_within(word, tf_idf, book),
                   y = tf_idf, fill = book)) +
    geom_col(show.legend = F) +
    coord_flip() + facet_wrap(~ book, scales = "free", ncol = 3) +
    scale_x_reordered() + labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))
