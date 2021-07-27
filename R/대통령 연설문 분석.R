library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(KoNLP)
library(textclean)
library(ldatuning)
library(scales)
library(ggplot2)
library(topicmodels)

raw_roh <- read_csv("C:/Users/lys74/Desktop/NLP/speeches_roh.csv")
glimpse(raw_roh)

roh <- raw_roh %>%
    mutate(content = str_replace_all(content, "[^∞°-∆R]", " "), # «—±€∏∏ ≥≤±‚±‚
           content = str_squish(content)) #¡ﬂ∫π∞¯πÈ ¡¶∞≈

#∏ÌªÁ √ﬂ√‚
word_noun <- roh %>%
    unnest_tokens(input = content,
                  output = word,
                  token = extractNoun,
                  drop = F) %>%
    filter(str_count(word) > 1) %>%
    
    # ø¨º≥πÆ ≥ª ¡ﬂ∫π ¥‹æÓ ¡¶∞≈
    group_by(id) %>%
    distinct(word, .keep_all = T) %>%
    ungroup() %>%
    select(id, word)

#¥‹æÓ ∫Ûµµ ±∏«œ±‚
count_word <- word_noun %>%
    add_count(word) %>%
    #∫Ûµµ 100»∏ ¿Ã«œ ¥‹æÓ √ﬂ√‚
    filter(n <= 100) %>%
    select(-n)
count_word

#¿”¿«¿« ∫“øÎæÓ ∏∏µÈ±‚
stopword <- c("µÈ¿Ã", "«œ¥Ÿ", "«œ∞‘", "«œ∏È", "«ÿº≠", "¿Ãπ¯", "«œ≥◊",
              "«ÿø‰", "¿Ã∞Õ", "¥œµÈ", "«œ±‚", "«œ¡ˆ", "«—∞≈", "«ÿ¡÷",
              "±◊∞Õ", "æÓµ", "ø©±‚", "±Ó¡ˆ", "¿Ã∞≈", "«œΩ≈", "∏∏≈≠")

#∫“øÎæÓ ¡¶∞≈
count_word <- count_word %>%
    filter(!word %in% stopword)

#ø¨º≥πÆ∫∞ ∫Ûµµ ±∏«œ±‚
count_word_doc <- count_word %>%
    count(id, word, sort = T)
count_word_doc

# DTM ∏∏µÈ±‚
dtm_comment <- count_word_doc %>%
    cast_dtm(document = id, term = word, value = n)
dtm_comment
as.matrix(dtm_comment[1:8, 1:8])

#√÷¿˚¿« ≈‰«» ºˆ º±¡§
models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))
models %>%select(topics, Griffiths2004)
FindTopicsNumber_plot(models)

# ≈‰«» ∏µ® √ﬂ√‚(≈‰«»ºˆ: 9∞≥)
optimal_model <- models %>%
    filter(topics == 9) %>%
    pull(LDA_model) %>% # ∏µ® √ﬂ√‚
    .[[1]] # list √ﬂ√‚
term_topic <- tidy(optimal_model, matrix = "beta")

# ≈‰«»∫∞ beta ªÛ¿ß 10∞≥ ¥‹æÓ √ﬂ√‚
top_term_topic <- term_topic %>%
    group_by(topic) %>%
    slice_max(beta, n = 10)
top_term_topic

#≈‰«»∫∞ ªÛ¿ß ¥‹æÓ ∏∑¥Î ±◊∏Æ«¡
ggplot(top_term_topic,
       aes( x = reorder_within(term, beta, topic),
            y = beta,
            fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free", ncol = 3) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(n.breaks = 4, #√‡ ¥´±›¿ª 4∞≥ ≥ªø‹∑Œ ¡§«œ±‚
                       labels = number_format(accuracy = .01)) +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))

doc_topic <- tidy(optimal_model, matrix = "gamma")
doc_topic

doc_topic %>% count(topic)

# πÆº≠∫∞∑Œ »Æ∑¸¿Ã ∞°¿Â ≥Ù¿∫ ≈‰«» √ﬂ√‚
doc_class <- doc_topic %>%
    group_by(document) %>%
    slice_max(gamma, n = 1)
doc_class

# integer∑Œ ∫Ø»Ø
doc_class$document <- as.integer(doc_class$document)
# µ•¿Ã≈Õº¬ ∞·«’«œ±‚ ¿ß«ÿ ±‚¡ÿ ∫Øºˆ ≈∏¿‘¿ª integer∑Œ ≈Î¿œ
# ø¯πÆø° ≈‰«» π¯»£ ∫Œø©
roh_topic <- raw_roh %>%
    left_join(doc_class, by = c("id" = "document"))
# ∞·«’ »Æ¿Œ
roh_topic %>%
    select(id, topic)

#≈‰«»∫∞ ¡÷ø‰ ¥‹æÓ ∏Ò∑œ
top_terms <- term_topic %>%
    group_by(topic) %>%
    slice_max(beta, n = 6, with_ties = F) %>%
    summarise(term = paste(term, collapse = ", "))
top_terms

#≈‰«»∫∞ πÆº≠ ∫Ûµµ
count_topic <- roh_topic %>%
    count(topic)
count_topic

#πÆº≠ ∫Ûµµø° ¡÷ø‰ ¥‹æÓ ∞·«’
count_topic_word <- count_topic %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic_name = paste("Topic", topic))
count_topic_word

#≈‰«»∫∞ πÆº≠ ºˆøÕ ¡÷ø‰ ¥‹æÓ ∏∑¥Î ±◊∑°«¡
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n), y = n, fill = topic_name)) +
    geom_col(show.legend = F) +
    coord_flip() +
    geom_text(aes(label = n) , # πÆº≠ ∫Ûµµ «•Ω√
              hjust = -0.2) + # ∏∑¥Î π€ø° «•Ω√
    geom_text(aes(label = term), # ¡÷ø‰ ¥‹æÓ «•Ω√
              hjust = 1.03, # ∏∑¥Î æ»ø° «•Ω√
              col = "white", # ªˆ±Ú
              fontface = "bold", # µŒ≤Æ∞‘
              family = "nanumgothic") + # ∆˘∆Æ
    scale_y_continuous( expand = c(0, 0), # y√‡-∏∑¥Î ∞£∞› ¡Ÿ¿Ã±‚
                        limits = c(0, 180)) + # y√‡ π¸¿ß
    labs(x = NULL)