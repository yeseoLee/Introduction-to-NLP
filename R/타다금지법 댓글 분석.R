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
#csv ºÒ·¯¿À±â
raw_tada <- read_csv("C:/Users/lys74/Desktop/NLP/tada.csv")%>%
    mutate(id = row_number())
#ÀüÃ³¸®
tada <- raw_tada %>%
    filter(str_count(reply, " ") >= 1) %>% # ¶ç¾î¾²±â 1°³ ÀÌ»ó ÃßÃâ
    mutate(reply_raw = str_squish(replace_html(reply)), # ¿ø¹® º¸À¯
           reply = str_replace_all(reply, "[^°¡-ÆR]", " "), # ÇÑ±Û¸¸ ³²±â±â
           reply = str_squish(reply))
#¸í»çÃßÃâ
word_noun <- tada %>%
    unnest_tokens(input = reply,
                  output = word,
                  token = extractNoun,
                  drop = F)
# ´Ü¾î ºóµµ ±¸ÇÏ±â
frequency <- word_noun %>%
    count(word, sort = T) %>% # ´Ü¾î ºóµµ ±¸ÇØ ³»¸²Â÷¼ø Á¤·Ä
    filter(str_count(word) > 1) # µÎ ±ÛÀÚ ÀÌ»ó¸¸ ³²±â±â
# »óÀ§ ´Ü¾î ÃßÃâ
frequency <- frequency %>% head(50)
#¿öµåÅ¬¶ó¿ìµå
ggplot(frequency, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234) +
    scale_radius(limits = c(1, NA), range = c(3, 15)) +
    scale_color_gradient(low = "#31609e", high = "#b52155") +
    theme_minimal()


# ÅäÅ«È­
pos_tada <- tada %>%
    unnest_tokens(input = reply, output = word_pos,
                  token = SimplePos22, drop = F)
#´ñ±Û¿¡¼­ ¸í»ç, µ¿»ç, Çü¿ë»ç¸¦ ÃßÃâÇÏ°í ¡°/·Î ½ÃÀÛÇÏ´Â ¸ğµç ¹®ÀÚ¡± ¸¦ '´Ù'·Î ¹Ù²Ù±â
separate_pos_tada <- pos_tada %>%
    separate_rows(word_pos, sep = "[+]") %>% # Ç°»ç ÅÂ±× ºĞ¸®
    filter(str_detect(word_pos, "/n|/pv|/pa")) %>% # Ç°»ç ÃßÃâ
    mutate(word = ifelse( str_detect(word_pos, "/pv|/pa"), # "/pv", "/pa" ÃßÃâ
                          str_replace(word_pos, "/.*$", "´Ù"), # "~´Ù"·Î ¹Ù²Ù±â
                          str_remove(word_pos, "/.*$"))) %>% # ÅÂ±× Á¦°Å
    filter(str_count(word) >= 2) %>% # 2±ÛÀÚ ÀÌ»ó ÃßÃâ
    arrange(id)
separate_pos_tada %>% select(word)

#´ñ±ÛÀ» ¹ÙÀÌ±×·¥À¸·Î ÅäÅ«È­ÇÑ ´ÙÀ½ ¹ÙÀÌ±×·¥ ´Ü¾î½ÖÀ» ºĞ¸®
#(ÇÑ ´ñ±ÛÀÌ ÇÏ³ªÀÇ ÇàÀ» ±¸¼ºÇÏµµ·Ï °áÇÕ)
line_comment <- separate_pos_tada %>%
    group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
#(¹ÙÀÌ±×·¥À¸·Î ÅäÅ«È­)
bigram_comment <- line_comment %>%
    unnest_tokens( input = sentence, output = bigram, token = "ngrams", n = 2)
#(¹ÙÀÌ±×·¥ ºĞ¸®ÇÏ±â)
bigram_seprated <- bigram_comment %>%
    separate(bigram, c("word1", "word2"), sep = " ")
#(´Ü¾î½Ö ºóµµ ±¸ÇÏ±â)
pair_bigram <- bigram_seprated %>%
    count(word1, word2, sort = T) %>% na.omit()

#´Ü¾î½Ö ºóµµ¸¦ ±¸ÇÑ ´ÙÀ½ ³×Æ®¿öÅ© ±×·¡ÇÁ µ¥ÀÌÅÍ ¸¸µé±â
#³­¼ö °íÁ¤, ºóµµ°¡ 10 ÀÌ»óÀÎ ´Ü¾î½Ö¸¸ »ç¿ë, ¿¬°á Áß½É¼º°ú Ä¿¹Â´ÏÆ¼¸¦ ³ªÅ¸³½ º¯¼ö Ãß°¡
set.seed(1234)
graph_bigram <- pair_bigram %>%
    filter(n >= 10) %>%
    as_tbl_graph(directed = F) %>%
    mutate( centrality = centrality_degree(), # Áß½É¼º
            group = as.factor(group_infomap())) # Ä¿¹Â´ÏÆ¼


install.packages("ggraph")
library(ggraph)

#¹ÙÀÌ±×·¥À» ÀÌ¿ëÇØ ³×Æ®¿öÅ© ±×·¡ÇÁ ¸¸µé±â
# ·¹ÀÌ¾Æ¿ôÀ» "fr"·Î ¼³Á¤
# ¿¬°á Áß½É¼º¿¡ µû¶ó ³ëµå Å©±â¸¦ Á¤ÇÏ°í,
# Ä¿¹Â´ÏÆ¼º°·Î ³ëµå »ö±òÀÌ ´Ù¸£°Ô ¼³Á¤,
# ³ëµåÀÇ ¹ü·Ê »èÁ¦
# ÅØ½ºÆ®°¡ ³ëµå ¹Û¿¡ Ç¥½ÃµÇ°Ô ¼³Á¤ÇÏ°í, ÅØ½ºÆ®ÀÇ Å©±â¸¦ 5·Î ¼³Á¤
# edge »ö»ó = "blue", edge alpha = 0.5
set.seed(1234)
ggraph(graph_bigram, layout = "fr") + # ·¹ÀÌ¾Æ¿ô
    geom_edge_link(color = "blue", # ¿§Áö »ö±ò
                   alpha = 0.5) + # ¿§Áö ¸í¾Ï
    geom_node_point(aes( size = centrality, # ³ëµå Å©±â
                         color = group), # ³ëµå »ö±ò
                    show.legend = F) + # ¹ü·Ê »èÁ¦
    scale_size(range = c(5, 15)) + # ³ëµå Å©±â ¹üÀ§
    geom_node_text(aes(label = name), # ÅØ½ºÆ® Ç¥½Ã
                   repel = T, # ³ëµå¹Û Ç¥½Ã
                   size = 5, # ÅØ½ºÆ® Å©±â
                   family = "nanumgothic") + # ÆùÆ®
    theme_graph() # ¹è°æ »èÁ¦

#reply ¿¡¼­ ¸í»ç ÃßÃâ(´Ü¾îÀÇ ±æÀÌ 2 ÀÌ»ó)
noun_tada <- tada %>% distinct(reply, .keep_all = T) %>% # Áßº¹ ´ñ±Û Á¦°Å
    filter(str_count(reply, boundary("word")) >= 3) %>% # ÂªÀº ´ñ±Û Á¦°Å
    unnest_tokens(input = reply, output = word, token = extractNoun, drop = F) # ¸í»ç ÃßÃâ
%>% filter(str_count(word) > 1)

#Áßº¹ ´Ü¾î Á¦°ÅÇÏ°í ºóµµ°¡ 100 È¸ ÀÌÇÏ ´Ü¾î ÃßÃâ
unique_noun_tada <- noun_tada %>%
    group_by(id) %>% # Áßº¹ ´Ü¾î Á¦°Å
    distinct(word, .keep_all = T) %>%
    ungroup() %>%
    add_count(word) %>% # ºóµµ 100È¸ ÀÌÇÏ ´Ü¾î¸¸
    filter(n <= 100) %>% select(id, word)

#´ñ±Ûº° ´Ü¾î ºóµµ ±¸ÇÑ ÈÄ
count_word <- unique_noun_tada %>%
    count(id, word, sort = T)

# DTM ¸¸µé±â
dtm_tada <- count_word %>%
    cast_dtm(document = id, term = word, value = n)

#ÅäÇÈ ¼ö¸¦ 2~20 °³·Î ¹Ù²ã°¡¸ç LDA ¸ğµ¨À» ¸¸µç ÈÄ
models_tada <- FindTopicsNumber(dtm = dtm_tada,
                                topics = 2:20,
                                return_models = T,
                                control = list(seed = 1234))
# ¼º´É ÁöÇ¥ ±×·¡ÇÁ
FindTopicsNumber_plot(models_tada)

# ÅäÇÈ ¼ö°¡ 8°³ÀÎ ¸ğµ¨ ÃßÃâ
lda_model <- models_tada %>%
    filter(topics == 8) %>%
    pull(LDA_model) %>% # ¸ğµ¨ ÃßÃâ
    .[[1]] # list ÃßÃâ
#LDA ¸ğµ¨ÀÇ beta ¸¦ ÀÌ¿ëÇØ °¢ ÅäÇÈ¿¡ µîÀåÇÒ È®·üÀÌ ³ôÀº »óÀ§ 10 °³ ´Ü¾î¸¦ ÃßÃâ
term_topic <- tidy(lda_model, matrix = "beta")
term_topic %>% 
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% print(n=10)

#ÅäÇÈº° ÁÖ¿ä ´Ü¾î¸¦ ³ªÅ¸³½ ¸·´ë ±×·¡ÇÁ ¸¸µé±â(¾Æ·¡ ±×¸², ±×·¡ÇÁ Á¦¸ñ Ãß°¡)
ggplot(term_topic, aes( x = reorder_within(term, beta, topic),
                            y = beta,
                            fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free", ncol = 4) +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))
