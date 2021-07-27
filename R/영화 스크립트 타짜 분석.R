library(stringr)
library(tidytext)
library(KoNLP)
library(dplyr)
library(textclean)

raw_tazza <- readLines("C:/Users/lys74/Desktop/NLP/Sharper.txt", encoding = "UTF-8")
tazza <- raw_tazza %>%
  str_replace_all("[^°¡-ÆR]", " " ) %>% # ÇÑ±Û¸¸ ³²±â±â
  str_squish() %>% # ¿¬¼ÓµÈ °ø¹é Á¦°Å
  as_tibble() # tibble·Î º¯È¯
head(tazza)

#¶ç¾î¾²±â¸¦ ±âÁØÀ¸·Î ÅäÅ«È­
word_space <- tazza %>% unnest_tokens(input = value, output = word, token = "words")
word_space <- word_space %>%  count(word, sort = T) #ºóµµ ±¸ÇÏ±â
word_space <- word_space %>% filter(str_count(word) > 1) #ÇÑ ±ÛÀÚ ´Ü¾î Á¦°Å
word_space

#¸í»ç¸¦ ±âÁØÀ¸·Î ÅäÅ«È­
word_noun <- tazza %>% unnest_tokens(input = value, output = word, token = extractNoun)
word_noun <- word_noun %>%  count(word, sort = T) #ºóµµ ±¸ÇÏ±â
word_noun <- word_noun %>% filter(str_count(word) > 1) #ÇÑ ±ÛÀÚ ´Ü¾î Á¦°Å
word_noun

#ºóµµ°¡ ³ôÀº 20°³ ´Ü¾î ÃßÃâ (¶ç¾î¾²±â ±âÁØ)
top20_space <- word_space %>% head(20)
top20_space

#ºóµµ°¡ ³ôÀº 20°³ ¸í»ç ÃßÃâ (¸í»ç ±âÁØ)
top20_noun <- word_noun %>% head(20)
top20_noun
 
library(ggplot2)
#¸·´ë ±×·¡ÇÁ ¸¸µé±â (¶ç¾î¾²±â ±âÁØ)
ggplot(top20_space, aes(x = reorder(word, n), y = n)) + # ´Ü¾î ºóµµ¼ø Á¤·Ä
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "Å¸Â¥ ´ëº» ´Ü¾î ºóµµ(¶ç¾î¾²±â ±âÁØ)", 
       x = NULL,y = NULL) + # Ãà ÀÌ¸§ »èÁ¦
  theme(title = element_text(size = 12))

#¸·´ë ±×·¡ÇÁ ¸¸µé±â (¸í»ç ±âÁØ)
ggplot(top20_noun, aes(x = reorder(word, n), y = n)) + # ´Ü¾î ºóµµ¼ø Á¤·Ä
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "Å¸Â¥ ´ëº» ´Ü¾î ºóµµ(¸í»ç ±âÁØ)", 
       x = NULL,y = NULL) + # Ãà ÀÌ¸§ »èÁ¦
  theme(title = element_text(size = 12))


# ÆùÆ® ¼³Á¤
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext_auto()


library(ggwordcloud)
#¿öµåÅ¬¶ó¿ìµå(¶ç¾î¾²±â ±âÁØ)
ggplot(word_space, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "gamjaflower") +
  scale_radius(limits = c(15, NA), range = c(5, 30)) +
  scale_color_gradient(low = "#31609e", high = "#b52155") +
  theme_minimal()

#¿öµåÅ¬¶ó¿ìµå(¸í»ç ±âÁØ)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(15, NA), range = c(5, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()

