library(stringr)
library(tidytext)
library(KoNLP)
library(dplyr)
library(textclean)

raw_tazza <- readLines("C:/Users/lys74/Desktop/NLP/Sharper.txt", encoding = "UTF-8")
tazza <- raw_tazza %>%
  str_replace_all("[^��-�R]", " " ) %>% # �ѱ۸� �����
  str_squish() %>% # ���ӵ� ���� ����
  as_tibble() # tibble�� ��ȯ
head(tazza)

#���⸦ �������� ��ūȭ
word_space <- tazza %>% unnest_tokens(input = value, output = word, token = "words")
word_space <- word_space %>%  count(word, sort = T) #�� ���ϱ�
word_space <- word_space %>% filter(str_count(word) > 1) #�� ���� �ܾ� ����
word_space

#���縦 �������� ��ūȭ
word_noun <- tazza %>% unnest_tokens(input = value, output = word, token = extractNoun)
word_noun <- word_noun %>%  count(word, sort = T) #�� ���ϱ�
word_noun <- word_noun %>% filter(str_count(word) > 1) #�� ���� �ܾ� ����
word_noun

#�󵵰� ���� 20�� �ܾ� ���� (���� ����)
top20_space <- word_space %>% head(20)
top20_space

#�󵵰� ���� 20�� ���� ���� (���� ����)
top20_noun <- word_noun %>% head(20)
top20_noun
 
library(ggplot2)
#���� �׷��� ����� (���� ����)
ggplot(top20_space, aes(x = reorder(word, n), y = n)) + # �ܾ� �󵵼� ����
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "Ÿ¥ �뺻 �ܾ� ��(���� ����)", 
       x = NULL,y = NULL) + # �� �̸� ����
  theme(title = element_text(size = 12))

#���� �׷��� ����� (���� ����)
ggplot(top20_noun, aes(x = reorder(word, n), y = n)) + # �ܾ� �󵵼� ����
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "Ÿ¥ �뺻 �ܾ� ��(���� ����)", 
       x = NULL,y = NULL) + # �� �̸� ����
  theme(title = element_text(size = 12))


# ��Ʈ ����
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext_auto()


library(ggwordcloud)
#����Ŭ����(���� ����)
ggplot(word_space, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "gamjaflower") +
  scale_radius(limits = c(15, NA), range = c(5, 30)) +
  scale_color_gradient(low = "#31609e", high = "#b52155") +
  theme_minimal()

#����Ŭ����(���� ����)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(15, NA), range = c(5, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()

