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
    mutate(content = str_replace_all(content, "[^��-�R]", " "), # �ѱ۸� �����
           content = str_squish(content)) #�ߺ����� ����

#���� ����
word_noun <- roh %>%
    unnest_tokens(input = content,
                  output = word,
                  token = extractNoun,
                  drop = F) %>%
    filter(str_count(word) > 1) %>%
    
    # ������ �� �ߺ� �ܾ� ����
    group_by(id) %>%
    distinct(word, .keep_all = T) %>%
    ungroup() %>%
    select(id, word)

#�ܾ� �� ���ϱ�
count_word <- word_noun %>%
    add_count(word) %>%
    #�� 100ȸ ���� �ܾ� ����
    filter(n <= 100) %>%
    select(-n)
count_word

#������ �ҿ�� �����
stopword <- c("����", "�ϴ�", "�ϰ�", "�ϸ�", "�ؼ�", "�̹�", "�ϳ�",
              "�ؿ�", "�̰�", "�ϵ�", "�ϱ�", "����", "�Ѱ�", "����",
              "�װ�", "���", "����", "����", "�̰�", "�Ͻ�", "��ŭ")

#�ҿ�� ����
count_word <- count_word %>%
    filter(!word %in% stopword)

#�������� �� ���ϱ�
count_word_doc <- count_word %>%
    count(id, word, sort = T)
count_word_doc

# DTM �����
dtm_comment <- count_word_doc %>%
    cast_dtm(document = id, term = word, value = n)
dtm_comment
as.matrix(dtm_comment[1:8, 1:8])

#������ ���� �� ����
models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))
models %>%select(topics, Griffiths2004)
FindTopicsNumber_plot(models)

# ���� �� ����(���ȼ�: 9��)
optimal_model <- models %>%
    filter(topics == 9) %>%
    pull(LDA_model) %>% # �� ����
    .[[1]] # list ����
term_topic <- tidy(optimal_model, matrix = "beta")

# ���Ⱥ� beta ���� 10�� �ܾ� ����
top_term_topic <- term_topic %>%
    group_by(topic) %>%
    slice_max(beta, n = 10)
top_term_topic

#���Ⱥ� ���� �ܾ� ���� �׸���
ggplot(top_term_topic,
       aes( x = reorder_within(term, beta, topic),
            y = beta,
            fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free", ncol = 3) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(n.breaks = 4, #�� ������ 4�� ���ܷ� ���ϱ�
                       labels = number_format(accuracy = .01)) +
    labs(x = NULL) +
    theme(text = element_text(family = "nanumgothic"))

doc_topic <- tidy(optimal_model, matrix = "gamma")
doc_topic

doc_topic %>% count(topic)

# �������� Ȯ���� ���� ���� ���� ����
doc_class <- doc_topic %>%
    group_by(document) %>%
    slice_max(gamma, n = 1)
doc_class

# integer�� ��ȯ
doc_class$document <- as.integer(doc_class$document)
# �����ͼ� �����ϱ� ���� ���� ���� Ÿ���� integer�� ����
# ������ ���� ��ȣ �ο�
roh_topic <- raw_roh %>%
    left_join(doc_class, by = c("id" = "document"))
# ���� Ȯ��
roh_topic %>%
    select(id, topic)

#���Ⱥ� �ֿ� �ܾ� ���
top_terms <- term_topic %>%
    group_by(topic) %>%
    slice_max(beta, n = 6, with_ties = F) %>%
    summarise(term = paste(term, collapse = ", "))
top_terms

#���Ⱥ� ���� ��
count_topic <- roh_topic %>%
    count(topic)
count_topic

#���� �󵵿� �ֿ� �ܾ� ����
count_topic_word <- count_topic %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic_name = paste("Topic", topic))
count_topic_word

#���Ⱥ� ���� ���� �ֿ� �ܾ� ���� �׷���
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n), y = n, fill = topic_name)) +
    geom_col(show.legend = F) +
    coord_flip() +
    geom_text(aes(label = n) , # ���� �� ǥ��
              hjust = -0.2) + # ���� �ۿ� ǥ��
    geom_text(aes(label = term), # �ֿ� �ܾ� ǥ��
              hjust = 1.03, # ���� �ȿ� ǥ��
              col = "white", # ����
              fontface = "bold", # �β���
              family = "nanumgothic") + # ��Ʈ
    scale_y_continuous( expand = c(0, 0), # y��-���� ���� ���̱�
                        limits = c(0, 180)) + # y�� ����
    labs(x = NULL)