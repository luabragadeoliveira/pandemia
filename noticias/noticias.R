#Carregar pacotes
library(rvest)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)

options(scipen = 999)
theme_set(theme_bw()) 

#Criando função de limpeza do texto
limpar <- function(corpus) {
  stopwords <- read.csv("C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\noticias\\stopwords_noticias.csv", header = TRUE, sep = ";")
  corpuslimpo = tm_map(corpus, content_transformer(tolower))
  corpuslimpo = tm_map(corpuslimpo, removeWords, stopwords$stopwords)
  corpuslimpo = tm_map(corpuslimpo, removePunctuation)
  corpuslimpo = tm_map(corpuslimpo, stripWhitespace)
  corpuslimpo = tm_map(corpuslimpo, removeNumbers)
  return(corpuslimpo)
  
}

##########################################################################
############################## CAPTURANDO E PREPARANDO DADOS #############
##########################################################################

############################ EB ######################

#URL base (radical)
url <- "http://www.eb.mil.br/combate-covid19?p_p_id=101_INSTANCE_HD6KzyKyeFnm&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_pos=1&p_p_col_count=2&_101_INSTANCE_HD6KzyKyeFnm_delta=10&_101_INSTANCE_HD6KzyKyeFnm_keywords=&_101_INSTANCE_HD6KzyKyeFnm_advancedSearch=false&_101_INSTANCE_HD6KzyKyeFnm_andOperator=true&p_r_p_564233524_resetCur=false&_101_INSTANCE_HD6KzyKyeFnm_cur="

#Será necessário capturar notícias página por página (de 1 a 82)

#Capturando títulos
url %>% 
  map2_chr(1:82,paste0) %>%
  map(. %>% 
        read_html() %>% 
        html_nodes("h3") %>% 
        html_text()
  ) %>% 
  unlist() -> eb

#Salvando dados capturados
write.csv(eb,"C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\noticias\\eb.csv", row.names = FALSE)
View(eb)

#Transforma em Corpus já fazendo a limpeza do texto
eb_corpus <- limpar(VCorpus(VectorSource(eb)))

#Criando DF só com as colunas de interesse
#Inserindo coluna Força
ebdf <- tidy(eb_corpus) %>% 
  select(id, text) %>% 
  mutate(forca = as.factor(c("Exército Brasileiro")))


############################ MB ######################

#URL base (radical)
url <- "https://www.marinha.mil.br/combate-ao-covid19/acoes-realizadas"

#Capturando títulos
url %>% 
  read_html() %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  unlist() -> mb

#Salvando dados capturados
write.csv(mb,"C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\noticias\\mb.csv", row.names = FALSE)
View(mb)

#Transforma em Corpus já fazendo a limpeza do texto
mb_corpus <- limpar(VCorpus(VectorSource(mb)))

#Criando DF só com as colunas de interesse
#Inserindo coluna Força
mbdf <- tidy(mb_corpus) %>% 
  select(id, text) %>% 
  mutate(forca = as.factor(c("Marinha do Brasil")))


############################ FAB ######################

#URL base (radical)
url <- "https://www.fab.mil.br/noticias/tag/COVID-19"

#Capturando títulos
url %>% 
  read_html() %>% 
  html_nodes("span") %>% 
  html_text() %>% 
  unlist() -> fab

#Salvando dados capturados
write.csv(fab,"C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\noticias\\fab.csv", row.names = FALSE)
View(fab)

#Transforma em Corpus já fazendo a limpeza do texto
fab_corpus <- limpar(VCorpus(VectorSource(fab)))

#Criando DF só com as colunas de interesse
#Inserindo coluna Força
fabdf <- tidy(fab_corpus) %>% 
  select(id, text) %>% 
  mutate(forca = as.factor(c("Força Aérea Brasileira")))


############################ UNINDO TODOS ######################
corpusdf <- bind_rows(ebdf, mbdf, fabdf)

######################################################################################
############################## ANÁLISE EXPLORATÓRIA ##################################
######################################################################################

#Tokenizando
tokens <- corpusdf %>%
  unnest_tokens(tokens, text) 

#Tabela de frequência
tokens_freq <- tokens %>%
  count(forca, tokens, sort=TRUE) %>%
  bind_tf_idf(tokens, forca, n)

#Wordcloud EB
cloud_eb <- tokens_freq %>% 
  filter(forca == "Exército Brasileiro")

set.seed(1234)
wordcloud(words = cloud_eb$tokens, 
          freq = cloud_eb$tf_idf, 
          min.freq = 1,
          max.words=500, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud MB
cloud_mb <- tokens_freq %>% 
  filter(forca == "Marinha do Brasil")

set.seed(1234)
wordcloud(words = cloud_mb$tokens, 
          freq = cloud_mb$tf_idf, 
          min.freq = 1,
          max.words=500, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud FAB
cloud_fab <- tokens_freq %>% 
  filter(forca == "Força Aérea Brasileira")

set.seed(1234)
wordcloud(words = cloud_fab$tokens, 
          freq = cloud_fab$tf_idf, 
          min.freq = 1,
          max.words=500, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Gráfico de frequência geral (TF_IDF)
tokens_freq %>%
  group_by(forca) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(tokens, tf_idf), fill = forca)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~forca, ncol = 2, scales = "free") +
  labs(title= "",
       subtitle = "Relevância estimada pelo TF-IDF das palavras no conjunto de notícias",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "",
       x = "TF-IDF")
