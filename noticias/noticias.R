#Carregar pacotes
library(rvest)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(highcharter)

#Importando banco de dados transfomado (as etapas de raspagem e preparação dos dados para se obter este data frame estão descritas abaixo)
setwd("C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\noticias")
corpusdf <- read.csv("corpusdf.csv", header = TRUE, sep = ",")

##########################################################################
############################## CAPTURANDO E PREPARANDO DADOS #############
##########################################################################

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

#Salvando DF completo e transformado, para novas análises
write.csv(corpusdf,"C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\noticias\\corpusdf.csv", row.names = FALSE)

######################################################################################
############################## ANÁLISE EXPLORATÓRIA ##################################
######################################################################################

#Tokenizando
tokens <- corpusdf %>%
  unnest_tokens(tokens, text) 

#Tabela de frequência
tokens_freq <- tokens %>%
  count(forca, tokens, sort=TRUE) %>%
  bind_tf_idf(tokens, forca, n) %>% 
  arrange(-tf_idf)

#Removendo linhas específicas
tokens_freq <- tokens_freq[-c(31, 26),]

########
## EB ##
########

#Wordcloud
cloud_eb <- tokens_freq %>% 
  filter(forca == "Exército Brasileiro") %>% 
  select(tokens, tf_idf)

set.seed(1234)
wordcloud2(data=cloud_eb, size=1.6)

#Barplot
tokens_freq %>%
  filter(forca == "Exército Brasileiro") %>% 
  select(forca, tokens, tf_idf) %>% 
  group_by(forca) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  hchart('bar', 
         hcaes(x = tokens, y = tf_idf),
         name = "TF-IDF") %>% 
  hc_title(text = "Termos mais relevantes nas notícias do Exército Brasileiro", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "Relevância estimada pela métrica TF-IDF", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório do Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "TF-IDF"))

########
## MB ##
########

#Wordcloud
cloud_mb <- tokens_freq %>% 
  filter(forca == "Marinha do Brasil") %>% 
  select(tokens, tf_idf)

set.seed(1234)
wordcloud2(data=cloud_mb, size=1.6)

#Barplot
tokens_freq %>%
  filter(forca == "Marinha do Brasil") %>% 
  select(forca, tokens, tf_idf) %>% 
  group_by(forca) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  hchart('bar', 
         hcaes(x = tokens, y = tf_idf),
         name = "TF-IDF") %>% 
  hc_title(text = "Termos mais relevantes nas notícias da Marinha do Brasil", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "Relevância estimada pela métrica TF-IDF", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório do Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "TF-IDF"))

########
## FAB ##
########

#Wordcloud
cloud_fab <- tokens_freq %>% 
  filter(forca == "Força Aérea Brasileira") %>% 
  select(tokens, tf_idf)

set.seed(1234)
wordcloud2(data=cloud_fab, size=1.6)

#Barplot
tokens_freq %>%
  filter(forca == "Força Aérea Brasileira") %>% 
  select(forca, tokens, tf_idf) %>% 
  group_by(forca) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  hchart('bar', 
         hcaes(x = tokens, y = tf_idf),
         name = "TF-IDF") %>% 
  hc_title(text = "Termos mais relevantes nas notícias da Força Aérea Brasileira", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "Relevância estimada pela métrica TF-IDF", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório do Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "TF-IDF"))
