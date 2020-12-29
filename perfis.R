#Carregar pacotes
library(tm)
library(tidyverse)
library(tidytext)
library(pdftools)
library(stringr)
library(lexiconPT)
library(widyr)
library(ggraph)
library(igraph)
library(topicmodels)
library(scales)
library(janitor)
library(wordcloud)
options(scipen = 999)
theme_set(theme_bw())

#Criando as stopwords
stopwords <- c(stopwords("pt"), stopwords("en"), stopwords(), stopwords("spanish"), "é")


######################################################################################
############################## IMPORTANTO OS DADOS ###################################
######################################################################################

################################ AZEVEDO ######################

setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\perfis\\azevedo")

#Montando corpus
diretorio <- getwd()
azevedo <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                  readerControl = list(reader = readPDF))

## Higienização do texto
azevedolimpo <- tm_map(azevedo, content_transformer(tolower))
azevedolimpo <- tm_map(azevedolimpo, removeWords, stopwords)
azevedolimpo <- tm_map(azevedolimpo, removePunctuation)
azevedolimpo <- tm_map(azevedolimpo, stripWhitespace)
azevedolimpo <- tm_map(azevedolimpo, removeNumbers)

#Transformando em DF
azevedodf <- data.frame(text=unlist(sapply(azevedolimpo, `[`, "content")), stringsAsFactors=F)

#Renomeando coluna
names(azevedodf) <- c("texto")

#Coluna: ministro
azevedodf$ministro <- c('Fernando Azevedo')

#Numerando os documentos
azevedodf$n.documento <- c(1:61)

#Removendo rownames
azevedodf <- remove_rownames(azevedodf) 


################################ TODOS ######################

corpusdf <- bind_rows(azevedodf, lunadf, jungmanndf, rebelodf, amorimdf, jobimdf)
corpusdf <- azevedodf

######################################################################################
############################## ANÁLISE DE FREQUÊNCIA (UNIGRAMS e BIGRAMS) ############
######################################################################################

#Tokenizando
tokens <- corpusdf %>%
  unnest_tokens(tokens, texto)

#Tokenizando por bigrams
tokens_bi <- corpusdf %>%
  unnest_tokens(bigrams, texto, token = "ngrams", n = 2)

#Wordcloud
pal2 <- brewer.pal(9,"BrBG")
wordcloud(tokens$tokens, min.freq=2,max.words=200, random.order=F, colors=pal2)

#Frequência de unigrams Azevedo
azevedo_freq <- tokens %>%
  filter(ministro %in% c("Fernando Azevedo")) %>%
  count(tokens)

azevedo_freq %>%
  top_n(30) %>%
  mutate(tokens = reorder(tokens, n)) %>%
  ggplot(aes(tokens, n)) +
  geom_col(aes(reorder(tokens, n), n), width = 0.6, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  theme_bw() +
  labs(title="Palavras mais utilizadas pelo Ministro Fernando Azevedo", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")

#Frequência de bigrams
azevedo_freq_bi <- tokens_bi %>%                 
  filter(ministro %in% c("Fernando Azevedo")) %>%
  count(bigrams)

azevedo_freq_bi %>%
  top_n(30) %>%
  mutate(bigrams = reorder(bigrams, n)) %>%
  ggplot(aes(bigrams, n)) +
  geom_col(aes(reorder(bigrams, n), n), width = 0.6, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  theme_bw() +
  labs(title="Palavras mais utilizadas pelo Ministro Fernando Azevedo (bi-grams)", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")

########################################################################
###################  ANÁLISE DE CLUSTERS ###############################
########################################################################

#Criando matriz com os top20 termos azevedo
top_azevedo_tokens <- azevedo_freq %>%
  top_n(20, n) %>% 
  remove_rownames %>% 
  column_to_rownames(var="tokens")

#Calculando distância euclideana entre os termos
azevedo_escala <- scale(top_azevedo_tokens)
azevedo_distancia <- dist(azevedo_escala , method = "euclidean")

#Clusterização
azevedo_clusters <- hclust(azevedo_distancia , method = "ward.D2")
plot(azevedo_clusters)

########################################################################
###################  ANÁLISE DE CORRELAÇÃO ENTRE PALAVRAS  #############
########################################################################

#Criando DF com o grau de correlação entre as palavras por tipo de documento
azevedo_cor <- tokens %>%
  filter(ministro %in% c("Fernando Azevedo")) %>%
  group_by(tokens) %>%
  filter(n() >= 0) %>% 
  pairwise_cor(tokens, n.documento, sort = TRUE)

#Principais termos correlacionados com palavras selecionadas
azevedo_cor %>%
  mutate(item1 = fct_reorder(item1, correlation, .desc = FALSE)) %>%
  filter(item1 %in% c("defesa", "segurança", "brasil")) %>%
  group_by(item1) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_col(aes(reorder(item2, correlation), correlation), width = 0.6, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=round(correlation, digits = 1)), color = "black", vjust = 0.3, size = 4) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() + 
  ylab("correlação") +
  xlab(" ") +
  labs(title="Principais correlações de palavras com o termo Defesa e Segurança", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")


#######################################################################################
################### ANÁLISE DE SENTIMENTOS  ###########################################
#######################################################################################

# carregar DF (vamos usar o LexiconPT, porém há outros. Trata-se de uma decisão por adequação ao tipo do texto analisado)
data("sentiLex_lem_PT02")
sent <- sentiLex_lem_PT02

#Renomeando colunas para fazer os joins
names(sent) <- c("tokens", "classe", "polaridade", "polaridade.alvo", "class.polaridade")

#Renomeando nomes das classes de palavras
sent$classe <- str_replace_all(sent$classe, "V", "Verbo") 
sent$classe <- str_replace_all(sent$classe, "Adj", "Adjetivo")
sent$classe <- str_replace_all(sent$classe, "N", "Substantivo") 

# acrescentar valências de sentimento ao dafaframe principal (mantém só as palavras com carga sentimental, negativa ou positiva)
tokens_sent <- inner_join(tokens, sent, by = "tokens")

#faz subsets de cada ministro
discursos_sent <- as.data.frame(filter(tokens_sent, grepl('Azevedo', ministro)))

########## Palavras que contribuíram mais para a carga sentimental por tipo de documento

#Sumarização das palavras com carga sentimental
discursos_sent_geral <- discursos_sent %>%                 
  group_by(tokens) %>% 
  summarise (n = sum(polaridade)) %>%
  arrange(desc(n))

#Filtrando as 10 com maior carga positiva e positiva
discursos_sent_pos <- as.data.frame(filter(discursos_sent_geral, n > 0))
discursos_sent_neg <- as.data.frame(filter(discursos_sent_geral, n < 0))
discursos_sent_geral <- bind_rows(discursos_sent_pos, discursos_sent_neg)

discursos_sent_geral %>% 
  mutate(tokens = fct_reorder(tokens, n, .desc = FALSE)) %>%
  ggplot(aes(x = n, y = tokens)) +
  geom_bar(stat = 'identity', width = 0.7, show.legend = FALSE, fill = "salmon1") + 
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  labs(x = "Carga Sentimenal", y = " ") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")


#######################################################################################
################### ANÁLISE DE CORRELAÇÃO ENTRE MINISTROS  ############################
#######################################################################################












