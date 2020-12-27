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

#Criando as stopwords
stopwords_pt <- c(stopwords("pt"), stopwords("en"), stopwords(), stopwords("spanish"), "é")
stopwords_pt_df <- data.frame(word = stopwords_pt)

setwd("C:\\Users\\Luã Braga\\Desktop\\The shift of Brazilian foreign policy")

################################################################################################
############################## PARTE 1 - ANÁLISE DE FREQUÊNCIA  ################################
################################################################################################


############################ GRÁFICOS DE FREQUÊNCIA ######################################

#Transformando o livro do Amorim em um corpus
diretorio <- getwd()
corpus <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                        readerControl = list(reader = readPDF))

## Higienização do texto (tudo em caixa baixa e remove números e pontuação)
corpuslimpo <- tm_map(corpus, content_transformer(tolower))
corpuslimpo <- tm_map(corpuslimpo, removeWords, stopwords_pt)
corpuslimpo <- tm_map(corpuslimpo, removePunctuation)
corpuslimpo <- tm_map(corpuslimpo, stripWhitespace)
corpuslimpo <- tm_map(corpuslimpo, removeNumbers)

#Transformando em DF
corpus_df <- data.frame(text=unlist(sapply(corpuslimpo, `[`, "content")), stringsAsFactors=F)

#Vemos que cada linha é uma pág, vamos remover elementos pré e pós-textuais de cada livro (cria subsets só com as páginas de conteúdo de cada livro)
corpus_frame1 <- as.data.frame(corpus_df[c(15:256), 1])
corpus_frame2 <- as.data.frame(corpus_df[c(17:579), 1])

#Renomeando colunas
names(corpus_frame1) <- c("Texto")
names(corpus_frame2) <- c("Texto")

#Inseringo coluna Livro
corpus_frame1$Livro <- c("Amorim")
corpus_frame2$Livro <- c("Araujo")

#Junta os subsets
corpus_df <- bind_rows(corpus_frame1, corpus_frame2)

#Tokenizando
tokens <- corpus_df %>%
  unnest_tokens(Tokens, Texto)

#Tabela de frequência
tokens_freq <- tokens %>%                 
  group_by(Livro) %>%
  count(Tokens)

#Gráfico de Frequência Amorim
freq_amorim <- filter(tokens_freq, Livro == "Amorim")

freq_amorim %>%
  top_n(30) %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip()

#Gráfico de Frequência Araújo
freq_araujo <- filter(tokens_freq, Livro == "Araujo")

freq_araujo %>%
  top_n(30) %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip()


############################ FREQUÊNCIAS DE PALAVRAS-CHAVE ######################################

### Religião
freq_rel <- filter(tokens_freq, grepl('deus|cristia|cristã|christ',Tokens))


freq_rel %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Meio Ambiente
freq_amb <- filter(tokens_freq, grepl('clima|amazônia|amazon|polui|gases|desmata|queimada|estufa|
                                        ozônio|aquecimento|carbono|co2',Tokens))

freq_amb %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("n") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Organizações e blocos
freq_ois <- filter(tokens_freq, grepl('cooper|integrac|integraç|onu|oea|mercosul|unasul|prosul|
                                        ibas|brics|omc|ocde|cplp|asean',Tokens))

freq_ois %>%
  top_n(20) %>% 
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Parcerias
freq_parcs <- filter(tokens_freq, grepl('argentin|venezuel|estadosunid|estaduni|americ|china|cuba|
                                        índia|rússia|russ|indian|áfrica|african|europ|israel',Tokens))

freq_parcs %>%
  top_n(20) %>% 
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Economia
freq_econ <- filter(tokens_freq, grepl('desenvolviment|crescimento|desigualdade|pobreza|privatiz|estatiz',Tokens))

freq_econ %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill="grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Ideologia
freq_ideol <- filter(tokens_freq, grepl('social|comunis|globalis|capitalis|imperialis|periferia|conservad',Tokens))

freq_ideol %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill="grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Defesa e Segurança
freq_def <- filter(tokens_freq, grepl('armad|defesa|segurança|security|defense|dissuação|fronteiras|exército|marinha|
                                      aeronáutica|army|terror|espionagem|spy|spionage',Tokens))

freq_def %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill="grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)


################################################################################################
################### PARTE  - ANÁLISE DE CORRELAÇÃO ENTRE PALAVRAS  #############################
################################################################################################


############################### AMORIM ##############################

#Separando tokens do Amorim e inserindo coluna de página
corpus_df_amorim <- corpus_df %>%
  filter(Livro == "Amorim") %>%
  mutate(Página = row_number()) %>%
  unnest_tokens(Tokens, Texto)

#Criando DF com o grau de correlação entre as palavras
cor_amorim <- corpus_df_amorim %>%
  group_by(Tokens) %>%
  pairwise_cor(Tokens, Página, sort = TRUE)

#Observando nuvem de correlações entre palavras (>.35)
set.seed(2020)
cor_amorim %>%
  filter(correlation > .35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#Meio Ambiente
cor_amorim %>%
  filter(item1 == c("clima", "amazônia", "desmatamento", "gases", "estufa", "aquecimento")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  ylab("Correlação") +
  xlab(" ") +
  coord_flip()
