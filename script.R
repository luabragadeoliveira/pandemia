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


############################ ANÁLISE DE PALAVRAS-CHAVE ######################################

### Religião
freq_rel <- filter(tokens_freq, grepl('deus|cristão|cristo|cristã|cristianismo', Tokens))

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
freq_amb <- filter(tokens_freq, grepl('amazônia|pantanal|poluição|clima|gases|atmosfera|estufa|aquecimento', Tokens))

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
freq_ois <- filter(tokens_freq, grepl('cooperação|integração|onu|oea|mercosul|unasul|prosul|ibas|brics|omc|ocde|zopacas',Tokens))

freq_ois %>%
  top_n(10, n) %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Parcerias
freq_parcs <- filter(tokens_freq, grepl('argentina|venezuela|eua|china|cuba|índia|rússia|áfrica|europa|europeia|japão|israel',Tokens))

freq_parcs %>%
  top_n(13, n) %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "grey65") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Livro)

### Economia

freq_econ <- filter(tokens_freq, grepl('desenvolvimento|crescimento|desigualdade|pobreza',Tokens))

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

freq_ideol <- filter(tokens_freq, grepl('socialismo|socialista|comunismo|comunista|globalismo|capitalismo|imperialismo|imperialista|imperialistas|periferia|conservador|conservadorismo|liberal|liberalismo',Tokens))

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

freq_def <- filter(tokens_freq, grepl('armadas|defesa|segurança|dissuação|fronteiras|exército|marinha|aeronáutica',Tokens))

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
############################## PARTE  - BIGRAMS  ###############################################
################################################################################################

############################################# AMORIM ###########################################

#Determine diretório
setwd("C:\\Users\\Luã Braga\\Desktop\\The shift of Brazilian foreign policy")

# Crie o Corpus
diretorio <- getwd() # change this to directory where files are located
amorimcorpus <- VCorpus(DirSource(diretorio, pattern = "amorim.pdf"), 
                        readerControl = list(reader = readPDF))

#Ajustes e Transformações
## Higienização do texto (tudo em caixa baixa e remove números e pontuação)
amorimlimpo <- tm_map(amorimcorpus, content_transformer(tolower))
amorimlimpo <- tm_map(amorimlimpo, removeWords, stopwords_pt)
amorimlimpo <- tm_map(amorimlimpo, removePunctuation)
amorimlimpo <- tm_map(amorimlimpo, stripWhitespace)
amorimlimpo <- tm_map(amorimlimpo, removeNumbers)

#Transforme o Corpus para formato Tidy
amorimtidy <- tidy(amorimlimpo)

#Tokenizando por bi-grams
amorimbi <- amorimtidy %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#Separando bi-grams para filtrar novas stopwords
amorimbi_sep <- amorimbi %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Filtrando novas stopwords
amorimbi_filt <- amorimbi_sep %>%
  filter(!word1 %in% stopwords_pt_df$word) %>%
  filter(!word2 %in% stopwords_pt_df$word)

#Novos bi-grams filtrados
amorimbi_filt_cont <- amorimbi_filt %>%
  count(word1, word2, sort = TRUE)

#Gráfico com somente os bi-grams mais comuns
amorim_graph <- amorimbi_filt_cont %>%
  filter(n %in% (15:10000)) %>%
  graph_from_data_frame()

#Gráfico de rede de bi-grams mais frequentes
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.10, "inches"))

ggraph(amorim_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = a, end_cap = circle(.05, "inches")) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

############################################# ARAUJO ###########################################

#Determine diretório
setwd("C:\\Users\\Luã Braga\\Desktop\\The shift of Brazilian foreign policy")

# Crie o Corpus
diretorio <- getwd() # change this to directory where files are located
araujocorpus <- VCorpus(DirSource(diretorio, pattern = "araujo.pdf"), 
                        readerControl = list(reader = readPDF))

#Ajustes e Transformações
## Higienização do texto (tudo em caixa baixa e remove números e pontuação)
araujolimpo <- tm_map(araujocorpus, content_transformer(tolower))
araujolimpo <- tm_map(araujolimpo, removeWords, stopwords_pt)
araujolimpo <- tm_map(araujolimpo, removePunctuation)
araujolimpo <- tm_map(araujolimpo, stripWhitespace)
araujolimpo <- tm_map(araujolimpo, removeNumbers)

#Transforme o Corpus para formato Tidy
araujotidy <- tidy(araujolimpo)

#Tokenizando por bi-grams
araujobi <- araujotidy %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#Separando bi-grams para filtrar novas stopwords
araujobi_sep <- araujobi %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Filtrando novas stopwords
araujobi_filt <- araujobi_sep %>%
  filter(!word1 %in% stopwords_pt_df$word) %>%
  filter(!word2 %in% stopwords_pt_df$word)

#Novos bi-grams filtrados
araujobi_filt_cont <- araujobi_filt %>%
  count(word1, word2, sort = TRUE)

#Gráfico com somente os bi-grams mais comuns
araujo_graph <- araujobi_filt_cont %>%
  filter(n %in% (15:10000)) %>%
  graph_from_data_frame()

#Gráfico de rede de bi-grams mais frequentes
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.10, "inches"))

ggraph(araujo_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = a, end_cap = circle(.05, "inches")) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


################################################################################################
############################## PARTE  - ASSOCIAÇÃO DE PALAVRAS  ###############################################
################################################################################################
