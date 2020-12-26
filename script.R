#Carregar pacotes
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(pdftools)
library(stringr)
library(gridExtra)
library(lexiconPT)
library(widyr)
library(ggraph)
library(igraph)
library(xtable)
library(topicmodels)

#Criando as stopwords
stopwords_pt <- c(stopwords("pt"), stopwords("en"), stopwords(), stopwords("spanish"), "é")
stopwords_pt_df <- data.frame(word = stopwords_pt)

setwd("C:\\Users\\Luã Braga\\Desktop\\The shift of Brazilian foreign policy")

################################################################################################
############################## PARTE 1 - ANÁLISE DE FREQUÊNCIA  ################################
################################################################################################


############################ GRÁFICOS DE FREQUÊNCIA ######################################

#Juntando ambos os livros em um corpus
diretorio <- getwd()
corpus <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                        readerControl = list(reader = readPDF))

## Higienização do texto (tudo em caixa baixa e remove números e pontuação)
corpuslimpo <- tm_map(corpus, content_transformer(tolower))
corpuslimpo <- tm_map(corpuslimpo, removeWords, stopwords_pt)
corpuslimpo <- tm_map(corpuslimpo, removePunctuation)
corpuslimpo <- tm_map(corpuslimpo, stripWhitespace)
corpuslimpo <- tm_map(corpuslimpo, removeNumbers)


#Criando matriz de frequência
frequencia <- as.matrix(DocumentTermMatrix(corpuslimpo))

#Tranformando em DF
frequenciadf <- as.data.frame(frequencia)

#Inserindo coluna de autor
frequenciadf$Gestão <- c("Amorim", "Araujo") 

#Transpondo dados
frequenciadf2 <- gather(frequenciadf, "Termo", "Frequência", 1:17869)# <- colunas que serão transformadas (todas exceto autor, a última)

#Gráfico de Frequência Amorim

freq_amorim <- filter(frequenciadf2, Gestão == "Amorim")

freq_amorim %>%
  top_n(30, Frequência) %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE, fill="red") +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip()

#Gráfico de Frequência de Araújo

freq_araujo <- filter(frequenciadf2, Gestão == "Araujo")

freq_araujo %>%
  top_n(30, Frequência) %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE, fill="blue") +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip()


############################ ANÁLISE DE PALAVRAS-CHAVE ######################################

# Eixos Temáticos:
#   Blocos e OIs: cooperação, integração, onu, oea, mercosul, unasul, prosul, ibas, brics, omc, ocde
#   Parcerias: argentina, venezuela, eua, china, cuba, índia, rússia, áfrica, europa, europeia, japão, israel
#   Ideologia: socialismo, socialista, comunismo, comunista, globalismo, capitalismo, imperialismo, imperialista, imperialistas, periferia, conservador, conservadorismo, liberal, liberalismo
#   Religião: fé, cristianismo, cristão, cristã, intolerância, deus
#   Economia: desenvolvimento, crescimento, renda, desigualdade, pobreza, fome
#   Defesa e Segurança: armadas, defesa, segurança, dissuação, fronteiras, exército, marinha, aeronáutica
#   Meio Ambiente: amazônia, pantanal, poluição, clima, climáticas, queimadas

### Religião
freq_rel <- filter(frequenciadf2, grepl('deus|cristão|cristo|cristã|cristianismo', Termo))

freq_rel %>%
  top_n(20, Frequência) %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)

### Meio Ambiente
freq_amb <- filter(frequenciadf2, grepl('amazônia|pantanal|poluição|clima|gases|atmosfera|estufa|aquecimento', Termo))

freq_amb %>%
  top_n(8, Frequência) %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)

### Organizações e blocos
freq_ois <- filter(frequenciadf2, grepl('cooperação|integração|onu|oea|mercosul|unasul|prosul|ibas|brics|omc|ocde|zopacas',Termo))

freq_ois %>%
  top_n(20, Frequência) %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)

### Parcerias
freq_parcs <- filter(frequenciadf2, grepl('argentina|venezuela|eua|china|cuba|índia|rússia|áfrica|europa|europeia|japão|israel',Termo))

freq_parcs %>%
  top_n(20, Frequência) %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)

### Economia

freq_econ <- filter(frequenciadf2, grepl('desenvolvimento|crescimento|desigualdade|pobreza',Termo))

freq_econ %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)

### Ideologia

freq_ideol <- filter(frequenciadf2, grepl('socialismo|socialista|comunismo|comunista|globalismo|capitalismo|imperialismo|imperialista|imperialistas|periferia|conservador|conservadorismo|liberal|liberalismo',Termo))

freq_ideol %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)

### Defesa e Segurança

freq_def <- filter(frequenciadf2, grepl('armadas|defesa|segurança|dissuação|fronteiras|exército|marinha|aeronáutica',Termo))

freq_def %>%
  mutate(Termo = reorder(Termo, Frequência)) %>%
  ggplot(aes(Termo, Frequência)) +
  geom_bar(aes(reorder(Termo, Frequência), Frequência), stat = 'identity', width = 0.7, show.legend = FALSE) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Gestão)


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
