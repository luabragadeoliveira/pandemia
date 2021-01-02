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
library(ggdendro)
library(BiocManager)
library(Rgraphviz)
library(lattice)
library(latticeExtra)

options(scipen = 999)
theme_set(theme_bw())


#Criando stopwords já adaptadas ao corpus
stopwords <- read.csv("C:\\Users\\Luã Braga\\Desktop\\omd\\discursos\\stopwords_discursos.csv", header = TRUE, sep = ";")

######################################################################################
############################## IMPORTANTO OS DADOS ###################################
######################################################################################

# Fonte: https://www.gov.br/defesa/pt-br/acesso-a-informacao/institucional-2/ministro-da-defesa/discurso-do-ministro-da-defesa/ministro-de-estado-da-defesa

################################ AZEVEDO ######################

setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\discursos\\azevedo")

#Montando corpus
diretorio <- getwd()
azevedo <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                  readerControl = list(reader = readPDF))

## Higienização do texto
azevedolimpo <- tm_map(azevedo, content_transformer(tolower))
azevedolimpo <- tm_map(azevedolimpo, removeWords, stopwords$stopwords) 
azevedolimpo <- tm_map(azevedolimpo, removePunctuation)
azevedolimpo <- tm_map(azevedolimpo, stripWhitespace)
azevedolimpo <- tm_map(azevedolimpo, removeNumbers)

#Transformando em DF
azevedodf <- data.frame(text=unlist(sapply(azevedolimpo, "content")), stringsAsFactors=F)

#Renomeando coluna
names(azevedodf) <- c("texto")

#Coluna: ministro
azevedodf$ministro <- c('Fernando Azevedo')

#Coluna: pagina
azevedodf$pagina <- c(1:61)

#Coluna: data
azevedodf <- azevedodf %>%
  rownames_to_column(var="data")

#Padronizando a coluna data
azevedodf$data <- str_sub(azevedodf$data, start=0, end=10)

#Convertendo
azevedodf$data <- as.Date(azevedodf$data, "%d-%m-%Y")

################################ AMORIM ######################

setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\discursos\\amorim")

#Montando corpus
diretorio <- getwd()
amorim <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                   readerControl = list(reader = readPDF))

## Higienização do texto
amorimlimpo <- tm_map(amorim, content_transformer(tolower))
amorimlimpo <- tm_map(amorimlimpo, removeWords, stopwords$stopwords)
amorimlimpo <- tm_map(amorimlimpo, removePunctuation)
amorimlimpo <- tm_map(amorimlimpo, stripWhitespace)
amorimlimpo <- tm_map(amorimlimpo, removeNumbers)

#Transformando em DF
amorimdf <- data.frame(text=unlist(sapply(amorimlimpo, "content")), stringsAsFactors=F)

#Renomeando coluna
names(amorimdf) <- c("texto")

#Coluna: ministro
amorimdf$ministro <- c('Celso Amorim')

#Coluna: pagina
amorimdf$pagina <- c(1:653)

#Coluna: data
amorimdf <- amorimdf %>%
  rownames_to_column(var="data")

#Padronizando a coluna data
amorimdf$data <- str_sub(amorimdf$data, start=0, end=10)

#Convertendo
amorimdf$data <- as.Date(amorimdf$data, "%d-%m-%Y")

################################ LUNA ######################

setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\discursos\\luna")

#Montando corpus
diretorio <- getwd()
luna <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                  readerControl = list(reader = readPDF))

## Higienização do texto
lunalimpo <- tm_map(luna, content_transformer(tolower))
lunalimpo <- tm_map(lunalimpo, removeWords, stopwords$stopwords)
lunalimpo <- tm_map(lunalimpo, removePunctuation)
lunalimpo <- tm_map(lunalimpo, stripWhitespace)
lunalimpo <- tm_map(lunalimpo, removeNumbers)

#Transformando em DF
lunadf <- data.frame(text=unlist(sapply(lunalimpo, "content")), stringsAsFactors=F)

#Renomeando coluna
names(lunadf) <- c("texto")

#Coluna: ministro
lunadf$ministro <- c('Joaquim Silva e Luna')

#Coluna: pagina
lunadf$pagina <- c(1:41)

#Coluna: data
lunadf <- lunadf %>%
  rownames_to_column(var="data")

#Padronizando a coluna data
lunadf$data <- str_sub(lunadf$data, start=0, end=10)

#Convertendo
lunadf$data <- as.Date(lunadf$data, "%d-%m-%Y")

################################ TODOS ######################

corpusdf <- bind_rows(azevedodf, amorimdf, lunadf)

######################################################################################
############################## ANÁLISE DE FREQUÊNCIA  ############
######################################################################################

#Tokenizando
tokens <- corpusdf %>%
  unnest_tokens(tokens, texto)

#Tabela de frequência geral de tokens
tokens_freq <- tokens %>%
  group_by(ministro) %>%
  count(tokens)

################################ AZEVEDO ######################

#Wordcloud
pal2 <- brewer.pal(9,"BrBG")
wordcloud(azevedolimpo, min.freq=2,max.words=500, random.order=F, colors=pal2)

#Frequência
azevedo_freq <- tokens %>%
  filter(ministro %in% c("Fernando Azevedo")) %>%
  count(tokens)

azevedo_freq %>%
  top_n(20, n) %>%
  mutate(tokens = reorder(tokens, n)) %>%
  ggplot(aes(tokens, n)) +
  geom_col(aes(reorder(tokens, n), n), width = 0.6, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  labs(title= "Quais foram os assuntos mais abordados pelo Ministro \nFernando Azevedo?",
       subtitle = "Palavras mais proferidas pelo Ministro em discursos e artigos",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "",
       x = "Frequência")


################################ AMORIM ######################

#Wordcloud
pal2 <- brewer.pal(9,"BrBG")
wordcloud(amorimlimpo, min.freq=2,max.words=200, random.order=F, colors=pal2)

#Frequência de unigrams
amorim_freq <- tokens %>%
  filter(ministro %in% c("Celso Amorim")) %>%
  count(tokens)

amorim_freq %>%
  top_n(20, n) %>%
  mutate(tokens = reorder(tokens, n)) %>%
  ggplot(aes(tokens, n)) +
  geom_col(aes(reorder(tokens, n), n), width = 0.6, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  theme_bw() +
  labs(title= "Quais foram os assuntos mais abordados pelo Ministro \nCelso Amorim?",
       subtitle = "Palavras mais proferidas pelo Ministro em discursos e artigos",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "",
       x = "Frequência")


################################ SILVA E LUNA ######################

#Wordcloud
pal2 <- brewer.pal(9,"BrBG")
wordcloud(lunalimpo, min.freq=2,max.words=200, random.order=F, colors=pal2)

#Frequência de unigrams
luna_freq <- tokens %>%
  filter(ministro %in% c("Joaquim Silva e Luna")) %>%
  count(tokens)

luna_freq %>%
  top_n(20, n) %>%
  mutate(tokens = reorder(tokens, n)) %>%
  ggplot(aes(tokens, n)) +
  geom_col(aes(reorder(tokens, n), n), width = 0.6, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  theme_bw() +
  labs(title= "Quais foram os assuntos mais abordados pelo Ministro \nSilva e Luna?",
       subtitle = "Palavras mais proferidas pelo Ministro em discursos e artigos",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "",
       x = "Frequência")

############################ FREQUÊNCIA DE DISCURSOS POR ANO ######################

#Tabela de frequência de discursos por ano
discursos_freq <- corpusdf %>%
  group_by(ministro) %>%
  count(data)

discursos_freq %>%
  ggplot(aes(data, n)) +
  geom_line() +
  ylab("Frequência") +
  xlab(" ")+
  facet_grid(ministro ~.)+
  labs(title= "Quais Ministros da Defesa produziram mais discursos e artigos?",
       subtitle = "Comparação do volume de discursos e artigos produzidos por ano pelos Ministros",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "Quantidade")


############################ FREQUÊNCIAS DE PALAVRAS-CHAVE ######################

tokens_freq %>%
  filter(tokens %in% c("argentina", "bolívia", "chile", "colômbia",
                       "equador", "guiana", "paraguai", "peru", "suriname", 
                       "uruguai", "venezuela")) %>%
  mutate(tokens = reorder(tokens, n)) %>%
  ggplot(aes(tokens, n)) +
  geom_col(aes(reorder(tokens, n), n), width = 0.1, show.legend = FALSE, fill = "green3") +
  geom_point(col="green3", size=6.5) +
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ ministro)+
  labs(title="A América do Sul nos discursos dos Ministros da Defesa",
       subtitle = "Ênfase estimada pela frequência de citações dos países sul-americanos \npelos Ministros em discursos e artigos",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "",
       x = "Frequência")


#######################################################################################
################### ANÁLISE DE CORRELAÇÃO ENTRE MINISTROS  ############################
#######################################################################################


############ AGRUPAMENTO HIERÁRQUICO ##################

#Transformando tabela de frequência em matriz Ministro x Termos
matrix <- tokens_freq %>% 
  spread(tokens, n, fill = NA, convert = FALSE) %>% 
  remove_rownames %>% 
  column_to_rownames(var="ministro")

#Calculando distância euclideana entre os Ministros
distancia <- dist(matrix , method = "euclidean")

#Clusterizando pelo método Ward.D2
clusters <- hclust(distancia , method = "ward.D2")

#Dendrograma 
clusters %>% 
  ggdendrogram(rotate = TRUE, size = 2, labels=TRUE, leaf_labels = TRUE) +
  labs(title="O quanto se parecem os Ministros da Defesa?",
       subtitle = "Similaridade estimada pela técnica de Clusterização Aglomerativa, \na partir da distância euclideana entre as frequências de emprego de \npalavras por cada Ministro",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "Similaridade (Distância Euclideana)",
       x = "")

########### CORRELAÇÃO NO EMPREGO DE TERMOS ###########

proporcao_tokens <- tokens %>% 
  mutate(tokens = str_extract(tokens, "[a-z']+")) %>%
  count(ministro, tokens) %>%
  group_by(ministro) %>%
  mutate(proporcao = n / sum(n)) %>% 
  select(-n) %>% 
  spread(ministro, proporcao) %>% 
  gather(ministro, proporcao, `Celso Amorim`|`Joaquim Silva e Luna`)

proporcao_tokens %>% 
  ggplot(aes(x = proporcao, y = `Fernando Azevedo`, 
             color = abs(`Fernando Azevedo` - proporcao))) +
  geom_abline(color = "black", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = tokens), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "green1", high = "green4") +
  facet_wrap(~ministro, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "Fernando Azevedo", x = NULL)+
  labs(title="O quanto são similares as palavras empregadas pelo atual \nMinistro e pelos Ministros anteriores?",
       subtitle = "Palavras mais próximas à linha diagonal aparecem com frequências similares \nem ambos os conjuntos de discursos e artigos",
       caption="Fonte: Observatório do Ministério da Defesa",
       y = "Fernando Azevedo",
       x = "")
