#Carregar pacotes
library(tm)
library(stm)
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
library(ape)
library(BiocManager)
library(Rgraphviz)
library(lattice)
library(latticeExtra)

options(scipen = 999)
theme_set(theme_bw())


#Criando stopwords já adaptadas ao corpus
stopwords <- read.csv("C:\\Users\\Luã Braga\\Desktop\\omd\\perfis\\stopwords_perfis.csv", header = TRUE, sep = ";")

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

setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\perfis\\amorim")

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

setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\perfis\\luna")

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

############################ FREQUÊNCIAS DE PALAVRAS-CHAVE ######################

freq_pcs <- filter(tokens_freq, grepl('desenvolvimento|cooperação|entorno|dissusão|estratégia',tokens))

freq_pcs %>%
  mutate(tokens = reorder(tokens, n)) %>%
  ggplot(aes(tokens, n)) +
  geom_col(aes(reorder(tokens, n), n), width = 0.1, show.legend = FALSE, fill = "green3") +
  geom_point(col="green3", size=6.5) +
  geom_text(aes(label=n), color="black", size=3, vjust = 0.3) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ ministro)

################################ AZEVEDO ######################

#Wordcloud
pal2 <- brewer.pal(9,"BrBG")
wordcloud(azevedolimpo, min.freq=2,max.words=200, random.order=F, colors=pal2)

#Frequência de unigrams
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
  theme_bw() +
  labs(title="Palavras mais utilizadas pelo Ministro Fernando Azevedo", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")


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
  labs(title="Palavras mais utilizadas pelo Ministro Celso Amorim", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")


########################################################################
###################  ANÁLISE DE CLUSTERS ###############################
########################################################################

################################ AZEVEDO ######################

#Criando matriz com os top20 termos de azevedo
top_azevedo_tokens <- azevedo_freq %>%
  top_n(20, n) %>% 
  remove_rownames %>% 
  column_to_rownames(var="tokens")

#Calculando distância euclideana entre os termos
azevedo_escala <- scale(top_azevedo_tokens)
azevedo_distancia <- dist(azevedo_escala , method = "euclidean")

#Clusterização
azevedo_clusters <- hclust(azevedo_distancia , method = "ward.D2")
plot(as.phylo(azevedo_clusters), cex = 0.6, label.offset = 0.09)

################################ AMORIM ######################

#Criando matriz com os top20 termos de amorim
top_amorim_tokens <- amorim_freq %>%
  top_n(20, n) %>% 
  remove_rownames %>% 
  column_to_rownames(var="tokens")

#Calculando distância euclideana entre os termos
amorim_escala <- scale(top_amorim_tokens)
amorim_distancia <- dist(amorim_escala , method = "euclidean")

#Clusterização
amorim_clusters <- hclust(amorim_distancia , method = "ward.D2")
plot(as.phylo(amorim_clusters), cex = 0.6, label.offset = 0.09)

########################################################################
###################  ANÁLISE DE CORRELAÇÃO ENTRE PALAVRAS  #############
########################################################################

#Crie a Matriz Termo-Frequência
dtm <- DocumentTermMatrix(transformacao)
inspect(dtmpalavra)

#Determine o corte de palavras cuja associação será analisada
Allpalavras <- as.matrix(dtmpalavra)
frequencia <- sort(colSums(Allpalavras), decreasing = TRUE)
corte <- quantile(frequencia, probs = 0.99) # valor mínimo de frequência escolhido. Nesse caso, deu 4 palavras.

# Gráfico de frequência das palavras do corte
barchart(co ~ seq_along(co),
         data = data.frame(co = frequencia[frequencia > corte]),
         axis = axis.grid,
         horizontal = FALSE,
         scales = list(
           x = list(rot = 45, labels = names(frequencia)[frequencia >corte])),
         panel = function(x, y, ...) {
           panel.barchart(x, y, ...)
           panel.text(x, y + 2, y)
         })
# Gráfico de associação de palavras do corte

freq.terms <- findFreqTerms(dtmpalavra, lowfreq = corte)

plot(dtmpalavra, term = freq.terms, corThreshold = 0.5, weighting = TRUE)

################################ AZEVEDO ######################

#Criando DF com o grau de correlação entre as palavras por pagina
azevedo_cor <- tokens %>%
  filter(ministro %in% c("Fernando Azevedo")) %>%
  group_by(tokens) %>%
  filter(n() >= 0) %>% 
  pairwise_cor(tokens, pagina, sort = TRUE)

#Principais termos correlacionados com palavras selecionadas
azevedo_cor %>%
  mutate(item1 = fct_reorder(item1, correlation, .desc = FALSE)) %>%
  filter(item1 %in% c("defesa", "segurança")) %>%
  group_by(item1) %>%
  top_n(10, correlation) %>%
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
  labs(title="Principais correlações feitas pelo Ministro Fernando Azevedo", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")


################################ AMORIM ######################

#Criando DF com o grau de correlação entre as palavras por pagina
amorim_cor <- tokens %>%
  filter(ministro %in% c("Celso Amorim")) %>%
  group_by(tokens) %>%
  filter(n() >= 3) %>% 
  pairwise_cor(tokens, pagina, sort = TRUE)

#Principais termos correlacionados com palavras selecionadas
amorim_cor %>%
  mutate(item1 = fct_reorder(item1, correlation, .desc = FALSE)) %>%
  filter(item1 %in% c("entorno", "cooperação", "desenvolvimento")) %>%
  group_by(item1) %>%
  top_n(10, correlation) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_col(aes(reorder(item2, correlation), correlation), width = 0.3, show.legend = FALSE, fill = "green1") +
  geom_point(col="green3", size=6.5) + 
  geom_text(aes(label=round(correlation, digits = 1)), color = "black", vjust = 0.3, size = 4) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() + 
  ylab("correlação") +
  xlab(" ") +
  labs(title="Principais correlações feitas pelo Ministro Celso Amorim", 
       subtitle="Dados coletados em artigos, discursos, palestras e demais manifestações públicas")


#######################################################################################
################### ANÁLISE DE CORRELAÇÃO ENTRE MINISTROS  ############################
#######################################################################################

############ CORRELAÇÃO ENTRE TEXTOS ################

#Crie um df com o grau de correlação entre cada Ministro, por pares
ministro_cor <- tokens_freq %>%
  pairwise_cor(ministro, tokens, n, sort = TRUE)

#Gráfico com as correlação (correlação significativa: >=0.8 ou <=-0.8)
ministro_cor %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 9, color="Gray") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_minimal() +
  labs(x = "", y = "")

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
  labs(y = "Fernando Azevedo", x = NULL)
