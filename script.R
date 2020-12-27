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

#Criando as stopwords
stopwords <- c(stopwords("pt"), stopwords("en"), stopwords(), stopwords("spanish"), "é")

setwd("C:\\Users\\Luã Braga\\Desktop\\The shift of Brazilian foreign policy")

######################################################################################
############################## ANÁLISE DE FREQUÊNCIA  ################################
######################################################################################

#Transformando os Ministros em um corpus
diretorio <- getwd()
corpus <- VCorpus(DirSource(diretorio, pattern = ".pdf"), 
                        readerControl = list(reader = readPDF))

## Higienização do texto (tudo em caixa baixa e remove números e pontuação)
corpuslimpo <- tm_map(corpus, content_transformer(tolower))
corpuslimpo <- tm_map(corpuslimpo, removeWords, stopwords)
corpuslimpo <- tm_map(corpuslimpo, removePunctuation)
corpuslimpo <- tm_map(corpuslimpo, stripWhitespace)
corpuslimpo <- tm_map(corpuslimpo, removeNumbers)

#Transformando em DF
corpus_df <- data.frame(text=unlist(sapply(corpuslimpo, `[`, "content")), stringsAsFactors=F)

#Vemos que cada linha é uma pág, vamos remover elementos pré e pós-textuais de cada Ministro (cria subsets só com as páginas de conteúdo de cada Ministro)
corpus_frame1 <- as.data.frame(corpus_df[c(15:256), 1])
corpus_frame2 <- as.data.frame(corpus_df[c(17:579), 1])

#Renomeando colunas
names(corpus_frame1) <- c("Texto")
names(corpus_frame2) <- c("Texto")

#Inserindo coluna de página
corpus_frame1$Página <- c(1:242)
corpus_frame2$Página <- c(1:563)

#Inseringo coluna Ministro
corpus_frame1$Ministro <- c("Amorim")
corpus_frame2$Ministro <- c("Araujo")

#Junta os subsets
corpus_df <- bind_rows(corpus_frame1, corpus_frame2)

#Tokenizando
tokens <- corpus_df %>%
  unnest_tokens(Tokens, Texto)

#Tabela de frequência
tokens_freq <- tokens %>%                 
  group_by(Ministro) %>%
  count(Tokens)

#Gráfico de Frequência Amorim
freq_amorim <- filter(tokens_freq, Ministro == "Amorim")

freq_amorim %>%
  top_n(30) %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "salmon1") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip()

#Gráfico de Frequência Araújo
freq_araujo <- filter(tokens_freq, Ministro == "Araujo")

freq_araujo %>%
  top_n(30) %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE, fill = "skyblue1") +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip()


############################ FREQUÊNCIAS DE PALAVRAS-CHAVE ######################################

#Religião e Costumes
freq_rel <- filter(tokens_freq, grepl('deus|cristia|cristã|christ|família|sexu|gay|abort',Tokens))

freq_rel %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n, fill = Ministro)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE) +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Ministro)

#Meio Ambiente
freq_amb <- filter(tokens_freq, grepl('clima|amazônia|amazon|polui|gases|desmata|queimada|estufa|
                                        ozônio|aquecimento|carbono|co2',Tokens))

freq_amb %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n, fill = Ministro)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE) +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("n") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Ministro)

#Economia
freq_econ <- filter(tokens_freq, grepl('desenvolviment|desigualdade|pobr|privatiz|estati|trabalhador|liberal|classe',Tokens))

freq_econ %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n, fill = Ministro)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE) +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Ministro)


#Ideologias Políticas
freq_ideol <- filter(tokens_freq, grepl('sociali|comunis|globalis|capitalis|imperialis|periferia|
                                        conservad|revoluc|reacionarismo|reacionário|
                                        conspir|direita|direiti|esquerda|esquerdi',Tokens))

freq_ideol %>%
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n, fill = Ministro)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE) +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Ministro)

#Relações Internacionais
freq_parcs <- filter(tokens_freq, grepl('cuba|venezuel|estadosunid|estaduni|americ|china|cuba|
                                        arábia|rússia|russ|europ|israel',Tokens))

freq_parcs %>%
  top_n(20) %>% 
  mutate(Tokens = reorder(Tokens, n)) %>%
  ggplot(aes(Tokens, n, fill = Ministro)) +
  geom_bar(aes(reorder(Tokens, n), n), stat = 'identity', width = 0.7, show.legend = FALSE) +
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  ylab("Frequência") +
  xlab(" ")+
  coord_flip() +
  facet_wrap(~ Ministro)

########################################################################################
###################  ANÁLISE DE CORRELAÇÃO ENTRE PALAVRAS  #############################
########################################################################################

############################### AMORIM ##############################

#Separando tokens do Amorim e inserindo coluna de página
corpus_df_amorim <- corpus_df %>%
  filter(Ministro == "Amorim") %>%
  unnest_tokens(Tokens, Texto)

#Criando DF com o grau de correlação entre as palavras
cor_amorim <- corpus_df_amorim %>%
  group_by(Tokens) %>%
  filter(n() >= 3) %>% 
  pairwise_cor(Tokens, Página, sort = TRUE)

# Palavras mais relacionadas com eixo Ideologias Políticas: como o corte foi em 3, não há.


############################### ARAUJO ##############################

#Separando tokens do Amorim e inserindo coluna de página
corpus_df_araujo <- corpus_df %>%
  filter(Ministro == "Araujo") %>%
  unnest_tokens(Tokens, Texto)

#Criando DF com o grau de correlação entre as palavras
cor_araujo <- corpus_df_araujo %>%
  group_by(Tokens) %>%
  filter(n() >= 9) %>% 
  pairwise_cor(Tokens, Página, sort = TRUE)

# Correlações no eixo Ideologias Políticas
cor_araujo %>%
  mutate(item1 = fct_reorder(item1, correlation, .desc = FALSE)) %>%
  filter(item1 %in% c("comunismo", "globalismo", "esquerda")) %>%
  group_by(item1) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = 'identity', fill = "skyblue1") + 
  geom_text(aes(label=round(correlation, digits = 1)), vjust = 0.3, size = 4) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


#######################################################################################
################### ANÁLISE DE SENTIMENTOS  ###########################################
#######################################################################################

# carregar DF (vamos usar o LexiconPT, porém há outros. Trata-se de uma decisão por adequação ao tipo do texto analisado)
data("sentiLex_lem_PT02")
sent <- sentiLex_lem_PT02

#Renomeando colunas para fazer os joins
names(sent) <- c("Tokens", "Classe", "Polaridade", "Polaridade Alvo", "Class. de Polaridade")

#Renomeando nomes das classes de palavras
sent$Classe <- str_replace_all(sent$Classe, "V", "Verbo") 
sent$Classe <- str_replace_all(sent$Classe, "Adj", "Adjetivo")
sent$Classe <- str_replace_all(sent$Classe, "N", "Substantivo") 

# acrescentar ao dafaframe principal
tokens_sent <- inner_join(tokens, sent, by = "Tokens")

#Para calcular polaridade de palavras por página: faz subsets de palavras cada Ministro
amorim_sent <- as.data.frame(tokens_sent[c(1:1049), c(1:7)])
araujo_sent <- as.data.frame(tokens_sent[c(1050:4275),c(1:7)])

#Somatório de carga sentimental por página de cada Ministro
Pol_pag_amorim <- amorim_sent %>%
  group_by(Página) %>% 
  summarise (n = sum(Polaridade))
names(Pol_pag_amorim) <- c("Página", "Pol.Pag")

Pol_pag_araujo <- araujo_sent %>%
  group_by(Página) %>% 
  summarise (n = sum(Polaridade))
names(Pol_pag_araujo) <- c("Página", "Pol.Pag")

#Inserindo polaridades por página nos subsets
amorim_sent <- inner_join(amorim_sent, Pol_pag_amorim, by = "Página")
araujo_sent <- inner_join(araujo_sent, Pol_pag_araujo, by = "Página")

#Junta os subsets
tokens_sent <- bind_rows(amorim_sent, araujo_sent)
tokens_sent$Pol.Pag <- as.numeric(tokens_sent$Pol.Pag)

#Total de palavras com carga setimental por ministro
tokens_sent %>% 
  ggplot(aes(x = Polaridade, y = Ministro, fill = Ministro)) +
  geom_bar(stat="identity", width = 0.3) + 
  labs(x = "Carga Sentimenal", y = " ") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

#Palavras com carga setimental por página
tokens_sent %>% 
  ggplot(aes(x = Página, y = Pol.Pag)) +
  geom_col(aes(fill = Ministro)) + 
  labs(x = "Páginas", y = "Carga sentimental")

#Palavras com carga setimental por classe gramatical
tokens_sent %>% 
  ggplot(aes(x = Polaridade, y = Classe, fill = Ministro)) +
  geom_bar(stat="identity", width = 0.3) + 
  labs(x = "Carga Sentimenal", y = " ") +
  facet_wrap(~ Ministro) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

##Palavras que contribuíram mais para a carga sentimental do Amorim

#Sumarização das palavras com carga sentimental
amorim_sent_geral <- amorim_sent %>%                 
  group_by(Tokens) %>% 
  summarise (n = sum(Polaridade)) %>%
  arrange(desc(n))

#Filtrando as 10 com maior carga positiva e positiva
amorim_sent_pos <- as.data.frame(amorim_sent_geral[c(1:10), c(1:2)])
amorim_sent_neg <- as.data.frame(amorim_sent_geral[c(274:283), c(1:2)])
amorim_sent_geral <- bind_rows(amorim_sent_pos, amorim_sent_neg)

amorim_sent_geral %>% 
  mutate(Tokens = fct_reorder(Tokens, n, .desc = FALSE)) %>%
  ggplot(aes(x = n, y = Tokens)) +
  geom_bar(stat = 'identity', width = 0.7, show.legend = FALSE, fill = "salmon1") + 
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  labs(x = "Carga Sentimenal", y = " ") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

## Palavras que contribuíram mais para a carga sentimental do Araujo

#Sumarização das palavras com carga sentimental
araujo_sent_geral <- araujo_sent %>%                 
  group_by(Tokens) %>% 
  summarise (n = sum(Polaridade)) %>%
  arrange(desc(n))

#Filtrando as 10 com maior carga positiva e positiva
araujo_sent_pos <- as.data.frame(araujo_sent_geral[c(1:10), c(1:2)])
araujo_sent_neg <- as.data.frame(araujo_sent_geral[c(545:554), c(1:2)])
araujo_sent_geral <- bind_rows(araujo_sent_pos, araujo_sent_neg)

araujo_sent_geral %>% 
  mutate(Tokens = fct_reorder(Tokens, n, .desc = FALSE)) %>%
  ggplot(aes(x = n, y = Tokens)) +
  geom_bar(stat = 'identity', width = 0.7, show.legend = FALSE, fill = "skyblue1") + 
  geom_text(aes(label=n), vjust = 0.3, size = 4) +
  labs(x = "Carga Sentimenal", y = " ") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")
