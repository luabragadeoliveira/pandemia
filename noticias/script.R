library(tidyverse)
library(RColorBrewer)
library(ggspatial)
library(geojsonio)
library(geobr)
library(wordcloud2)
library(tidytext)
library(htmlwidgets)
library(tm)

#Desabilita notação científica
options(scipen=999)

setwd("C:\\Users\\luaoliveira\\Desktop\\Dados - Pandemia")

#importa base de dados
script <- read.csv("preencher.csv", header = T, sep = ";")
glimpse(script)

######################################### TEMÁTICAS ####################################

#Tabela de frequência
tematica <- script %>%
  count(tematica, sort=TRUE)

#Exclui linhas faltantes
tematica <- tematica[-c(5),]

#Gráficoo
tematica %>%
  mutate(tematica = fct_reorder(tematica, n)) %>%
  ggplot(aes(tematica, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  labs(title = "Eixos temáticos das ações da Operação COVID-19",
       subtitle = "As principais atividades foram as de higienização",
       x = "",
       y = "quantidade de ações",
       caption = "Fonte: Projeto Pandemia e Defesa") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=14, vjust=0.5)) +
  theme(axis.text.x=element_text(size=15, vjust=0.5)) +
  theme(axis.text.y=element_text(size=15, vjust=0.5)) +
  coord_flip()


############################################### NUVEM DE PALAVRAS ######################################

#Criando função de higienização do texto
limpar <- function(corpus) {
  stopwords <- read.csv("C:\\Users\\luaoliveira\\Desktop\\Pandemia\\noticias\\stopwords.csv", header = TRUE, sep = ";")
  corpuslimpo = tm_map(corpus, content_transformer(tolower))
  corpuslimpo = tm_map(corpuslimpo, removeWords, stopwords$stopwords)
  corpuslimpo = tm_map(corpuslimpo, removePunctuation)
  corpuslimpo = tm_map(corpuslimpo, stripWhitespace)
  corpuslimpo = tm_map(corpuslimpo, removeNumbers)
  return(corpuslimpo)
}

#Isola só a variável título, transforma em character, transforma em corpus e limpa
corpus <- limpar(Corpus(VectorSource(as.character(script$titulo))))

#Transforma corpus em data frame
corpusdf <- data.frame(text=sapply(corpus, identity), stringsAsFactors=F)

#Tokeniza e gera tabela de frequência de palavras
tokens <- corpusdf %>%
  unnest_tokens(tokens, text) %>% 
  select(tokens) %>% 
  count(tokens)

#Wordcloud
set.seed(123)
wordcloud <- wordcloud2(tokens, size=1.6, color='random-dark')

#Salvando em html
saveWidget(wordcloud, "minha_nuvem.html", selfcontained = F)


############################################### MAPAS ######################################

##Baixa limites do Brasil
brasil <- read_country()

#Baixa limites dos estados brasileiros
estados <- read_state(code_state = 'all')

#Analisa dados
head(brasil)
head(estados)

############ ESTADOS DO BRASIL ###############

#Tabela de frequência (já renomeando a variável)
estados_freq <- script %>%
  count(estado, sort=TRUE) %>% 
  rename(abbrev_state = estado)

#Exclui linhas faltantes
estados_freq <- estados_freq[-c(14),]

#Junta no DF dos estados e renomeia N
estados <- inner_join(estados, estados_freq) %>% 
  rename(Ações = n)

#Inserindo parâmetros no BR
ggplot(estados) +
  geom_sf(aes(fill = Ações, col = Ações)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Greens")) +
  scale_color_gradientn (colours = brewer.pal(9, "Greens")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Ações da Operação COVID-19 no Brasil",
       subtitle = "Houve maior concentração nos estados do Distrito Federal, \nPará e Amazonas",
       x = "",
       y = "",
       caption = "") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))
