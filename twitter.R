install.packages("twitteR")
install.packages("rtweet")
library(twitteR)
library(rtweet)
library(tidyverse)
library(tm)
library(tidytext)
library(wordcloud2)
library(ggpubr)
library(geobr)

#Autorizações
api_key <- "c6GLS7YInNcMXkVspEBKGlNZf"
api_secret <- "G4UjBn9Ml1NLNgX7YLaQK88WCYD7hEAj0xz3z90N8AO6vSYB4Q"
access_token <- "1468948689184083971-Um889rR3hOSlSSkonKypX9ErKKMX73"
access_token_secret <- "IzCIW9cClAOn1RX5qCDdZCrMGYr5g6urLSpFm1joNrWGl"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


#Perguntas?
# O quão ativos foram os perfis oficiais na divulgação da Operação?
# Qual foi o alcance de suas publicações?
# Quais perfis mais interagiram (curtidas, retweets, etc) com os oficiais (outros órgãos e entidades públicas, políticos, influenciadores, pessoas comuns?)
# Em quais localidades tiveram mais ações, segundo os Tweets?

setwd("C:\\Users\\luaoliveira\\Google Drive\\pandemia\\twitter")

#Criando função de higienização do texto
limpar <- function(corpus) {
  stopwords <- read.csv("stopwords.csv", header = TRUE, sep = ";")
  corpuslimpo = tm_map(corpus, content_transformer(tolower))
  corpuslimpo = tm_map(corpuslimpo, removeWords, stopwords$stopwords)
  corpuslimpo = tm_map(corpuslimpo, removePunctuation)
  corpuslimpo = tm_map(corpuslimpo, stripWhitespace)
  corpuslimpo = tm_map(corpuslimpo, removeNumbers)
  return(corpuslimpo)
}


#Timeline das 3 forças e do MD
tmls <- get_timeline(c("DefesaGovBr", "exercitooficial", "marmilbr", "fab_oficial"), n = 3200)

##################################
# SÉRIES TEMPORAIS
##################################

#MD
ts_md <- tmls %>% 
  filter(screen_name == "DefesaGovBr") %>% 
  filter(grepl('covid|COVID|Covid|pandemia|Pandemia|corona|Corona',text)) %>% 
  ts_plot("months", color="#32cd32", size=1) +
  labs(title = "",
       subtitle = "Ministério da Defesa (DefesaGovBr)",
       x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  theme(plot.title=element_text(color = "black", size=12, vjust=0.5, hjust=0.5, family = "Lucida Sans")) +
  theme(plot.subtitle=element_text(color = "black", size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.x=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.y=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(plot.caption=element_text(size=8, vjust=0.5, family = "Lucida Sans")) +
  theme(legend.position = "none")

#EB
ts_eb <- tmls %>% 
  filter(screen_name == "exercitooficial") %>% 
  filter(grepl('covid|COVID|Covid|pandemia|Pandemia|corona|Corona',text)) %>% 
  ts_plot("months", color="#32cd32", size=1) +
  labs(title = "",
       subtitle = "Exército Brasileiro (exercitooficial)",
       x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  theme(plot.title=element_text(color = "black", size=12, vjust=0.5, hjust=0.5, family = "Lucida Sans")) +
  theme(plot.subtitle=element_text(color = "black", size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.x=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.y=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(plot.caption=element_text(size=8, vjust=0.5, family = "Lucida Sans")) +
  theme(legend.position = "none")

#MB
ts_mb <- tmls %>% 
  filter(screen_name == "marmilbr") %>% 
  filter(grepl('covid|COVID|Covid|pandemia|Pandemia|corona|Corona',text)) %>% 
  ts_plot("months", color="#32cd32", size=1) +
  labs(title = "",
       subtitle = "Marinha do Brasil (marmilbr)",
       x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  theme(plot.title=element_text(color = "black", size=12, vjust=0.5, hjust=0.5, family = "Lucida Sans")) +
  theme(plot.subtitle=element_text(color = "black", size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.x=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.y=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(plot.caption=element_text(size=8, vjust=0.5, family = "Lucida Sans")) +
  theme(legend.position = "none")

#FAB
ts_fab <- tmls %>% 
  filter(screen_name == "fab_oficial") %>% 
  filter(grepl('covid|COVID|Covid|pandemia|Pandemia|corona|Corona',text)) %>% 
  ts_plot("months", color="#32cd32", size=1) +
  labs(title = "",
       subtitle = "Força Aérea Brasileira (fab_oficial)",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_minimal() +
  theme(plot.title=element_text(color = "black", size=12, vjust=0.5, hjust=0.5, family = "Lucida Sans")) +
  theme(plot.subtitle=element_text(color = "black", size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.x=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(axis.text.y=element_text(size=10, vjust=0.5, family = "Lucida Sans")) +
  theme(plot.caption=element_text(size=8, vjust=0.5, family = "Lucida Sans")) +
  theme(legend.position = "none")


ts <- ggarrange(ts_md, ts_eb, ts_mb, ts_fab, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
annotate_figure(ts, top = text_grob("Frequência de tweets relacionados à pandemia de COVID-19", face = "bold", size = 14))

##################################
# MENTIONs
##################################

###### MD

#Tweets do MD mencionando covid
tweets_md <- tmls %>% 
  filter(screen_name == "DefesaGovBr") %>% 
  filter(grepl('covid|COVID|Covid|pandemia|Pandemia|corona|Corona',text))

#Isola só a variável título, transforma em character, transforma em corpus e limpa
corpus_mentions_md <- limpar(Corpus(VectorSource(tweets_md$mentions_screen_name)))

#Transforma corpus em Data frame
corpus_mentions_md <- data.frame(text=sapply(corpus_mentions_md, identity), stringsAsFactors=F)

#Tokeniza e gera tabela de frequência de palavras
tokens_mentions_md <- corpus_mentions_md %>%
  unnest_tokens(tokens, text) %>% 
  select(tokens) %>% 
  count(tokens) %>% 
  arrange(-n) %>%
  drop_na()

#Wordcloud
wordcloud2(tokens_mentions_md, 
           size= 3)

###### EB

###### MB

###### FAB



##################################
# LOCALIDADES
##################################


##Baixa limites do Brasil
brasil <- read_country()

#Baixa limites dos estados brasileiros
estados <- read_state(code_state = 'all')

#Baixa limites dos municipios
municipios <- read_municipality(code_muni = "all")





