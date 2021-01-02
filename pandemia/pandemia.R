#Carrega pacotes
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(scales)
library(forcats)
library(treemap)
theme_set(theme_bw())

options(scipen = 999)

#Fixa diretório
setwd("C:\\Users\\Luã Braga\\Desktop\\omd\\pandemia")

#######################################
# Importação e manipulação dos dados
######################################

#Importas bancos
eb <- read.csv("eb.csv", header = TRUE, sep = ";", dec = ",")
head(eb)
glimpse(eb)

mb <- read.csv("mb.csv", header = TRUE, sep = ";", dec = ",")
head(mb)
glimpse(mb)

fab <- read.csv("fab.csv", header = TRUE, sep = ";", dec = ",")
head(fab)
glimpse(fab)


################# EB #####################

#Remove colunas vazias
eb[ ,9] <- NULL

#Padronizando maiúsculas e minúsculas
eb$ug <- str_to_title(eb$ug)
eb$contratado <- str_to_title(eb$contratado)

#Remover cifrões e pontos
eb$valor <- gsub('[R$.]', '', eb$valor)

#Converte valores para numeric (reconhece )
eb$valor <- as.numeric(sub(",", ".", eb$valor, fixed = TRUE))

#Insere coluna Força
eb$forca <- c("Exército Brasileiro")

################# MB #####################

#Remove colunas vazias
mb[ ,12:13] <- NULL

#Padronizando maiúsculas e minúsculas
mb$ug <- str_to_title(mb$ug)
mb$contratado <- str_to_title(mb$contratado)

#Remover cifrões e pontos
mb$valor <- gsub('[R$.]', '', mb$valor)

#Converte valores para numeric (reconhece )
mb$valor <- as.numeric(sub(",", ".", mb$valor, fixed = TRUE))

#Insere coluna Força
mb$forca <- c("Marinha do Brasil")

#Converte datas para date
mb$data <-  as.Date(mb$data, "%m/%d/%Y")


################# FAB #####################

#Remove colunas vazias
fab[ ,13:26] <- NULL

#Padronizando maiúsculas e minúsculas
fab$ug <- str_to_title(fab$ug)
fab$contratado <- str_to_title(fab$contratado)

#Remover cifrões e pontos
fab$valor <- gsub('[R$.]', '', fab$valor)

#Converte valores para numeric (reconhece )
fab$valor <- as.numeric(sub(",", ".", fab$valor, fixed = TRUE))

#Insere coluna Força
fab$forca <- c("Força Aérea Brasileira")

################# UNINDO BANCOS #####################
geral <- bind_rows(eb, mb, fab)
geral$valor <- as.numeric(geral$valor)

################################
# Análises exploratórias
################################

#Estatísticas descritivas do valor das compras
summary(geral$valor) #o máximo está muito distante da média, o que indica outliers

#calculando o desvio padrão do valor das compras
sd(geral$valor)

#Histograma do valor das compras
hist(geral$valor)

#Distribuição do valor das compras
geral %>% 
ggplot(aes(forca, valor, fill=forca)) +
geom_boxplot(varwidth=T, fill="plum") + 
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  labs(title="Distribuição dos valores empenhados para \ncompras emergenciais por Força Armada", 
       subtitle="Os dados apresentam grande variância e indicam que poucas aquisições \nforam responsáveis pela maior parte dos gastos",
       caption="Fonte: Observatório do Ministério da Defesa",
       x=" ",
       y=" ")

#Distribuição nominal de gastos por força armada (NÃO ESTÁ SUMARIZANDO FAB E EB)
forca_freq <- geral %>%
  group_by(forca) %>%
  summarise (total = sum(valor)) %>%
  arrange(desc(total))

forca_freq %>% 
  mutate(forca = fct_reorder(forca, total, .desc = TRUE)) %>%
  ggplot(aes(forca, total, fill=forca)) +
  geom_bar(stat="identity", width = 0.5) + 
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  geom_text(aes(label=total), color="black", size=3, vjust = -0.5) +
  theme(legend.position = "none") +
  labs(title="Total dos valores empenhados para compras emergenciais \npor Força Armada", 
       subtitle="O Exército Brasileiro realizou mais despesas no combate à Covid-19", 
       caption="Fonte: Observatório do Ministério da Defesa",
       x = " ",
       y = " ")

#Distribuição percentual de gastos por força armada







#Valor dos gastos por ug
ug_freq <- geral %>%
  group_by(forca, ug) %>%
  summarise (total = sum(valor)) %>%
  arrange(desc(total))

#Treemap com as 15 primeiras ugs
ug_freq_15 <- ug_freq %>% 
  filter(total > 4156222.59)

treemap(ug_freq_15, #Your data frame object
        index=c("ug"),  #A list of your categorical variables
        vSize = "total",  #This is your quantitative variable
        vColor = "forca", #This is a categorical variable
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = "Set1",  #Select your color palette from the RColorBrewer presets or make your own.
        title="As 15 organizações militares que mais empenharam recursos \npara combate à Covid-19", #Customize your title
        fontsize.title = 14, #Change the font size of the title
        position.legend = "bottom",
        aspRatio = NA,
        title.legend = "",
)


#Valor dos gastos por contratado
contratado_freq <- geral %>%
  group_by(forca, contratado) %>%
  summarise (total = sum(valor)) %>%
  arrange(desc(total))

#Treemap com as 15 primeiras contratados

contratado_freq_15 <- contratado_freq %>% 
  filter(total > 4156222.59)

treemap(contratado_freq_15, #Your data frame object
        index=c("contratado"),  #A list of your categorical variables
        vSize = "total",  #This is your quantitative variable
        vColor = "forca", #This is a categorical variable
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = "Set1",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Os 15 maiores fornecedores das Forças Armadas \nem compras emergenciais para combate à Covid-19", #Customize your title
        fontsize.title = 14, #Change the font size of the title
        position.legend = "bottom",
        aspRatio = NA,
        title.legend = "",
)

#Top 15 contratados por força





