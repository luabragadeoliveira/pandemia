#Carrega pacotes
library(tidyverse)
library(scales)
library(lubridate)
library(treemap)
library(rgdal)

theme_set(theme_bw())
options(scipen = 999)

#Fixa diretório
setwd("C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia")

#######################################
# Importação e manipulação dos dados
######################################

################# EB #####################

eb <- read.csv("eb.csv", header = TRUE, sep = ";", dec = ",")
head(eb)
glimpse(eb)

#Seleciona somente colunas desejadas
#Padronizando maiúsculas e minúsculas
#Remover cifrões e pontos
#Converte valores para numeric
#Insere coluna Força
eb <- eb %>% 
  select(ug, contratado, valor) %>% 
  mutate(ug = as.factor(str_to_title(ug)),
         contratado = as.factor(str_to_title(contratado)),
         valor = gsub('[R$.]', '', valor),
         valor = as.numeric(as.character(sub(",", ".", valor, fixed = TRUE))),
         forca = as.factor(c("Exército Brasileiro"))) 

################# MB #####################

mb <- read.csv("mb.csv", header = TRUE, sep = ";", dec = ",")
head(mb)
glimpse(mb)

#Seleciona somente colunas desejadas
#Renomeia coluna "obs" para "material"
#Padronizando maiúsculas e minúsculas
#Remover cifrões e pontos
#Converte valores para numeric
#Insere coluna Força
#Trnasforma coluna de data em date
mb <- mb %>% 
  select(ug, contratado, valor, data_empenho, obs) %>% 
  rename(material = obs) %>% 
  mutate(ug = as.factor(str_to_title(ug)),
         contratado = as.factor(str_to_title(contratado)),
         material = as.factor(str_to_title(material)),
         valor = gsub('[R$.]', '', valor),
         valor = as.numeric(as.character(sub(",", ".", valor, fixed = TRUE))),
         forca = as.factor(c("Marinha do Brasil")),
         data_empenho = as.Date(data_empenho, "%d/%m/%Y")) 


################# FAB #####################

fab <- read.csv("fab.csv", header = TRUE, sep = ";", dec = ",")
head(fab)
glimpse(fab)

#Seleciona somente colunas desejadas
#Renomeia coluna objeto para material
#Padronizando maiúsculas e minúsculas
#Remover cifrões e pontos
#Converte valores para numeric
#Insere coluna Força
fab <- fab %>% 
  select(ug, contratado, valor, objeto) %>% 
  rename(material = objeto) %>% 
  mutate(ug = as.factor(str_to_title(ug)),
         contratado = as.factor(str_to_title(contratado)),
         material = as.factor(str_to_title(material)),
         valor = gsub('[R$.]', '', valor),
         valor = as.numeric(as.character(sub(",", ".", valor, fixed = TRUE))),
         forca = as.factor(c("Força Aérea Brasileira")))

################# FAB #####################

hfa <- read.csv("hfa.csv", header = TRUE, sep = ";", dec = ",")
head(hfa)
glimpse(hfa)

#Seleciona somente colunas desejadas
#Renomeia coluna objeto para material
#Padronizando maiúsculas e minúsculas
#Remover cifrões e pontos
#Converte valores para numeric
#Insere coluna Força
hfa <- hfa %>% 
  select(contratado, valor) %>% 
  mutate(contratado = as.factor(str_to_title(contratado)),
         valor = gsub('[R$.]', '', valor),
         valor = as.numeric(as.character(sub(",", ".", valor, fixed = TRUE))),
         forca = as.factor(c("Hospital das Forças Armadas")))

################# UNINDO BANCOS #####################
geral <- bind_rows(eb, mb, fab, hfa)

glimpse(geral)


################################
# Análises exploratórias
################################

#Estatísticas descritivas do valor das compras
summary(geral$valor) #o máximo está muito distante da média, o que indica outliers

#calculando o desvio padrão do valor das compras
sd(geral$valor)

#Histograma do valor das compras
hist(geral$valor)

#Distribuição do valor das compras (Boxplot)
geral %>% 
ggplot(aes(forca, valor, fill=forca)) +
geom_boxplot(varwidth=T, fill="plum") + 
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  labs(title=" ", 
       subtitle=" ",
       caption=" ",
       x=" ",
       y=" ")

#Retirando as compras muito grandes que distorcem os dados
geral %>% 
  filter(valor < 10000) %>% 
  ggplot(aes(forca, valor, fill=forca)) +
  geom_boxplot(varwidth=T, show.legend = FALSE) + 
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  labs(title=" ", 
       subtitle=" ",
       caption=" ",
       x=" ",
       y=" ")

#### GASTOS POR FORÇA

#Tabela de Frequência
forca_freq <- geral %>%
  group_by(forca) %>%
  summarise (total = sum(valor, na.rm = TRUE)) %>%
  arrange(desc(total))

#Barplot
forca_freq %>% 
  mutate(forca = fct_reorder(forca, total, .desc = TRUE)) %>%
  ggplot(aes(forca, total, fill=forca)) +
  geom_col(width = 0.5) + 
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  geom_text(aes(label=total), color="black", size=3, vjust = -0.5) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title=" ", 
       subtitle=" ", 
       caption=" ",
       x = " ",
       y = " ")

#### GASTOS POR CONTRATADO POR FORÇA

#Tabela de Frequência
contratado_freq <- geral %>%
  group_by(contratado, forca) %>%
  summarise (total = sum(valor, na.rm = FALSE)) %>%
  arrange(desc(total))

#Simplifica nomes removendo strings terminados em Ltda, S/A, SA, Eireli, etc e pontos e traços
contratado_freq$contratado <- contratado_freq$contratado %>% 
  str_replace_all("Sa$|S.a$|S.a.$|S/A$|Ltda$|Ltda.$|Eireli$", " ") %>% 
  str_replace_all(fixed(".|-"), "")

#Barplot
contratado_freq %>%
  group_by(forca) %>%
  slice_max(total, n = 10) %>%
  ungroup() %>%
  ggplot(aes(total, fct_reorder(contratado, total), fill = forca)) +
  scale_x_continuous(labels = label_dollar(prefix = "R$")) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~forca, ncol = 1, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title= " ",
       subtitle = " ",
       caption=" ",
       y = "",
       x = " ")

#Treemap
contratado_freq_top <- contratado_freq %>%
  group_by(forca) %>%
  slice_max(total, n = 10) %>%
  ungroup()

treemap(contratado_freq_top, #Your data frame object
        index=c("contratado"),  #A list of your categorical variables
        vSize = "total",  #This is your quantitative variable
        vColor = "forca", #This is a categorical variable
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = "Set1",  #Select your color palette from the RColorBrewer presets or make your own.
        title=" ", #Customize your title
        fontsize.title = 14, #Change the font size of the title
        fontsize.labels	= 10,
        position.legend = "bottom",
        aspRatio = NA,
        title.legend = "",
)


#### GASTOS POR UG POR FORÇA

#Tabela de Frequência
ug_freq <- geral %>%
  group_by(ug, forca) %>%
  summarise (total = sum(valor, na.rm = TRUE)) %>%
  arrange(desc(total))

#Barplot
ug_freq %>%
  group_by(forca) %>%
  filter(forca %in% c("Marinha do Brasil", "Força Aérea Brasileira", "Exército Brasileiro")) %>% 
  slice_max(total, n = 10) %>%
  ungroup() %>%
  ggplot(aes(total, fct_reorder(ug, total), fill = forca)) +
  scale_x_continuous(labels = label_dollar(prefix = "R$")) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~forca, ncol = 3, scales = "free") +
  coord_flip () +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title= " ",
       subtitle = " ",
       caption=" ",
       y = "",
       x = " ")

#Treemap
ug_freq_top <- ug_freq %>%
  group_by(forca) %>%
  slice_max(total, n = 10) %>%
  ungroup()

treemap(ug_freq_top, #Your data frame object
        index=c("ug"),  #A list of your categorical variables
        vSize = "total",  #This is your quantitative variable
        vColor = "forca", #This is a categorical variable
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = "Set1",  #Select your color palette from the RColorBrewer presets or make your own.
        title=" ", #Customize your title
        fontsize.title = 14, #Change the font size of the title
        fontsize.labels	= 8,
        position.legend = "bottom",
        aspRatio = NA,
        title.legend = "",
)


### MAPA DE GASTOS POR ESTADO

#Exportando tabela de frequência de UG, para serem inseridas as localidades pela equipe
write.csv(ug_freq,"C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\ug_freq.csv", row.names = FALSE)

#Importando shapefile
shp <- readOGR("C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\mapa", "BR_Municipios_2019", stringsAsFactors=FALSE, encoding="UTF-8")

#Importando DF com localidades
regional <- read.csv("C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\regional.csv", header=T,sep=";")

#Juntando à tabela de frequência de ugs
ug_regional <- left_join(regional, ug_freq)


#### SÉRIE TEMPORAL DE GASTOS DA MARINHA



#### PRINCIPAIS MATERIAIS FAB

#Tabela de frequência
materiais_fab_freq <- geral %>%
  filter(forca %in% "Força Aérea Brasileira") %>% 
  group_by(material) %>%
  summarise (total = sum(valor, na.rm = FALSE)) %>%
  arrange(desc(total))

#Simplifica nomes removendo a frase "Serviço De Engenharia Para"
materiais_fab_freq$material <- materiais_fab_freq$material %>% 
  str_replace_all("Serviço De Engenharia Para", "")

#Barplot
materiais_fab_freq %>%
  slice_max(total, n = 10) %>%
  ggplot(aes(total, fct_reorder(material, total), fill = material)) +
  scale_x_continuous(labels = label_dollar(prefix = "R$")) +
  geom_col(show.legend = FALSE) +
  labs(title= " ",
       subtitle = " ",
       caption=" ",
       y = "",
       x = " ")


#### PRINCIPAIS MATERIAIS MARINHA

