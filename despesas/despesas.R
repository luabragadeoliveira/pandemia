#Pacotes utilizados
library(readr)
library(tidyverse)
library(summarytools)
library(ggstatsplot)
library(car)
library(nortest)
library(dunn.test)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(ggspatial)
library(geojsonio)
library(geobr)
library(ggpubr)

#Desabilita notação científica
options(scipen=999)

#Diretório
setwd("C:\\Users\\luaoliveira\\Desktop\\Pandemia\\despesas")

#### Importa as bases

#Fonte dos dados: https://www.gov.br/compras/pt-br/painel-covid
#Filtros: 
#Ano: 2020
#Modalidade: Dispensa de Licitação
#Poder: Executivo
#Esfera: Federal
#Lei: Lei nº 13079 (Emergência Coronavírus)
#Órgão: Comando do Exército / Comando da Marinha / Comando da Aeronáutica

eb_uasg <- read_csv("eb_uasg.csv")
eb_empenho <- read_csv("eb_empenho.csv")
eb_fornecedor <- read_csv("eb_fornecedor.csv")

mb_uasg <- read_csv("mb_uasg.csv")
mb_empenho <- read_csv("mb_empenho.csv")
mb_fornecedor <- read_csv("mb_fornecedor.csv")

fab_uasg <- read_csv("fab_uasg.csv")
fab_empenho <- read_csv("fab_empenho.csv")
fab_fornecedor <- read_csv("fab_fornecedor.csv")

##### Unifica as bases

## uasg

eb_uasg <- eb_uasg %>% 
  mutate("Força Singular" = "Exército Brasileiro")

mb_uasg <- mb_uasg %>% 
  mutate("Força Singular" = "Marinha do Brasil")

fab_uasg <- fab_uasg %>% 
  mutate("Força Singular" = "Força Aérea Brasileira")

uasg <- bind_rows(eb_uasg, mb_uasg, fab_uasg)

rm(eb_uasg, mb_uasg, fab_uasg)

## Empenho

eb_empenho <- eb_empenho %>% 
  mutate("Força Singular" = "Exército Brasileiro")

mb_empenho <- mb_empenho %>% 
  mutate("Força Singular" = "Marinha do Brasil")

fab_empenho <- fab_empenho %>% 
  mutate("Força Singular" = "Força Aérea Brasileira")

empenho <- bind_rows(eb_empenho, mb_empenho, fab_empenho)

rm(eb_empenho, mb_empenho, fab_empenho)

## Fornecedor

eb_fornecedor <- eb_fornecedor %>% 
  mutate("Força Singular" = "Exército Brasileiro")

mb_fornecedor <- mb_fornecedor %>% 
  mutate("Força Singular" = "Marinha do Brasil")

fab_fornecedor <- fab_fornecedor %>% 
  mutate("Força Singular" = "Força Aérea Brasileira")

fornecedor <- bind_rows(eb_fornecedor, mb_fornecedor, fab_fornecedor)

rm(eb_fornecedor, mb_fornecedor, fab_fornecedor)

######################################################
############# Estatísticas Descritivas ###############
#####################################################

dfSummary(uasg)
dfSummary(empenho)
dfSummary(fornecedor)

### Perguntas
# Quanto foi gasto no total por cada Força?
# Como se deu a distribuição dos gastos de cada Força?
# Quais foram os itens mais adquiridos?
# Quais itens representaram os maiores gastos?
# Quais foram os fornecedores mais contratados?
# Quais foram os fornecedores com maiores valores de contratos?
# Quais uasgs realizaram mais compras?
# Quais uasgs tiveram maiores despesas totais? 
# Como os gastos de cada Força Singular foram distribuídos pelo Brasil?
# Como os gastos evoluíram ao longo do ano?

######################################################
##### Quanto foi gasto no total por cada Força?######
#####################################################

uasg %>% 
  group_by(`Força Singular`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  mutate(`Força Singular` = fct_reorder(`Força Singular`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Força Singular`, Total, fill = `Força Singular`)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen", "darkblue", "darkred")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5)) +
  labs(title = "Valor das despesas emergenciais por Força Armada",
       subtitle = "O Exército Brasileiro realizou mais gastos emergenciais no combate à pandemia em 2020",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=12, vjust=0.5)) +
  theme(axis.text.x=element_text(size=16, vjust=0.5)) +
  theme(axis.text.y=element_text(size=16, vjust=0.5))

#Exército gastou nitidamente mais.

#############################################################
##### Como de deu a distribuição dos gastos cada Força?######
#############################################################

## Estatísticas descritivas

#Geral
descr(fornecedor$`Valor Total do Item`)

#EB
fornecedor %>% 
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  select(`Valor Total do Item`) %>% 
  descr()

#MB
fornecedor %>% 
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  select(`Valor Total do Item`) %>% 
  descr()

#FAB
fornecedor %>% 
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  select(`Valor Total do Item`) %>% 
  descr()

## Aquisições extremamentes concentradas! Nem precisava fazer teste de normalidade e homogeneidade de variâncias, mas vamos ver:

#Testes de normalidade (Shapiro-Wilk e Anderson-Darling)
shapiro.test(fornecedor$`Valor Total do Item`[0:5000])
ad.test(fornecedor$`Valor Total do Item`)

#Testes de homogeneidade de variâncias (Levene e Fligner-Killeen)
leveneTest(`Valor Total do Item` ~ `Força Singular`, data = fornecedor)
fligner.test(`Valor Total do Item` ~ `Força Singular`, data = fornecedor)

## Visualizando e identificando outliers (itens, fonecedores e uasgs)

fornecedor %>% 
  ggbetweenstats(x = `Força Singular`,
                 y = `Valor Total do Item`,
                 type = "np", 
                 outlier.tagging = TRUE,
                 outlier.label = `Descrição do Item`,
                 title = "Distribuição do valor das compras por Força Singular",
                 caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)")

fornecedor %>% 
  ggbetweenstats(x = `Força Singular`,
                 y = `Valor Total do Item`,
                 type = "np", 
                 outlier.tagging = TRUE,
                 outlier.label = `Razão Social`,
                 title = "Distribuição do valor das compras por Força Singular",
                 caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)")

uasg %>% 
  ggbetweenstats(x = `Força Singular`,
                 y = `Valor Total da Compra`,
                 type = "np", 
                 outlier.tagging = TRUE,
                 outlier.label = `Nome da UASG`,
                 title = "Distribuição do valor das compras por Força Singular",
                 caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)")

## Para enxergar melhor as distribuições, vamos reduzir ao percentil 90

quantile(fornecedor$`Valor Total do Item`, probs = 0.90)

fornecedor %>% 
  filter(`Valor Total do Item` < 25270) %>% 
  ggbetweenstats(x = `Força Singular`,
                 y = `Valor Total do Item`,
                 type = "np", 
                 outlier.tagging = TRUE,
                 outlier.label = `Descrição do Item`,
                 title = "Distribuição do valor das compras por Força Singular (percentil 90)",
                 caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)")

#Embora o Exército tenha gastado mais no total, FAB e Marinha tiveram gastos unitários maiores por compra, principalmente a FAB.

##############################################################################################
##### Quais foram os itens mais adquiridos e quais representaram os maiores gastos? ##########
##############################################################################################

## EB

#Itens mais adquiridos
fornecedor %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  count(`Descrição do Item`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, n, .desc = TRUE)) %>%
  ggplot(aes(`Descrição do Item`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Itens mais adquiridos pelo Exército Brasileiro",
       subtitle = "Álcool etílico e luvas de cirurgia foram os itens mais adquiridos pelo EB",
       x = "",
       y = "Compras realizadas",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Itens que representaram os maiores gastos
fornecedor %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Descrição do Item`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Total de despesas por item adquirido pelo Exército Brasileiro",
       subtitle = "Locação de bens móveis representou o maior gasto do EB",
       x = "",
       y = "Custo total do item",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Embora os itens mais adquiridos pelo EB tenham sido álcool e luvas de cirurgia, locação de bens móveis representou os maiores gastos da Força.

## MB

#Itens mais adquiridos
fornecedor %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  count(`Descrição do Item`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, n, .desc = TRUE)) %>%
  ggplot(aes(`Descrição do Item`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkred")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Itens mais adquiridos pela Marinha do Brasil",
       subtitle = "Manutenção e reparo de embarcações foi o serviço mais adquirido pela MB",
       x = "",
       y = "Compras realizadas",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

# Itens que representaram os maiores gastos
fornecedor %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Descrição do Item`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkred")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Total de despesas por item adquirido pela Marinha do Brasil",
       subtitle = "Aventais e reagentes representaram o maior gasto da MB",
       x = "",
       y = "Custo total do item",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Embora manutenção e reparo de embarcações tenham sido os serviços mais adquiridos pela MB, aventais e reagentes representaram os maiores gastos da Força.

## FAB

#Itens mais adquiridos
fornecedor %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  count(`Descrição do Item`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, n, .desc = TRUE)) %>%
  ggplot(aes(`Descrição do Item`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkblue")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Itens mais adquiridos pela Força Aérea Brasileira",
       subtitle = "Álcool etílico foi o item mais adquirido pela FAB",
       x = "",
       y = "Compras realizadas",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Itens que representaram os maiores gastos
fornecedor %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Descrição do Item`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkblue")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Total de despesas por item adquirido pela Força Aérea Brasileira",
       subtitle = "Serviços de engenharia representaram o maior gasto da FAB",
       x = "",
       y = "Custo total do item",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Embora o item mais adquirido pela FAB tenha sido álcool etílico, serviços de engenharia representaram o maior gasto da Força.


##################################################################################################
##### O quanto foi gasto com Cloroquina em comparação a outros medicamentos utilizados? ##########
##################################################################################################

###################### Cloroquina/Hidroxicloroquina, Azitromicina e Ivermectina ####################

#Gastos com cada item do protocolo
fornecedor %>%
  filter(grepl('CLOROQUINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total)

#Gastos totais
fornecedor %>%
  filter(grepl('CLOROQUINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  summarise(Total = sum(Total))

####################################### Antibióticos #################################

fornecedor %>%
  filter(grepl('AMOXICILINA|CLAVULANATO|CEFTRIAXONE|PIPERACILINA|MEROPENEM|AZITROMICINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total)

#Gastos totais
fornecedor %>%
  filter(grepl('AMOXICILINA|CLAVULANATO|CEFTRIAXONE|PIPERACILINA|MEROPENEM|AZITROMICINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  summarise(Total = sum(Total))

#Gráfico comparativo
antibióticos <- fornecedor %>%
  filter(grepl('CLOROQUINA|AMOXICILINA|CLAVULANATO|CEFTRIAXONE|PIPERACILINA|MEROPENEM|AZITROMICINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = FALSE)) %>%
  ggplot(aes(`Descrição do Item`, Total, fill = factor(ifelse(`Descrição do Item`%in% c("CLOROQUINA"), 
                                                              "Cloroquina: R$ 1.431.869,00", 
                                                              "Antibióticos: R$ 171.636,00")))) +
  geom_bar(stat="identity", width=0.5) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  scale_fill_manual(name = "", values = c("darkgray", "#32cd32")) +
  labs(title = "Antibióticos",
       x = "") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, hjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5)) +
  theme(legend.position = c(0.6, 0.2),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x=element_blank()) +
  coord_flip()

####################################### Sedativos #################################

fornecedor %>%
  filter(grepl('MIDAZOLAM|FENTANIL|PROPOFOL|KETAMINA|LIDOCAÍNA|ROCURÔNIO|ATRACÚRIO|SUCCINILCOLINA|SUGAMADEX|ETOMIDATO|DEXMEDETOMIDINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total)

#Gastos totais
fornecedor %>%
  filter(grepl('MIDAZOLAM|FENTANIL|PROPOFOL|KETAMINA|LIDOCAÍNA|ROCURÔNIO|ATRACÚRIO|SUCCINILCOLINA|SUGAMADEX|ETOMIDATO|DEXMEDETOMIDINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  summarise(Total = sum(Total))

#Gráfico comparativo
sedativos <- fornecedor %>%
  filter(grepl('CLOROQUINA|MIDAZOLAM|FENTANIL|PROPOFOL|KETAMINA|LIDOCAÍNA|ROCURÔNIO|ATRACÚRIO|SUCCINILCOLINA|SUGAMADEX|ETOMIDATO|DEXMEDETOMIDINA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = FALSE)) %>%
  ggplot(aes(`Descrição do Item`, Total, fill = factor(ifelse(`Descrição do Item`%in% c("CLOROQUINA"), 
                                                              "Cloroquina: R$ 1.431.869,00", 
                                                              "Sedativos: R$ 3.947.818")))) +
  geom_bar(stat="identity", width=0.5) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  scale_fill_manual(name = "", values = c("#32cd32", "darkgray")) +
  labs(title = "Sedativos",
       x = "") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, hjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5)) +
  theme(legend.position = c(0.6, 0.2),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x=element_blank()) +
  coord_flip()

####################################### Anticoagulantes #################################

fornecedor %>%
  filter(grepl('ENOXAPARINA|CLEXANE|HEPERINA|VARFARINA|ELIQUIS|PRADAZA|XARELTO', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total)

#Gastos totais
fornecedor %>%
  filter(grepl('ENOXAPARINA|CLEXANE|HEPERINA|VARFARINA|ELIQUIS|PRADAZA|XARELTO', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  summarise(Total = sum(Total))

#Gráfico comparativo
anticoagulantes <- fornecedor %>%
  filter(grepl('CLOROQUINA|ENOXAPARINA|CLEXANE|HEPERINA|VARFARINA|ELIQUIS|PRADAZA|XARELTO', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = FALSE)) %>%
  ggplot(aes(`Descrição do Item`, Total, fill = factor(ifelse(`Descrição do Item`%in% c("CLOROQUINA"), 
                                                              "Cloroquina: R$ 1.431.869,00", 
                                                              "Anticoagulantes: R$ 883.832,00")))) +
  geom_bar(stat="identity", width=0.5) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  scale_fill_manual(name = "", values = c("darkgray", "#32cd32")) +
  labs(title = "Anticoagulantes",
       x = "") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, hjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5)) +
  theme(legend.position = c(0.6, 0.2),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x=element_blank()) +
  coord_flip()

####################################### Corticoides #################################

fornecedor %>%
  filter(grepl('DEXAMETASONA|PREDNISONA|CORTISONA|PREDNISOLONA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total)

#Gastos totais
fornecedor %>%
  filter(grepl('DEXAMETASONA|PREDNISONA|CORTISONA|PREDNISOLONA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  summarise(Total = sum(Total))

#Gráfico comparativo
corticóides <- fornecedor %>%
  filter(grepl('CLOROQUINA|DEXAMETASONA|PREDNISONA|CORTISONA|PREDNISOLONA', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = FALSE)) %>%
  ggplot(aes(`Descrição do Item`, Total, fill = factor(ifelse(`Descrição do Item`%in% c("CLOROQUINA"), 
                                                              "Cloroquina: R$ 1.431.869,00", 
                                                              "Corticóides: R$ 27.640,00")))) +
  geom_bar(stat="identity", width=0.5) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  scale_fill_manual(name = "", values = c("#32cd32", "darkgray")) +
  labs(title = "Corticóides",
       x = "") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, hjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5)) +
  theme(legend.position = c(0.6, 0.2),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x=element_blank()) +
  coord_flip()

####################################### Oxigênio #################################

fornecedor %>%
  filter(grepl('OXIGÊNIO', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total)

#Gastos totais
fornecedor %>%
  filter(grepl('OXIGÊNIO', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  summarise(Total = sum(Total))

#Gráfico comparativo
oxigênio <- fornecedor %>%
  filter(grepl('CLOROQUINA|OXIGÊNIO', `Descrição do Item`)) %>%   
  group_by(`Descrição do Item`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  mutate(`Descrição do Item` = fct_reorder(`Descrição do Item`, Total, .desc = FALSE)) %>%
  ggplot(aes(`Descrição do Item`, Total, fill = factor(ifelse(`Descrição do Item`%in% c("CLOROQUINA"), 
                                                              "Cloroquina: R$ 1.431.869,00", 
                                                              "Oxigênio: R$ 391.431,00")))) +
  geom_bar(stat="identity", width=0.5) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  scale_fill_manual(name = "", values = c("#32cd32", "darkgray")) +
  labs(title = "Oxigênio",
       x = "") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, hjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5)) +
  theme(legend.position = c(0.6, 0.2),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x=element_blank()) +
  coord_flip()

##################### Combina todos #####################

ggarrange(antibióticos, anticoagulantes, corticóides, oxigênio, sedativos)

#########################################################################################################
##### Quais foram os fornecedores mais contratados e quais representaram os maiores contratos? ##########
#########################################################################################################

## EB

#Fornecedores mais contratados
fornecedor %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  count(`Razão Social`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Razão Social` = fct_reorder(`Razão Social`, n, .desc = TRUE)) %>%
  ggplot(aes(`Razão Social`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Fornecedores mais contratados pelo Exército Brasileiro",
       subtitle = "Fabiana Aguiar da Costa da Silva e Ágora Produção de Eventos foram os fornecedores mais contratados pelo EB",
       x = "",
       y = "Contratos realizados",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Fornecedores que representaram os maiores gastos
fornecedor %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  group_by(`Razão Social`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Razão Social` = fct_reorder(`Razão Social`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Razão Social`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Fornecedores que representaram os maiores contratos com o Exército Brasileiro",
       subtitle = "A empresa Ágora Produção de Eventos representou os maiores contratos com o EB",
       x = "",
       y = "Valor total contratado",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Fabiana Aguiar da Costa da Silva realizou o maior número de contratos com o EB, seguida da empresa Ágora Produção de Eventos. Essa última, entretanto, foi a que obteve contratos com os maiores valores.

## MB

#Fornecedores mais contratados
fornecedor %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  count(`Razão Social`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Razão Social` = fct_reorder(`Razão Social`, n, .desc = TRUE)) %>%
  ggplot(aes(`Razão Social`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkred")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Fornecedores mais contratados pela Marinha do Brasil",
       subtitle = "A empresa Medisil foi o fornecedor mais contratado pela MB",
       x = "",
       y = "Contratos realizados",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Fornecedores que representaram os maiores gastos
fornecedor %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  group_by(`Razão Social`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Razão Social` = fct_reorder(`Razão Social`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Razão Social`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkred")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Fornecedores que representaram os maiores contratos com a Marinha do Brasil",
       subtitle = "A empresa Lang e Filhos representou os maiores contratos com a MB",
       x = "",
       y = "Valor total contratado",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Embora a empresa Medisil tenha sido o fornecedor que mais realizou contratos com a MB, a empresa Lang e Filho foi responsável pelos maiores valores em contratos.

## FAB

#Fornecedores mais contratados
fornecedor %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  count(`Razão Social`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Razão Social` = fct_reorder(`Razão Social`, n, .desc = TRUE)) %>%
  ggplot(aes(`Razão Social`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkblue")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Fornecedores mais contratados pela Força Aérea Brasileira",
       subtitle = "A empresa Vila Gugua Carnes foi o fornecedor mais contratado pela FAB",
       x = "",
       y = "Contratos realizados",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Fornecedores que representaram os maiores gastos
fornecedor %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  group_by(`Razão Social`) %>% 
  summarise(Total = sum(`Valor Total do Item`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Razão Social` = fct_reorder(`Razão Social`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Razão Social`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkblue")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Fornecedores que representaram os maiores contratos com a Força Aérea Brasileira",
       subtitle = "A empresa Acácia Construções e Serviços representou os maiores contratos com a FAB",
       x = "",
       y = "Valor total contratado",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Embora a empresa Vila Gugu Carnes tenha sido o fornecedor que mais realizou contratos com a FAB, a empresa Acácia Construções e Serviços foi responsável pelos maiores valores em contratos.

#########################################################################################################
##### Quais uasgs realizaram mais compras e quais tiveram maiores despesas totais? ######################
#########################################################################################################

## EB

#uasgs que realizaram mais compras
uasg %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  count(`Nome da UASG`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Nome da UASG` = fct_reorder(`Nome da UASG`, n, .desc = TRUE)) %>%
  ggplot(aes(`Nome da UASG`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Unidades do Exército Brasileiro que realizaram mais compras",
       subtitle = "Os Hospitais Militares de São Paulo e de Porto Alegre foram as unidades que mais realizaram compras no EB",
       x = "",
       y = "Contratos realizados",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#uasgs com maiores despesas totais
uasg %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  group_by(`Nome da UASG`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Nome da UASG` = fct_reorder(`Nome da UASG`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Nome da UASG`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkgreen")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Unidades do Exército Brasileiro com maiores despesas totais",
       subtitle = "O Comando Logístico e o Comando Militar da Amazônia foram as unidades do EB com maiores despesas totais",
       x = "",
       y = "Valor total contratado",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#Os Hospitais Militares de São Paulo e de Porto Alegre foram as unidades que mais realizaram compras, mas o Comando Logístico e o Comando Militar da Amazônia foram as unidades do EB com maiores despesas totais.

## MB

#uasgs que realizaram mais compras
uasg %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  count(`Nome da UASG`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Nome da UASG` = fct_reorder(`Nome da UASG`, n, .desc = TRUE)) %>%
  ggplot(aes(`Nome da UASG`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkred")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Unidades da Marinha do Brasil que realizaram mais compras",
       subtitle = "A Base de Fuzileiros Navais do Rio Meriti e o Hospital Naval de Salvador foram as unidades que mais realizaram compras na MB",
       x = "",
       y = "Contratos realizados",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#uasgs com maiores despesas totais
uasg %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  group_by(`Nome da UASG`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Nome da UASG` = fct_reorder(`Nome da UASG`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Nome da UASG`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkred")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Unidades da Marinha do Brasil com maiores despesas totais",
       subtitle = "A Diretoria de Abastecimento da Marinha foi a unidade naval com maiores despesas totais",
       x = "",
       y = "Valor total contratado",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#A Base de Fuzileiros Navais do Rio Meriti e o Hospital Naval de Salvador foram as unidades que mais realizaram compras na MB, mas a Diretoria de Abastecimento da Marinha foi a unidade naval com maiores despesas totais.

## FAB

#uasgs que realizaram mais compras
uasg %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  count(`Nome da UASG`) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 15) %>% 
  mutate(`Nome da UASG` = fct_reorder(`Nome da UASG`, n, .desc = TRUE)) %>%
  ggplot(aes(`Nome da UASG`, n)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkblue")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Unidades da Força Aérea Brasileira que realizaram mais compras",
       subtitle = "O Grupamento de Apoio de São Paulo foi a unidade que mais realizou compras na FAB",
       x = "",
       y = "Contratos realizados",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#uasgs com maiores despesas totais
uasg %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  group_by(`Nome da UASG`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  arrange(-Total) %>% 
  slice_max(Total, n = 15) %>% 
  mutate(`Nome da UASG` = fct_reorder(`Nome da UASG`, Total, .desc = TRUE)) %>%
  ggplot(aes(`Nome da UASG`, Total)) +
  geom_bar(stat="identity", width=0.5, fill = c("darkblue")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$")) +
  scale_x_discrete(labels = label_wrap(width = 5))  +
  labs(title = "Unidades da Força Aérea Brasileira com maiores despesas totais",
       subtitle = "O Centro de Aquisições Específicas foi a unidade da FAB com maiores despesas totais",
       x = "",
       y = "Valor total contratado",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_classic() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=14, vjust=0.5))

#O Grupamento de Apoio de São Paulo foi a unidade que mais realizou compras na FAB, mas o Centro de Aquisições Específicas foi a unidade da FAB com maiores despesas totais.

#########################################################################################################
####### Como as compras e os gastos de cada Força Singular foram distribuídos pelo Brasil? ##############
#########################################################################################################

##Edita coluna Município/UF para conter somente a UF
uasg$`Município/UF` <- str_sub(uasg$`Município/UF`, start = -2) 

##Baixa limites do Brasil
brasil <- read_country()

##Baixa limites dos estados brasileiros
estados <- read_state(code_state = 'all')

#Analisa dados
head(brasil)
head(estados)

##EB: compras

#Tabela de frequência (já renomeando a variável)
eb_uf_compras <- uasg %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  count(`Município/UF`) %>% 
  rename(abbrev_state = `Município/UF`)

#Junta no DF dos estados e renomeia N
eb_uf_compras <- inner_join(estados, eb_uf_compras) %>% 
  rename("Total de compras realizadas" = n)

#Inserindo parâmetros no mapa
ggplot(eb_uf_compras) +
  geom_sf(aes(fill = `Total de compras realizadas`, col = `Total de compras realizadas`)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Greens")) +
  scale_color_gradientn (colours = brewer.pal(9, "Greens")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Concentração de compras do Exército Brasileiro por estado",
       subtitle = "Houve maior concentração de compras no estado do Rio Grande do Sul",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))

##EB: gastos

#Tabela de frequência (já renomeando a variável)
eb_uf_gastos <- uasg %>%
  filter(`Força Singular` == "Exército Brasileiro") %>% 
  group_by(`Município/UF`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  rename(abbrev_state = `Município/UF`)

#Junta no DF dos estados e renomeia N
eb_uf_gastos <- inner_join(estados, eb_uf_gastos) %>% 
  rename("Despesas Totais" = Total)

#Inserindo parâmetros no mapa
ggplot(eb_uf_gastos) +
  geom_sf(aes(fill = `Despesas Totais`, col = `Despesas Totais`)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Greens")) +
  scale_color_gradientn (colours = brewer.pal(9, "Greens")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Concentração de gastos do Exército Brasileiro por estado",
       subtitle = "Houve maior concentração de gastos nos estados do Amazonas e do Distrito Federal",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))

##MB: compras

#Tabela de frequência (já renomeando a variável)
mb_uf_compras <- uasg %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  count(`Município/UF`) %>% 
  rename(abbrev_state = `Município/UF`)

#Junta no DF dos estados e renomeia N
mb_uf_compras <- inner_join(estados, mb_uf_compras) %>% 
  rename("Total de compras realizadas" = n)

#Inserindo parâmetros no mapa
ggplot(mb_uf_compras) +
  geom_sf(aes(fill = `Total de compras realizadas`, col = `Total de compras realizadas`)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Reds")) +
  scale_color_gradientn (colours = brewer.pal(9, "Reds")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Concentração de compras da Marinha do Brasil por estado",
       subtitle = "Houve maior concentração de compras no estado do Rio de Janeiro",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))

##MB: gastos

#Tabela de frequência (já renomeando a variável)
mb_uf_gastos <- uasg %>%
  filter(`Força Singular` == "Marinha do Brasil") %>% 
  group_by(`Município/UF`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  rename(abbrev_state = `Município/UF`)

#Junta no DF dos estados e renomeia N
mb_uf_gastos <- inner_join(estados, mb_uf_gastos) %>% 
  rename("Despesas Totais" = Total)

#Inserindo parâmetros no mapa
ggplot(mb_uf_gastos) +
  geom_sf(aes(fill = `Despesas Totais`, col = `Despesas Totais`)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Reds")) +
  scale_color_gradientn (colours = brewer.pal(9, "Reds")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Concentração de gastos da Marinha do Brasil por estado",
       subtitle = "Houve maior concentração de gastos no estado do Rio de Janeiro",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))

##FAB: compras

#Tabela de frequência (já renomeando a variável)
fab_uf_compras <- uasg %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  count(`Município/UF`) %>% 
  rename(abbrev_state = `Município/UF`)

#Junta no DF dos estados e renomeia N
fab_uf_compras <- inner_join(estados, fab_uf_compras) %>% 
  rename("Total de compras realizadas" = n)

#Inserindo parâmetros no mapa
ggplot(fab_uf_compras) +
  geom_sf(aes(fill = `Total de compras realizadas`, col = `Total de compras realizadas`)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Blues")) +
  scale_color_gradientn (colours = brewer.pal(9, "Blues")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Concentração de compras da Força Aérea Brasileira por estado",
       subtitle = "Houve maior concentração de compras no estado de São Paulo",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))

##FAB: gastos

#Tabela de frequência (já renomeando a variável)
fab_uf_gastos <- uasg %>%
  filter(`Força Singular` == "Força Aérea Brasileira") %>% 
  group_by(`Município/UF`) %>% 
  summarise(Total = sum(`Valor Total da Compra`)) %>% 
  rename(abbrev_state = `Município/UF`)

#Junta no DF dos estados e renomeia N
fab_uf_gastos <- inner_join(estados, fab_uf_gastos) %>% 
  rename("Despesas Totais" = Total)

#Inserindo parâmetros no mapa
ggplot(fab_uf_gastos) +
  geom_sf(aes(fill = `Despesas Totais`, col = `Despesas Totais`)) + #fill = preenchimento, col = contorno
  geom_sf(data = brasil, fill = "transparent", colour = "black") +
  scale_fill_gradientn (colours = brewer.pal(9, "Blues")) +
  scale_color_gradientn (colours = brewer.pal(9, "Blues")) +
  annotation_scale(location = "br") + #insere escala ("bl" = bottom right = canto direito inferior) +
  annotation_north_arrow (location = "tr", 
                          style = north_arrow_nautical()) + #insere orientação ("tr" = top right = canto direito superior)
  labs(title = "Concentração de gastos da Força Aérea Brasileira por estado",
       subtitle = "Houve maior concentração de gastos no estado do Rio de Janeiro",
       x = "",
       y = "",
       caption = "Fonte: Observatório do Ministério da Defesa (https://observatoriomd.irid.ufrj.br)") +
  theme_void() +
  theme(plot.title=element_text(color = "black", size=20, vjust=0.5, face = "bold")) +
  theme(plot.subtitle=element_text(color = "black", size=18, vjust=0.5)) +
  theme(plot.caption=element_text(color = "black", size=8, vjust=0.5))
