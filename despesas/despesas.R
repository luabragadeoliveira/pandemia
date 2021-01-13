#Carrega pacotes
library(tidyverse)
library(lubridate)
library(highcharter)

options(scipen = 999)

#Fixa diretório
setwd("C:\\Users\\Luã Braga\\Desktop\\projetos\\pandemia\\despesas")


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
         forca = as.factor(c("Exército Brasileiro"))) %>% 
  drop_na("valor","contratado", "ug") #remove linhas que tenham NA na coluna valor

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
         data_empenho = dmy(data_empenho)) %>%
  drop_na("valor","contratado", "ug")

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
         forca = as.factor(c("Força Aérea Brasileira"))) %>% 
  drop_na("valor","contratado", "ug")

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
         forca = as.factor(c("Hospital das Forças Armadas"))) %>% 
  drop_na("valor","contratado")

################# UNINDO BANCOS #####################
geral <- bind_rows(eb, mb, fab, hfa)

################################
# Análises exploratórias
################################

#Estatísticas descritivas do valor das compras
summary(geral$valor) #o máximo está muito distante da média, o que indica outliers

#Boxplot sem outliers
hcboxplot(x = geral$valor,
          var = geral$forca,
          outliers = FALSE,
          tooltip = list(valueDecimals = 2, valuePrefix = "R$"),
          name = "Estatísticas Descritivas",
          color = "black") %>% 
  hc_chart(type = "column") %>%
  hc_title(text = "Distribuição das despesas emergenciais por Força Armada", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "Excluídas as compras de grande vulto (outliers)", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#### GASTOS POR FORÇA

#Tabela de Frequência
forca_freq <- geral %>%
  group_by(forca) %>%
  summarise (total = sum(valor, na.rm = TRUE)) %>%
  arrange(desc(total))

#Barplot
forca_freq %>% 
  hchart('column', 
         hcaes(x = forca, y = total, color = forca),
         tooltip = list(valueDecimals = 2, valuePrefix = "R$"),
         name = "Valor total dos contratos") %>% 
  hc_title(text = "Valor das despesas emergenciais por Força Armada", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))


################################
# Gastos por empresa contratada
################################

#Tabela de Frequência
contratado_freq <- geral %>%
  group_by(contratado, forca) %>%
  summarise (total = sum(valor, na.rm = FALSE)) %>%
  mutate(contratado = as.factor(contratado)) %>% 
  arrange(desc(total))

#Removendo linha que ficou vazia
contratado_freq <- contratado_freq[-c(4),]

#Todas
contratado_freq %>%
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("treemap",
         hcaes(x = contratado, value = total, color = total),
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Principais empresas contratadas", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "Pessoas jurídicas ou físicas contratadas para provisão de produtos e serviços emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))


#EB
contratado_freq %>%
  filter(forca %in% "Exército Brasileiro") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = contratado, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Principais empresas contratadas pelo Exército Brasileiro", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 principais pessoas jurídicas ou físicas contratadas para provisão de produtos e serviços emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#MB
contratado_freq %>%
  filter(forca %in% "Marinha do Brasil") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = contratado, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Principais empresas contratadas pela Marinha do Brasil", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 principais pessoas jurídicas ou físicas contratadas para provisão de produtos e serviços emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#FAB
contratado_freq %>%
  filter(forca %in% "Força Aérea Brasileira") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = contratado, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Principais empresas contratadas pela Força Aérea Brasileira", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 principais pessoas jurídicas ou físicas contratadas para provisão de produtos e serviços emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#HFA
contratado_freq %>%
  filter(forca %in% "Hospital das Forças Armadas") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = contratado, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Principais empresas contratadas pelo Hospital das Forças Armadas", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 principais pessoas jurídicas ou físicas contratadas para provisão de produtos e serviços emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))


################################
# Gastos por Unidade Gestora
################################

#Tabela de Frequência
ug_freq <- geral %>%
  group_by(ug, forca) %>%
  summarise (total = sum(valor, na.rm = FALSE)) %>%
  mutate(ug = as.factor(ug)) %>% 
  drop_na(ug) %>% 
  arrange(desc(total))

#Todas
ug_freq %>%
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("treemap",
         hcaes(x = ug, value = total, color = total),
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Valor das despesas emergenciais por Unidade Gestora", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "Organizações militares que mais realizaram despesas emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#EB
ug_freq %>%
  filter(forca %in% "Exército Brasileiro") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = ug, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Valor das despesas emergenciais por Unidade Gestora do Exército Brasileiro", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 organizações militares do EB que mais realizaram despesas emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#MB
ug_freq %>%
  filter(forca %in% "Marinha do Brasil") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = ug, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Valor das despesas emergenciais por Unidade Gestora da Marinha do Brasil", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 organizações militares da MB que mais realizaram despesas emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

#FAB
ug_freq %>%
  filter(forca %in% "Força Aérea Brasileira") %>% 
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>% 
  hchart("pyramid", 
         hcaes(x = ug, y = total),
         name = "Valor total dos contratos",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Valor das despesas emergenciais por Unidade Gestora da Força Aérea Brasileira", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_subtitle(text = "As 15 organizações militares da FAB que mais realizaram despesas emergenciais para combate à Covid-19", 
              fontFamily = "Mermaid",
              align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))


################################
# Principais materiais FAB
################################

#Tabela de frequência
materiais_fab_freq <- geral %>%
  filter(forca %in% "Força Aérea Brasileira") %>% 
  group_by(material, forca) %>%
  summarise (total = sum(valor, na.rm = FALSE)) %>%
  arrange(desc(total))


#Principais materiais
materiais_fab_freq %>%
  group_by(forca) %>%
  slice_max(total, n = 15) %>%
  ungroup() %>%
  hchart('bar', 
         hcaes(x = material, y = total),
         name = "Valor gasto",
         tooltip = list(valueDecimals = 2, valuePrefix = "R$")) %>% 
  hc_title(text = "Principais produtos ou serviços adquiridos pela Força Aérea Brasileira no combate à Covid-19", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))

################################
# Série Temporal MB
################################

#Preparando banco
mb_tempo <- mb %>% 
  select(data_empenho, valor) %>% 
  group_by(data_empenho) %>%
  summarise (total = sum(valor, na.rm = FALSE)) %>%
  arrange(desc(total)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="data_empenho") 

#Transformando em XTS
mb_tempo <- as.xts(mb_tempo)

#Gráfico
hchart(mb_tempo,
       tooltip = list(valueDecimals = 2, valuePrefix = "R$"),
       name = "Valor gasto") %>% 
  hc_title(text = "Evolução das despesas emergenciais da Marinha do Brasil no combate à Covid-19", 
           style = list(fontWeight = "bold", fontSize = "14px"),
           fontFamily = "Mermaid",
           align = "center") %>% 
  hc_credits(enabled = TRUE, 
             text = "Fonte: Observatório de Ministério da Defesa",
             style = list(fontSize = "10px"),
             fontFamily = "Mermaid") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Valor (R$)"))
