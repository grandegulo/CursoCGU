#verificando o diretório atual:
getwd()

#Os dados estarão em D:/CguData. Vamos alterar o diretório:
setwd('D:/CguData/PpGrupo2')

#Instalar as bibliotecas
install.packages('plyr')
install.packages("readxl")
install.packages('dplyr')
install.packages("readr")
install.packages("writexl")
#O R pediu essa biblioteca ... Instalei
install.packages("rlang")
install.packages("vctrs")

#Carregar a biblioteca de leitura do Excel:
library(readxl)

#carregando os dados da tabela completa
TabelaCompleta <- read_excel("TabelaCOMPLETA.xlsx", 
                             sheet = "TabelaCOMPLETA", na = "null")

#visualizando a Tabela
View(TabelaCompleta)

#Sumario Estatistico:
summary(TabelaCompleta)

#Carregar a Tabela PopulacaoIBGE
#carrega a biblioteca com ocomando read_delim
library(readr)

PopulacaoIBGE <- read_delim("PopulacaoIBGE.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
#Visualizar a Tabela
View(PopulacaoIBGE)

#Apagar todas as linhas com pelo menos um "NA" ou "sujeira"
PopulacaoIBGE <- na.omit(PopulacaoIBGE)
View(PopulacaoIBGE)

colnames(TabelaCompleta)

MunPedAno<-data.frame(TabelaCompleta$IDMunicipio,TabelaCompleta$IdPedido,TabelaCompleta$TipoDemandante,TabelaCompleta$Pais,TabelaCompleta$UF,TabelaCompleta$Municipio,TabelaCompleta$DataRegistro)
View(MunPedAno)

#Agora vou renomear as colunas, retirando a referência à TabelaCompleta
library(dplyr)
colnames(MunPedAno)
MunPedAno<-MunPedAno%>% rename( IDMunicipio = TabelaCompleta.IDMunicipio,
                                TipoDemandante = TabelaCompleta.TipoDemandante,
                                IdPedido = TabelaCompleta.IdPedido,
                                DataRegistro = TabelaCompleta.DataRegistro,
                                Pais = TabelaCompleta.Pais,
                                UF = TabelaCompleta.UF,
                                Municipio = TabelaCompleta.Municipio)
                                

View(MunPedAno)

#Para agregar os pedidos por ano, precisamos do grão "ano"
#Vamos Extrair o Ano da Data de Registro
# Primeiro, verificar que tipo de dado é:
class(MunPedAno$DataRegistro)

#importar o pacote stringr
install.packages('stringr')

#chamar o pacote stringr
library(stringr)
#extrair apenas os quatro primeiros caracteres e armazenar em nova coluna
MunPedAno["Ano"]<-str_sub(MunPedAno$DataRegistro, end = 4)

#apaga a coluna data registro:
MunPedAno$DataRegistro<-NULL

#visualiza o dataset
View (MunPedAno)
colnames(MunPedAno)

#Filtrar só pedidos do Brasil:
library ('dplyr')
detach ('plyr')
#Há linhas com Municipio NA e Pais Brasil?
#MunPedAno%>% filter(Pais=='Brasil'& Municipio==NA)
BrasiSemMun<-MunPedAno%>% filter(Pais=='Brasil'& is.na(Municipio))
library(writexl)
write_xlsx(BrasiSemMun,"D:\\CguData\\BrasiSemMun_Mun_NA_pais_Brasil.xlsx")

#Há linhas com Municipio diferente de NA  e Pais diferente de Brasil?
EstrangComMun<-MunPedAno%>% filter(Pais!='Brasil'& !is.na(Municipio))
write_xlsx(EstrangComMun,"D:\\CguData\EstrangComMun_Mun_naoNA_pais_Estrangeiro.xlsx")

#Subset com apenas pessoas físicas:
MunPedAno<-MunPedAno%>% filter(TipoDemandante=='Pessoa Física')
View(MunPedAno)

#Subset desconsiderando pedidos sem municípios
MunPedAno<-MunPedAno%>% filter(!is.na(Municipio))
View(MunPedAno)

#subset desconsiderando Estrangeiros sem municípios
MunPedAno<-MunPedAno%>% filter(!is.na(Municipio))
View(MunPedAno)
#Nesse Dataset, vamos incluir a populacao
MunPedAno<-left_join(MunPedAno,PopulacaoIBGE,by = c("IDMunicipio"="Cód."),copy = FALSE)
View (MunPedAno)



#Agora vamos tratamos o dataset remanescente, mas não vamos juntar. É preciso depurar a homonímia
#Colocar código nos municípios que não tem código informado, cruzando com a tabela de PopulacaoIBGE
#Os municípios que não tiverem código serão descartados
View(PopulacaoIBGE)
#O nome de Municipio da Tabela de pedidos está desacompanhado da UF ...
# O nome de Municipio  da Tabela de Populacao, tem UF (SP)
#Verificar se há Municipio sem UF na Tabela de Pedidos:
MunSemUf<-MunPedAno%>% filter(is.na(UF))
#Há 949 Municipios. Vamos normalizar pela Tabela de PopulacaoIBGE
library(stringr)
#Com essa biblioteca, vamos criar colunas de UF e Municipio separados
PopulacaoIBGE['UF']<-str_sub(PopulacaoIBGE$Município, start =-4)
PopulacaoIBGE['Municipio_sem_UF']<-str_extract(PopulacaoIBGE$Município,"^[:alpha:].*\\s")
PopulacaoIBGE['Municipio_sem_UF']<-str_trim(PopulacaoIBGE$Municipio_sem_UF)
View(PopulacaoIBGE)
View (MunSemUf)
MunSemUf<-left_join(MunSemUf,PopulacaoIBGE,by = c("Municipio"="Municipio_sem_UF"),copy = FALSE)
View (MunSemUf)
#Retirar os parênteses da UFy
# o parâmetro "fixed" é para não considerar regex "("
uefe=MunSemUf$UF.y
uefe= str_replace_all(uefe,fixed("(")," ")
MunSemUf$UF.y= str_replace_all(uefe,fixed(")")," ")
View(MunSemUf)
#Agora vamos deixar com a mesma aparencia do Dataset de origem:

MunSemUf<- MunSemUf%>%
  select(IDMunicipio,IdPedido,TipoDemandante,Pais,UF.y,Municipio,Ano,Municipio,Populacao.y)
MunSemUf<- MunSemUf%>% rename(UF = UF.y)
MunSemUf<- MunSemUf%>% rename(Populacao = Populacao.y)
View(MunSemUf)

#A tabela MunPedAno tem uma coluna "Município" a mais. Vamos eliminá-la
Munpedano<-MunPedAno%>% select(-Município)
View(Munpedano)


#Não vamos unir as tabelas. O dataset complementar
#fica reservado para estudo posterior e depuração de falsos positivos
#preparando o dataset:
#Munpedano<-rbind(Munpedano,MunSemUf)
#View(Munpedano)

#Listar os objetos neste projeto
ls()
rm(uefe)

#Dataset completo para análises
#Apagar todas as linhas com pelo menos um "NA" ou "sujeira"
Munpedano <- na.omit(Munpedano)
View(Munpedano)

#Vamos inserir o critério de classificação pelo nr habitantes
Munpedano['ClassMunPop']<-Munpedano$Populacao
Munpedano<-Munpedano%>%
  mutate(ClassMunPop=case_when(Populacao<=15000~'Muito Pequeno',
                             Populacao<50000~'Pequeno',
                             Populacao<200000~'Medio',
                             Populacao<500000~'Grande',
                             T~'Muito Grande'
                             ))
write_xlsx(Munpedano,"D:\\CguData\\Munpedano_Dataset_Para_Analise.xlsx")
View(Munpedano)

#Quantos pedidos foram feitos por ano?
PedAno <-Munpedano%>%
  count(Ano)
#Renomear o "n"
PedAno <- PedAno%>% rename(TotalPedidos = n)
library("writexl")
write_xlsx(PedAno,"D:\\CguData\\Plan_Respostas\\PedAno_Ped_feitos_Por_Ano_2012_2018.xlsx")
View(PedMunPeriodo)
View(PedAno)

#quantos pedidos foram feitos no período 2012-2018 por tamanho de municipio?
PedMunPeriodo<-Munpedano%>%
  group_by(ClassMunPop)%>%tally()
PedMunPeriodo <- PedMunPeriodo%>% rename(TotalPedidos = n)
library("writexl")
write_xlsx(PedMunPeriodo,"D:\\CguData\\Plan_Respostas\\PedMunPeriodo_por_tamanho_municipio_2012_2018.xlsx")
View(PedMunPeriodo)

#Em quantos municipios por tipo foram feitos pelo menos UM pedido?
#Cada linha é um pedido
#Contar distintamente os municipios por tipo
PedPorClasAno <- Munpedano %>%
  group_by(Ano,ClassMunPop) %>%
  summarize(Pedidos = n_distinct(Municipio))
View(PedPorClasAno)
#Exportar a resposta pro Excel:
library("writexl")
write_xlsx(PedPorClasAno,"D:\\CguData\\Plan_Respostas\\PedClasAno_PeloMenos1Ped_Por_tamanho_por_ano.xlsx")

#Quantos Pedidos foram feitos por ano, por tamanho de município?
#Cada linha é um pedido
#Contar quantos pedidos por ano, por tamanho de municipio
PedAnoMunClaspop<-Munpedano%>%
  group_by(Ano,ClassMunPop)%>%
  summarise(Municipio=n())
View(PedAnoMunClaspop)
#Exportar a resposta pro Excel:
write_xlsx(PedAnoMunClaspop,"D:\\CguData\\Plan_Respostas\\PedAnoMunClaspop_Ped_por_ano_por_tamanho_mun.xlsx")


#Para saber o percentual dos municípios que tiveram pelo menos UM pedido,
#precisamos saber quantos municípios de cada tipo existiam...
#Vou aplicar a classificação que criamos na Tabela PopulacaoIBGE
PopulacaoIBGE['ClassMunPop']<-PopulacaoIBGE$Populacao
PopulacaoIBGE<-PopulacaoIBGE%>%
  mutate(ClassMunPop=case_when(Populacao<=15000~'Muito Pequeno',
                               Populacao<50000~'Pequeno',
                               Populacao<200000~'Medio',
                               Populacao<500000~'Grande',
                               T~'Muito Grande'
  ))
View(PopulacaoIBGE)
#Quantos municípios de cada tipo existem na tabela do IBGE?
QtMunClas<-PopulacaoIBGE%>%
  group_by(ClassMunPop)%>%
  summarise(qtd_municipios=n())
write_xlsx(QtMunClas,"D:\\CguData\\Plan_Respostas\\QtMunClas_Mun_por_tamanho_Tab_IBGE.xlsx")
View(QtMunClas)

#Criar um dataset para resposta percentual:
#Quantos dos municípios "pequenos" e "muito pequenos"
#fizeram pelo menos UM pedido?
#vamos usar a cláusula "in", como no SQL
PeloMenosUmPedido<-PedPorClasAno%>%
  filter(ClassMunPop%in%c("Muito Pequeno","Pequeno"))
View(PeloMenosUmPedido)

#Calcular o percentual de municípios q tiveram pelo menos um pedido:
#Existem 3250 municipios muito pequenos e 1643 pequenos
#Qual o percentual de municípios, em razão do total
#de municípios por tipo, fizeram pelo menos um pedido?
categoria=PeloMenosUmPedido$ClassMunPop
valor=PeloMenosUmPedido$Pedidos
PeloMenosUmPedido['% MunComPedido']<-case_when(
categoria=="Muito Pequeno"~signif(valor/3250*100,4),
  categoria=="Pequeno"~signif(valor/1643*100,4)
)
View(PeloMenosUmPedido)
write_xlsx(PeloMenosUmPedido,"D:\\CguData\\Plan_Respostas\\PeloMenosUmPedido_Percentual_em_relac_qt_mun_do_tipo_Pelo_menos_1_pedido.xlsx")

#Qual a relação pedidos/habitante, considerando
#os municípios "pequenos" e "muito pequenos"?
#Vamos montar um dataset para essa resposta:
PedPorHab<-Munpedano%>%
  filter(ClassMunPop%in%c("Muito Pequeno","Pequeno"))
PedPorHab= select(PedPorHab,"IDMunicipio","Municipio","UF","Ano","Populacao")
View(PedPorHab)

#Contar quantos pedidos por ano, por IDmunicipio
Pedhab<-PedPorHab%>%
  group_by(Ano,IDMunicipio)%>%
  summarise(Pedidos=n())

#Juntar a quantidade por ID Municipio e Ano como Chave

PedporHabitante <- left_join(PedPorHab,Pedhab,by = c("IDMunicipio" = "IDMunicipio", "Ano"="Ano"), copy = FALSE)
View(PedporHabitante)

#Calcular a quantidade de pedidos por habitante:
qthab=PedporHabitante$Populacao
qtped=PedporHabitante$Pedidos

PedporHabitante['QtPed/Hab']<-round(qtped/qthab,6)
View(PedporHabitante)
#Exportar a resposta pro Excel:
write_xlsx(PedporHabitante,"D:\\CguData\\Plan_Respostas\\PedporHabitante_relacao_pedidos_por_habitante.xlsx")


