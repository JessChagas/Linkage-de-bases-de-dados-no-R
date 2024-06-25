#######################################################
#SCRIPT JESSICA ALMEIDA - 13/06/24
#PARA RL DE BASES DE DADOS SIM E SRAG (SIVEP-GRIPE)
#SCRIPT ELABORADO PARA TESE DE DOUTORADO
##########################################################################################
# DICAS : PARA O LINK DETERMINISTICO COM ESSE SCRIPT 
# SELECIONE AS VARIAVEIS QUE VC QUER EM CADA BANCO , ISTO NO MOMENTO DE 
# CRIAR O BANQUINHO DO LINK (rl_xxxx);
# 
# PARA LINK E NECESSARIO QUE O NOME DAS  VARIAVEIS: NOME,DT NASC,SEXO E NOME DA MAE SEJAM 
# IDENTICOS EM AMBOS OS BANCOS;
# 
# PARA RETIRAR POSSIVEL DUPLICIDADE PODE SER USADO NOME DA MAE, DT NASC, ETC...;
# 
# NO FINAL DE TUDO DA PARA SELECIONAR AS VARIAVEIS QUE QUEREMOS PARA A BASE FINAL LINKADA,
# SERIA BOM RETIRAR AS VARIAVEIS QUE SE REPETEM EM AMBOS OS BANCOS - VC PODE ORGANIZA-LAS
# DO JEITO QUE DESEJAR;
###########################################################################################

#1 PASSO - MUDAR O DIRETORIO 


#2 PASSO - CHAMAR PACOTES NA BIBLIOTECA
###CASO NAO TENHA ESSES PACOTES INSTALADOS SO INSTALAR 

#install.packages(NOME DO PACOTE)


library(epiDisplay)
library(plyr)
library(dplyr)
library(lubridate)
library(openxlsx) # LER BASES EM xlsx
library(stringi) #PASSA PARA MAIUSCULO OU minusculo
library(gdata) 
library(writexl)
library(SoundexBR)#REALIZA O SOUNDEX DE NOMES BRASILEIROS
library(stringr) #RETIRA OS ESPACOS


#CHAMANDO O Rdata E DEIXANDO A BASE QUE IREI TRABALHAR

# BASES:
# SIM TOTAL E SRAG TOTAL


# 3.2 PASSO - SELECIONANDO AS VARIAVEIS QUE IREI UTILIZAR PARA O RL 
# CRIANDO UM NOVO OBJETO PARA COMPORTAR ESSAS VARIAVEIS

names(SIM_TOTAL) 


rl_SIM <-SIM_TOTAL[, c("numerodo","nome","nomemae","dtnasc","sexo","dtobito",
                       "racacor","nomepai","estciv","esc","ocup","baires",
                       "endres","lococor","codmunocor","baiocor","endocor",
                       "causabas","idade2","STATUS")] 


# MODIFICANDO O NOME DAS VAIRAVEIS
#PS: TEM QUE SER O MESMO NOME NAS DUAS BASES PARA FAZER O RL

names(rl_SIM) <- c("n_do","nome","nomemae","dtnasc","sexo","dtobito",
                    "racacor","nomepai","estciv","esc","ocup","baires",
                    "endres","lococor","codmunocor","baiocor","endocor",
                    "causabas","idade2","STATUS")



#### 
#  QUEBRANDO A VARIAVEL DATA 
##########

# OLHANDO AS 10 PRIMEIRAS LINHAS

rl_SIM$dtnasc[1:10] 

#DECOMPONDO EM DIA,MES E ANO

rl_SIM$dianasc <- day(ymd(rl_SIM$dtnasc))
rl_SIM$mesnasc <- month(ymd(rl_SIM$dtnasc))
rl_SIM$anonasc <- year(ymd(rl_SIM$dtnasc))


#### 
#  QUEBRANDO A VARIAVEL NOME 
##########


# DESMEMBRANDO NOME = PRINOME E ULTIMONOME

names(rl_SIM)

rl_SIM$prinome <- sapply(strsplit(rl_SIM$nome, " "), function(x) x[1]) 
rl_SIM$ultinome <- sapply(strsplit(rl_SIM$nome, " "), function(x) x[length(x)])



#############
# SOUNDEX
#############

# SOUDEX COM PRIMEIRO NOME DO PACIENTE


#NOME PRIMEIRO 
#SOUNDEX1

rl_SIM$prinome_sdx <- soundexBR(rl_SIM$prinome) 
rl_SIM$prinome_sdx <- substr(soundexBR(rl_SIM$prinome), 1, 1) 
rl_SIM$SOUNDEX1 <- NA
rl_SIM$SOUNDEX1 <- soundexBR(rl_SIM$prinome)
tab1(rl_SIM$SOUNDEX1, graph=F)


# SOUNDEX DO ULTIMO NOME DO PACIENTE

#ULTIMO NOME DO PACIENTE
#SOUNDEX2

rl_SIM$ultinome_sdx <- soundexBR(rl_SIM$ultinome)
rl_SIM$ultinome_sdx <- substr(soundexBR(rl_SIM$ultinome), 1, 1) 
rl_SIM$SOUNDEX2 <- NA
rl_SIM$SOUNDEX2 <- soundexBR(rl_SIM$ultinome)
tab1(rl_SIM$SONUDEX2, graph=F)


table(rl_SIM$sexo)


#####################
#### BASE SRAG TOTAL
######################

# 3.2 PASSO - SELECIONANDO AS VARIAVEIS QUE IREI UTILIZAR PARA O RL 
# CRIANDO UM NOVO OBJETO PARA COMPORTAR ESSAS VARIAVEIS

names(SRAG_TOTAL) 


rl_SRAGTOTAL<-SRAG_TOTAL[, c("nu_do","nm_pacient","nm_mae_pac","dt_nasc","cs_sexo","nu_idade_n",
                                "dt_notific","dt_sin_pri","id_unidade","cs_gestant","cs_raca","cs_escol_n",
                                "nm_bairro","febre","tosse","garganta","dispneia","desc_resp",
                                "saturacao","diarreia","vomito","fator_risc","puerpera","cardiopati",
                                "hematologi","sind_down","hepatica","asma","diabetes","neurologic","pneumopati",
                                "imunodepre","renal","obesidade","vacina","vacina_cov", "hospital","dt_interna",
                                "co_mu_inte","nm_un_inte","uti","dt_entuti","dt_saiduti","suport_ven","classi_fin",
                                "criterio","evolucao","dt_evoluca","dt_encerra")] 


# MODIFICANDO O NOME DAS VAIRAVEIS
#PS: TEM QUE SER O MESMO NOME NAS DUAS BASES PARA FAZER O RL

names(rl_SRAGTOTAL) <- c("n_do","nome","nomemae","dtnasc","sexo","idade2",
                        "dt_notific","dt_sin_pri","id_unidade","cs_gestant","cs_raca","cs_escol_n",
                        "nm_bairro","febre","tosse","garganta","dispneia","desc_resp",
                        "saturacao","diarreia","vomito","fator_risc","puerpera","cardiopati",
                        "hematologi","sind_down","hepatica","asma","diabetes","neurologic","pneumopati",
                        "imunodepre","renal","obesidade","vacina","vacina_cov", "hospital","dt_interna",
                        "co_mu_inte","nm_un_inte","uti","dt_entuti","dt_saiduti","suport_ven","classi_fin",
                        "criterio","evolucao","dt_evoluca","dt_encerra")





#DECOMPONDO EM DIA,MES E ANO

rl_SRAGTOTAL$dianasc <- day(ymd(rl_SRAGTOTAL$dtnasc))
rl_SRAGTOTAL$mesnasc <- month(ymd(rl_SRAGTOTAL$dtnasc))
rl_SRAGTOTAL$anonasc <- year(ymd(rl_SRAGTOTAL$dtnasc))


# DESMEMBRANDO NOME = PRINOME E ULTIMONOME

names(rl_SRAGTOTAL)

rl_SRAGTOTAL$prinome <- sapply(strsplit(rl_SRAGTOTAL$nome, " "), function(x) x[1]) 
rl_SRAGTOTAL$ultinome <- sapply(strsplit(rl_SRAGTOTAL$nome, " "), function(x) x[length(x)])


# 4.2 PASSO - SOUDEX COM PRIMEIRO NOME DO PACIENTE

# SOUNDEX

#NOME
#SOUNDEX 1

rl_SRAGTOTAL$prinome_sdx <- soundexBR(rl_SRAGTOTAL$prinome) 
rl_SRAGTOTAL$prinome_sdx <- substr(soundexBR(rl_SRAGTOTAL$prinome), 1, 1) 
rl_SRAGTOTAL$SOUNDEX1 <- NA
rl_SRAGTOTAL$SOUNDEX1 <- soundexBR(rl_SRAGTOTAL$prinome)
tab1(rl_SRAGTOTAL$SOUNDEX1, graph=F)


# 4.3 PASSO - SOUDEX DO ULTIMO NOME DO PACIENTE

#SOUNDEX2

#ULTIMO NOME DO PACIENTE

rl_SRAGTOTAL$ultinome_sdx <- soundexBR(rl_SRAGTOTAL$ultinome)
rl_SRAGTOTAL$ultinome_sdx <- substr(soundexBR(rl_SRAGTOTAL$ultinome), 1, 1) 
rl_SRAGTOTAL$SOUNDEX2 <- NA
rl_SRAGTOTAL$SOUNDEX2 <- soundexBR(rl_SRAGTOTAL$ultinome)
tab1(rl_SRAGTOTAL$SOUNDEX2, graph=F)


table(rl_SRAGTOTAL$sexo)

############################
#VERIFICANDO A CLASSIFICACAO DE ALGUMAS VARIAVEIS
#############################

class(rl_SIM$n_do)

class(rl_SRAGTOTAL$n_do)


# TRANSFORMANDO

rl_SRAGTOTAL$n_do <- as.factor(rl_SRAGTOTAL$n_do)

rl_SIM$n_do <- as.factor(rl_SIM$n_do)


#VERIFICANDO A CLASSIFICACAO

class(rl_SIM$n_do)

class(rl_SRAGTOTAL$n_do)

##############################
# DETERMINISTICO
##############################

names(rl_SIM)

names(rl_SRAGTOTAL)


# REALIZANDO O LINK DETERMINISTICO

DETERMINI_TOTAL<-inner_join(rl_SIM,rl_SRAGTOTAL, by = c("SOUNDEX1","SOUNDEX2","nome","dtnasc","nomemae","sexo"))


dim(DETERMINI_TOTAL) 
summary(DETERMINI_TOTAL)
names(DETERMINI_TOTAL)


#  vERIFICAR DUPLICADOS E DUPLA ENTRADA  


duplic <- DETERMINI_TOTAL |>
  janitor::get_dupes() 


# EXCLUINDO OS DUPLICADOS PELAS LINHAS OU ID

DETERMINI_TOTAL <- DETERMINI_TOTAL[-c(24133,11731,20248,21834,18701,22866,19501,22310,23693,
                                      21534,25801,28032,27276,28073,30444),]



############ // ############ // ########################

### VERIFICANDO A QUANTIDADE DE DUPLA ENTRADA 
#PELO NOME E DATA DE NASCIMENTO

names(DETERMINI_TOTAL)

dados.nome_DT <- data.frame(NOME = DETERMINI_TOTAL$nome, DTNASC = DETERMINI_TOTAL$dtnasc)
dim(dados.nome_DT)

dados.novo <- NULL
dados.novo <- dados.nome_DT[!duplicated(dados.nome_DT),] #usando nome e dt nasc
nrow(dados.novo) 



# vendo os duplicados
dados.duplicados <- NULL
dados.duplicados <- dados.nome_DT[duplicated(dados.nome_DT),]

#ordem alfabetica
dados.duplicados <- dados.duplicados[order(dados.duplicados$NOME),]
nrow(dados.duplicados)                     
View(dados.duplicados) 


# FORAM VERIFICADAS XXX POSSIVEIS DUPLAS ENTRADAS NO SIVEP
# OPTAMOS POR MANTER NA BASE PARA VERIFICAR REINTERNACAO ETC.

#EXCLUINDO ALGUNS BANCOS
rm (dados.duplicados,dados.nome_DT,dados.novo)


############## // ############### // #########################

# RETIRANDO DO BANCO SRAG TOTAL OS CASOS LOCALIZADO NO LINK 
# OU SEJA , CRIANDO UMA BASE NOVA BASE PARA O RL PROBABILISTICO

SRAGTOTAL_novo<-anti_join(rl_SRAGTOTAL,DETERMINI_TOTAL, by = c("SOUNDEX1","SOUNDEX2","nome","dtnasc","nomemae","sexo"))

dim(SRAGTOTAL_novo)
summary(SRAGTOTAL_novo)
names(SRAGTOTAL_novo)



# ESCOLHENDO VARIAVEIS
SRAGTOTAL_novo <-SRAGTOTAL_novo[, c( "n_do","nome","nomemae","dtnasc","sexo","idade2","dt_notific","dt_sin_pri",
                                   "id_unidade","cs_gestant","cs_raca","cs_escol_n","nm_bairro","febre","tosse",
                                   "garganta","dispneia","desc_resp","saturacao","diarreia","vomito","fator_risc",
                                   "puerpera","cardiopati","hematologi","sind_down","hepatica","asma","diabetes",
                                   "neurologic","pneumopati","imunodepre","renal","obesidade","vacina","vacina_cov",  
                                   "hospital","dt_interna","co_mu_inte","nm_un_inte","uti","dt_entuti","dt_saiduti",
                                   "suport_ven","classi_fin","criterio","evolucao","dt_evoluca","dt_encerra" )]

## EXCLUINDO ALGUNS BANCOS

rm(rl_SIM,rl_SRAGTOTAL,SIM_TOTAL,SRAG_TOTAL)


### SALVANDO Rdata DE SRAG TOTAL novo 

rm(SRAGTOTAL_novo)


####################
#SELECIONANDO AS VARIAVEIS FINAIS
# ARRUMANDO A BASE LINKCADA PELO DETERMINISTICO
###############################################

names(DETERMINI_TOTAL)


DETERMINI_TOTAL_select <- DETERMINI_TOTAL|> dplyr::select("nome","nomemae","nomepai","dtnasc", "idade2.x","sexo","racacor",
  "estciv","esc","ocup","baires","endres", "cs_gestant","dt_sin_pri","febre","tosse",
  "garganta","dispneia","desc_resp","saturacao","diarreia","vomito","fator_risc","puerpera",
  "cardiopati","hematologi","sind_down","hepatica","asma","diabetes","neurologic","pneumopati",
  "imunodepre","renal","obesidade","vacina","vacina_cov","hospital","dt_notific","dt_interna","co_mu_inte",    
  "nm_un_inte","uti","dt_entuti","dt_saiduti","suport_ven","classi_fin","criterio","evolucao",
  "dt_evoluca","dt_encerra","dtobito","lococor","codmunocor","baiocor","endocor","causabas","STATUS")


# ESSE SELECT VAI FICAR SEPAREDO PARA QUANDO ACHARMOS PARES VERDADEIROS 
# NO LINK PROBABILISTICO PARA AO FINAL DE TUDO JUNTAR TODOS EM UM UNICO BANCO
# LEMBRENDO QUE A ORDEM DAS VARIAVEIS DEVEM SER AS MESMAS (RL DETERM. E PROBI.) 
# NO MOMENTO DAS SELECAO.


# SALVANDO RDATA - LINK DETERMINISTICO