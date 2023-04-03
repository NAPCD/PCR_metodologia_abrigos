#----------------LENDO BANDO DE DADOS NECESSARIO-----------------------------

install.packages("dplyr")
install.packages("here")
install.packages("ggplot2")
install.packages("readxl")
install.packages("gdata")

library(dplyr)
library(here)
library(ggplot2)
library(readxl)
library(rio)
library(tidyr)
library(gdata)


#----------definindo de acordo com dados do censo DA EDUCAÇÃO BÁSICA----------

censo_educacao_basica_versao_original <- import(here("data","microdados_ed_basica_2022.csv"))

colnames(censo_educacao_basica)

head(censo_educacao_basica)

censo_educacao_basica_recife <- censo_educacao_basica_versao_original%>%
  select(
  NO_MUNICIPIO, CO_MUNICIPIO, CO_DISTRITO, CO_ENTIDADE, NO_ENTIDADE,
  NU_ENDERECO,IN_VINCULO_SECRETARIA_EDUCACAO,
  TP_CATEGORIA_ESCOLA_PRIVADA, NO_BAIRRO, NU_TELEFONE, TP_SITUACAO_FUNCIONAMENTO,
  IN_LOCAL_FUNC_GALPAO, TP_OCUPACAO_GALPAO,            
  IN_LOCAL_FUNC_SALAS_OUTRA_ESC, IN_LOCAL_FUNC_OUTROS,          
  IN_PREDIO_COMPARTILHADO, IN_AGUA_FILTRADA,              
  IN_AGUA_POTAVEL, IN_AGUA_REDE_PUBLICA,          
  IN_AGUA_POCO_ARTESIANO,IN_AGUA_CACIMBA,
  IN_AGUA_INEXISTENTE,           
  IN_ENERGIA_REDE_PUBLICA, IN_ENERGIA_GERADOR,            
  IN_ENERGIA_GERADOR_FOSSIL, IN_ENERGIA_OUTROS,             
  IN_ENERGIA_RENOVAVEL, IN_ENERGIA_INEXISTENTE,        
  IN_ESGOTO_REDE_PUBLICA, IN_ESGOTO_FOSSA_SEPTICA,       
  IN_ESGOTO_FOSSA_COMUM, IN_ESGOTO_FOSSA,             
  IN_ESGOTO_INEXISTENTE, IN_LIXO_SERVICO_COLETA,         
  IN_LIXO_QUEIMA, IN_LIXO_ENTERRA,              
  IN_LIXO_DESTINO_FINAL_PUBLICO, IN_LIXO_DESCARTA_OUTRA_AREA,    
  IN_LIXO_JOGA_OUTRA_AREA, IN_LIXO_OUTROS,                 
  IN_LIXO_RECICLA, IN_TRATAMENTO_LIXO_SEPARACAO,  
  IN_TRATAMENTO_LIXO_REUTILIZA, IN_TRATAMENTO_LIXO_RECICLAGEM,  
  IN_TRATAMENTO_LIXO_INEXISTENTE, IN_ALMOXARIFADO,               
  IN_AREA_VERDE, IN_AUDITORIO,                  
  IN_BANHEIRO_FORA_PREDIO, IN_BANHEIRO_DENTRO_PREDIO,     
  IN_BANHEIRO, IN_BANHEIRO_EI,                
  IN_BANHEIRO_PNE,     
  IN_BANHEIRO_CHUVEIRO,   
  IN_COZINHA, IN_DESPENSA,                    
  IN_PATIO_COBERTO,              
  IN_PATIO_DESCOBERTO,
  IN_PARQUE_INFANTIL,           
  IN_PISCINA, IN_QUADRA_ESPORTES,             
  IN_QUADRA_ESPORTES_COBERTA, IN_QUADRA_ESPORTES_DESCOBERTA, 
  IN_REFEITORIO, IN_SALA_ATELIE_ARTES,          
  IN_LAVANDERIA,IN_ACESSIBILIDADE_CORRIMAO,    
  IN_ACESSIBILIDADE_SINAL_SONORO, IN_ACESSIBILIDADE_INEXISTENTE,
  QT_SALAS_UTILIZADAS,QT_SALAS_UTILIZA_CLIMATIZADAS, QT_SALAS_UTILIZADAS_ACESSIVEIS,
  IN_INTERNET)%>%
  filter(CO_MUNICIPIO == "2611606")

#----------------Visualizando estrura das escolas do Recife------------------------------

VARS_PRIMEIRA_SELECAO <- censo_educacao_basica_recife%>%
  select(NO_MUNICIPIO, CO_MUNICIPIO, CO_DISTRITO, CO_ENTIDADE, NO_ENTIDADE,
         NU_ENDERECO,IN_VINCULO_SECRETARIA_EDUCACAO,TP_SITUACAO_FUNCIONAMENTO,
         TP_CATEGORIA_ESCOLA_PRIVADA, NO_BAIRRO, NU_TELEFONE, 
         IN_LOCAL_FUNC_GALPAO, TP_OCUPACAO_GALPAO, IN_AGUA_FILTRADA,
         IN_ENERGIA_REDE_PUBLICA, IN_ENERGIA_GERADOR, IN_ESGOTO_REDE_PUBLICA,
         IN_ESGOTO_INEXISTENTE, IN_ALMOXARIFADO,
         IN_COZINHA, IN_PATIO_COBERTO,              
         IN_PATIO_DESCOBERTO, IN_REFEITORIO, IN_LAVANDERIA,
         IN_ACESSIBILIDADE_INEXISTENTE,QT_SALAS_UTILIZADAS,
         IN_INTERNET, IN_QUADRA_ESPORTES,             
         IN_QUADRA_ESPORTES_COBERTA)

VARS_SELECAO_QUANT <- censo_educacao_basica_recife%>%
  select(CO_ENTIDADE,NO_ENTIDADE,QT_SALAS_UTILIZADAS)%>%
  filter(CO_ENTIDADE == c(26124904))

VARS_SELECAO_QUANT_TESTE <- censo_educacao_basica_recife%>%
  select(CO_ENTIDADE,NO_ENTIDADE,QT_SALAS_UTILIZADAS)%>%
  filter(CO_ENTIDADE == c(26177820))

  
VARS_CONTX_ESCOLAS <- Contextualizacao_Unidades_de_Ensino%>%
  select(INEP, TIPO, `UNIDADE DE ENSINO`, BAIRRO, RPA, Regiao, Regional,
         LOGRADOURO, COMPLEMENTO, Longitude.OK, Latitude.OK)%>%
  rename(CO_ENTIDADE = INEP)

teste <- BANCO_ESCOLAS_MUNIC_RECIFE_ABRIGOS_USAR_ESSE%>%
  select(RPA, IN_ENERGIA_REDE_PUBLICA, IN_ESGOTO_REDE_PUBLICA, 
         IN_COZINHA, IN_PATIO_COBERTO, IN_PATIO_DESCOBERTO, IN_REFEITORIO,
         IN_ACESSIBILIDADE_INEXISTENTE, QT_SALAS_UTILIZADAS, IN_QUADRA_ESPORTES,             
         IN_QUADRA_ESPORTES_COBERTA)%>%
  group_by(RPA)%>%
  summarise(ENERGIA = sum(IN_ENERGIA_REDE_PUBLICA),
            ESGOTO = sum(IN_ESGOTO_REDE_PUBLICA),
            COZINHA = sum(IN_COZINHA),
            PATIO_COBERTO = sum(IN_PATIO_COBERTO),
            PATIO_DESCOBERTO = sum(IN_PATIO_DESCOBERTO),
            QUADRA_COBERTA = sum(IN_QUADRA_ESPORTES),
            REFEITORIO = sum(IN_REFEITORIO))%>%
  pivot_longer(!c(RPA), names_to = "TIPO",
               values_to = "count")%>%
  rename(N_TIPO = count)


COM_NOME_ESCOLAS <- censo_educacao_basica_recife%>%
  select(RPA, NO_BAIRRO, CO_ENTIDADE, NO_ENTIDADE, IN_ENERGIA_REDE_PUBLICA, 
         IN_ESGOTO_REDE_PUBLICA, IN_COZINHA, IN_PATIO_COBERTO, 
         IN_PATIO_DESCOBERTO, IN_REFEITORIO,
         IN_ACESSIBILIDADE_INEXISTENTE, QT_SALAS_UTILIZADAS, IN_QUADRA_ESPORTES,             
         IN_QUADRA_ESPORTES_COBERTA)
 


ggplot(teste, aes(x= TIPO, y = N_TIPO))+
  geom_bar(position="identity",stat="identity", fill = "#0B5BA4", alpha=.6, width=.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ RPA)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0), limits = c(-10,100))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_TIPO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Infraestrutura Escolas por RPA",
       y = NULL,
       caption ="
          \n Fonte: Censo Educação Básica 2022
          \n NAPCD - GGPLAN")


#verificando isso por bairro

INFRA_ESCOLAS_RPA <-BANCO_ESCOLAS%>%
  select(RPA, NO_BAIRRO, IN_ENERGIA_REDE_PUBLICA, IN_ESGOTO_REDE_PUBLICA, 
         IN_COZINHA, IN_PATIO_COBERTO, IN_PATIO_DESCOBERTO, IN_REFEITORIO,
         IN_ACESSIBILIDADE_INEXISTENTE, QT_SALAS_UTILIZADAS, IN_QUADRA_ESPORTES,             
         IN_QUADRA_ESPORTES_COBERTA)%>%
  group_by(RPA, NO_BAIRRO)%>%
  summarise(ENERGIA = sum(IN_ENERGIA_REDE_PUBLICA),
            ESGOTO = sum(IN_ESGOTO_REDE_PUBLICA),
            COZINHA = sum(IN_COZINHA),
            PATIO_COBERTO = sum(IN_PATIO_COBERTO),
            PATIO_DESCOBERTO = sum(IN_PATIO_DESCOBERTO),
            QUADRA_COBERTA = sum(IN_QUADRA_ESPORTES),
            REFEITORIO = sum(IN_REFEITORIO))%>%
  pivot_longer(!c(RPA, NO_BAIRRO), names_to = "TIPO",
               values_to = "count")%>%
  rename(N_TIPO = count)



TESTE <- BANCO_ESCOLAS%>%
  select(RPA, NO_BAIRRO, IN_ENERGIA_REDE_PUBLICA, IN_ESGOTO_REDE_PUBLICA, 
         IN_COZINHA, IN_PATIO_COBERTO, IN_PATIO_DESCOBERTO, IN_REFEITORIO,
         IN_ACESSIBILIDADE_INEXISTENTE, QT_SALAS_UTILIZADAS, IN_QUADRA_ESPORTES,             
         IN_QUADRA_ESPORTES_COBERTA)%>%
  group_by(RPA, NO_BAIRRO)%>%
  summarise(QUADRA_ESPORTES_COBERTA = sum(IN_QUADRA_ESPORTES_COBERTA))


INFRA_ESCOLAS_RPA%>%
filter(RPA == 6)%>%
ggplot(aes(x= NO_BAIRRO, y = N_TIPO))+
  geom_bar(position="identity",stat="identity", fill = "#0B5BA4", alpha=.6, width=.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ TIPO)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0), limits =c(-3, 25))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_TIPO), 
    size = 2.5,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Infraestrutura Escolas por RPA",
       subtitle = "RPA-6",
       y = NULL,
       caption ="
          \n Fonte: Censo Educação Básica 2022
          \n NAPCD - GGPLAN")



#--------------DEFININDO PRIORITARIAMENTE ESCOLAS QUE SERAO USADAS----------------

EQUIPAM_ESCOLAS_RECIFE <- Contextualizacao_Unidades_de_Ensino%>%
  select(INEP, TIPO, `UNIDADE DE ENSINO`, BAIRRO, RPA, Regiao, Regional,
         LOGRADOURO, COMPLEMENTO, Longitude.OK, Latitude.OK)%>%
  rename(CO_ENTIDADE_INEP = INEP,
         UNIDADE_ENSINO = `UNIDADE DE ENSINO`)

VARS_SEGUNDA_SELECAO <- VARS_PRIMEIRA_SELECAO%>%
  rename(CO_ENTIDADE_INEP = CO_ENTIDADE)%>%
  filter(TP_SITUACAO_FUNCIONAMENTO == 1,
         IN_VINCULO_SECRETARIA_EDUCACAO == 1)
#SELECIONANDO SOMENTE ESCOLAS QUE ESTAO FUNCIONANDO

VARS_PRIMEIRA_SELECAO%>%
  select(TP_CATEGORIA_ESCOLA_PRIVADA)%>%
  na.omit(TP_CATEGORIA_ESCOLA_PRIVADA)%>%
  count(TP_CATEGORIA_ESCOLA_PRIVADA)

BANCO_ESCOLAS_MUNIC_RECIFE <- VARS_SEGUNDA_SELECAO%>%
    left_join(EQUIPAM_ESCOLAS_RECIFE,
   by = join_by(CO_ENTIDADE_INEP))

BANCO_ESCOLAS_MUNIC_RECIFE$QTD_PESSOAS_ESTIMADAS_SALA <- (BANCO_ESCOLAS_MUNIC_RECIFE$QT_SALAS_UTILIZADAS*6)


#Qual capacidade de escolas que oferecem infraestrutura adequada por RPA

write.csv2(BANCO_ESCOLAS_MUNIC_RECIFE, "BANCO_ESCOLAS_MUNIC_RECIFE.csv")

colnames(BANCO_ESCOLAS_MUNIC_RECIFE)

#Energia da rede pública;
#Esgoto rede pública;
#Pátio com estrutura coberta;
#Quadra de esportes coberta;
#Cozinha;
#Refeitório; 
#Banheiros com chuveiros; 
#Quantidade de salas disponíveis para uso;
#Acessibilidade para pessoas com deficiência.


teste <- BANCO_ESCOLAS_MUNIC_RECIFE%>%
  select(NO_BAIRRO, RPA, IN_ENERGIA_REDE_PUBLICA, IN_ESGOTO_REDE_PUBLICA,
         IN_PATIO_COBERTO, IN_QUADRA_ESPORTES_COBERTA, IN_COZINHA,
         IN_REFEITORIO, QT_SALAS_UTILIZADAS, QTD_PESSOAS_ESTIMADAS_SALA)%>%
  group_by(RPA, NO_BAIRRO)%>%
  filter(IN_ENERGIA_REDE_PUBLICA == 1,
         IN_ESGOTO_REDE_PUBLICA == 1,
         IN_PATIO_COBERTO == 1,
         IN_QUADRA_ESPORTES_COBERTA == 1,
         IN_COZINHA == 1,
         IN_REFEITORIO == 1)%>%
  summarise(QTD_ESCOLAS_APROPRIADAS = sum(n()))
  

write.csv2(teste, "ESCOLAS_APROPRIADAS_RECIFE.csv")


ESCOLAS_APROPRIADAS_RECIFE$...1 <- NULL


ESCOLAS_APROPRIADAS_RECIFE <- ESCOLAS_APROPRIADAS_RECIFE%>%
  group_by(RPA, NO_BAIRRO)%>%
  summarise(QTD_ESCOLAS_APROPRIADAS = sum(QTD_ESCOLAS_APROPRIADAS))

write.csv2(BANCO_ESCOLAS_MUNIC_RECIFE, "BANCO_ESCOLAS_MUNIC_RECIFE.csv")


#-------------CALCULANDO pessoas em situação de risco R3 e R4--------------

#inicialmente preciso definir o bairro onde o risco está inserido


PONTOS_DE_RISCO_API_SEDEC_BAIRROS <- PONTOS_DE_RISCO_API_SEDEC%>%
  mutate(BAIRRO = str_extract(adresss,",.*,"))%>%
  mutate(BAIRRO = str_replace(BAIRRO, ",", ""))%>%
  mutate(BAIRRO = str_replace(BAIRRO, ",", ""))%>%
  mutate(BAIRRO = trim(BAIRRO))%>%
  mutate_if(is.character, toupper)
  
  
PONTOS_DE_RISCO_API_SEDEC_BAIRROS$BAIRRO <- abjutils::rm_accent(PONTOS_DE_RISCO_API_SEDEC_BAIRROS$BAIRRO)


#AGORA SIM irei plotar o grafico
TESTANDO%>%
  select(risk, N_PESSOAS_RISCO)%>%
  filter(risk == "R3")%>%
  group_by(risk)%>%
  summarise(sum(N_PESSOAS_RISCO))


TESTANDO <- PONTOS_DE_RISCO_API_SEDEC_BAIRROS%>%
  select(BAIRRO, risk, families_count, peoples_count)%>%
  group_by(BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-10,260))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 2.5,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco por Bairro",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")


#VISULIZANDO POR RPA

RPA <- Contextualizacao_BairrosTerritorio%>%
  select(BAIRRO, RPA)

PONTOS_DE_RISCO_API_SEDEC_BAIRROS_RPA <- 
  merge(x=PONTOS_DE_RISCO_API_SEDEC_BAIRROS,y= RPA, by="BAIRRO",all=TRUE)

write.csv2(PONTOS_DE_RISCO_API_SEDEC_BAIRROS_RPA, "PONTOS_DE_RISCO_API_SEDEC_BAIRROS_RPA.csv")


#AGORA POSSO VISUALIZAR POR RPA TUDO 

PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  facet_wrap(~ RPA)+
  scale_x_continuous(expand = c(0,0), limits = c(-10,280))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 2.5,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco por Bairro e RPA",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")

#SEPARADO

PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0,
         RPA == 1)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-5,100))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco",
       subtitle =  "RPA 1",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")

PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0,
         RPA == 2)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-5,300))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco",
       subtitle =  "RPA 2",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")

PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0,
         RPA == 3)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-5,300))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco",
       subtitle =  "RPA 3",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")


PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0,
         RPA == 4)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-5,100))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco",
       subtitle =  "RPA 4",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")

PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0,
         RPA == 5)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-5,100))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco",
       subtitle =  "RPA 5",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")

PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0,
         RPA == 6)%>%
  ggplot(aes(x= N_PESSOAS_RISCO, y = BAIRRO))+
  geom_bar(position="identity", stat="identity", fill="#0B5BA4", alpha=0.8, width=0.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ risk)+
  scale_x_continuous(expand = c(0,0), limits = c(-5,300))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_PESSOAS_RISCO), 
    size = 4.0,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Número de pessoas em situação de Risco",
       subtitle =  "RPA 6",
       x = NULL,
       caption ="
          \n Fonte: API SEDEC
          \n Tratamento: NAPCD")

#----numero de escolas com infraestrutura apropriada para receber pessoas definidas no 

VISAO_GERAL_ESCOLAS <- BANCO_ESCOLAS_MUNIC_RECIFE_ABRIGOS_APLICACAO%>%
  select(NO_BAIRRO, RPA, IN_ENERGIA_REDE_PUBLICA, IN_ESGOTO_REDE_PUBLICA,
         IN_PATIO_COBERTO, IN_QUADRA_ESPORTES_COBERTA, IN_COZINHA,
         IN_REFEITORIO, QT_SALAS_UTILIZADAS, QTD_PESSOAS_ESTIMADAS)%>%
  group_by(RPA, NO_BAIRRO)


VISAO_GERAL_ESCOLAS%>%
  filter(RPA == 5)%>%
  ggplot(aes(x= NO_BAIRRO, y = N_TIPO))+
  geom_bar(position="identity",stat="identity", fill = "#0B5BA4", alpha=.6, width=.8)+
  ggthemes::theme_fivethirtyeight()+
  facet_grid(~ TIPO)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0), limits =c(-3, 25))+
  theme(
    axis.title.x = element_text(size = 15, face="bold"))+
  geom_text(
    aes(label = N_TIPO), 
    size = 2.5,
    position = position_dodge(width=-0.9), hjust=-0.1)+
  labs(title = "Infraestrutura Escolas por RPA",
       subtitle = "RPA-5",
       y = NULL,
       caption ="
          \n Fonte: Censo Educação Básica 2022
          \n NAPCD - GGPLAN")


VISAO_GERAL_ESCOLAS <- BANCO_ESCOLAS_MUNIC_RECIFE_ABRIGOS_USAR_ESSE%>%
  select(NO_BAIRRO, RPA, IN_ENERGIA_REDE_PUBLICA, IN_ESGOTO_REDE_PUBLICA,
         IN_PATIO_COBERTO, IN_QUADRA_ESPORTES_COBERTA, IN_COZINHA,
         IN_REFEITORIO, QT_SALAS_UTILIZADAS, QTD_PESSOAS_ESTIMADAS)%>%
  group_by(RPA, NO_BAIRRO)

