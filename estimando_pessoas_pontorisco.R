# Os dados da API SEDEC nos indicam que

PONTOS_DE_RISCO_API_SEDEC_BAIRROS <- PONTOS_DE_RISCO_API_SEDEC%>%
  mutate(BAIRRO = str_extract(adresss,",.*,"))%>%
  mutate(BAIRRO = str_replace(BAIRRO, ",", ""))%>%
  mutate(BAIRRO = str_replace(BAIRRO, ",", ""))%>%
  mutate(BAIRRO = trim(BAIRRO))%>%
  mutate_if(is.character, toupper)

#criando somente a variavel Bairros
PONTOS_DE_RISCO_API_SEDEC_BAIRROS$BAIRRO <- abjutils::rm_accent(PONTOS_DE_RISCO_API_SEDEC_BAIRROS$BAIRRO)

#Verificando o quantitativo atual do preenchimento da SEDEC

PONTOS_RISCO_CONTABILIZADOS_SEDEC <-PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, risk, families_count, peoples_count)%>%
  group_by(RPA, BAIRRO, risk)%>%
  summarise(N_PESSOAS_RISCO = sum(peoples_count),
            N_FAMILIAS_RISCO = sum(families_count))%>%
  na.omit()%>%
  filter(N_PESSOAS_RISCO != 0)

#com pessoas
PONTOS_RISCO_CONTABILIZADOS_SEDEC_COM_PESSOAS <- PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA, `OBJECTID *`, risk, families_count, peoples_count)%>%
  filter(families_count != 0 & peoples_count != 0)%>%
  rename(OBJECT_ID = `OBJECTID *`)

#sem pessoas
PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS <- PONTOS_RISCO_BAIRRO_RPA%>%
  select(BAIRRO, RPA,`OBJECTID *`, risk, families_count, peoples_count)%>%
  filter(families_count == 0 & peoples_count == 0)%>%
  rename(OBJECT_ID = `OBJECTID *`)

#visualizando a distribuicao por tipo de risco

PONTOS_RISCO_CONTABILIZADOS_SEDEC_COM_PESSOAS%>%
  select(risk,peoples_count)%>%
  filter(risk == "R3")%>%
  ggplot(aes(x=peoples_count)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

PONTOS_RISCO_CONTABILIZADOS_SEDEC_COM_PESSOAS%>%
  select(risk,peoples_count)%>%
  filter(risk == "R4")%>%
  ggplot(aes(x=peoples_count)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")


#estimando uma distribuicao poisson de lambda 1

lamb <- 1
u <- runif(1)
flag <- 0
i <- 0; F <- p <- exp(-lamb)
          repeat{
            if(u<F){
              x <- i; flag <- 1
            } else {
              p <- (lamb/(i+1))*p; F <- F+p
              i <- i+1
            }
            if(flag==1){break}
          }
x
## [1] 0
u
## [1] 0.119341


#assim iremos simular n valores uma distribuicao Poisson de lambda 1

fsimul.pois <- function(u,lamb){
  flag <- 0
  i <- 0; F <- p <- exp(-lamb)
  repeat{
    if(u<F){
      out <- i; flag <- 1
    } else {
      p <- (lamb/(i+1))*p; F <- F+p
      i <- i+1
    }
    if(flag==1){break}
  }
  return(out)
}


n <- 6755; lamb <- 1

amostra <- rep(NA,n)

for(k in 1:n){u <- runif(1); amostra[k] <- fsimul.pois(u,lamb)}
table(amostra) # freq. absoluta de cada possível valor de X na amostra
## amostra
## 0    1    2    3    4    5    6    7 
## 2386 2612 1217  400  115   21    3    1 

## amostra modificada pois meus dados iniciam sempre no 1
##   1    2    3    4    5     6    7    8
## 2386 2612 1217  400  115   21    3    1

freq <- (as.vector(table(amostra)))/n
freq # freq. relativa de cada possível valor de X na amostra
## [1] 0.3532198372 0.3866765359 0.1801628423 0.0592153960 0.0170244264 0.0031088083 0.0004441155
## [8] 0.0001480385


#agora preciso pegar o meu valor amostral e inserir no meu banco de pontos de risco sem pessoas

PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS$peoples_count <- amostra

PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS <- PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS%>%
  mutate(peoples_count = recode(
    peoples_count, '0'='1', '1'='2', '2'='3','3'='4', '4'='5', '5'='6', '6'='7', '7'='8' 
  ))

PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS$peoples_count <- as.numeric(PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS$peoples_count)


#distribuição original
PONTOS_RISCO_CONTABILIZADOS_SEDEC_COM_PESSOAS%>%
  select(risk,peoples_count)%>%
  ggplot(aes(x=peoples_count)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#0B5BA4")+
  ggthemes::theme_fivethirtyeight()

#distribuição simulada
PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS%>%
  select(risk,peoples_count)%>%
  ggplot(aes(x=peoples_count)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")
  #geom_density(alpha=.2, fill="#FF6666")

sum(PONTOS_RISCO_CONTABILIZADOS_SEDEC_COM_PESSOAS$peoples_count)
sum(PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS$peoples_count)



PONTOS_RISCO_SEDEC_REAIS_E_SIMUALADOS <- rbind(
  PONTOS_RISCO_CONTABILIZADOS_SEDEC_COM_PESSOAS,
  PONTOS_RISCO_CONTABILIZADOS_SEDEC_SEM_PESSOAS)

PONTOS_RISCO_SEDEC_REAIS_E_SIMUALADOS$families_count <- NULL


#COLOCANDO JUNTO COM O BANCO ANTERIOR






