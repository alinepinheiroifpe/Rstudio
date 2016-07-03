# Rstudio
library(LogisticDx) #pacote contando os dados
library(gridExtra) #para calcular a densidade empírica no RStudio (com  alpha=0.5)
library(ggplot2) #pacote para usar os recursos gráficos do ggplot2
library(MASS) #pra calcular os pontos médios e a densidade empírica
data(bbdm)
dados=bbdm
attach(dados) # com o attach vocÃª pode chamar as colunas pelo nome
head(bbdm)
help(bbdm)
summary(bbdm)
# dados<-read.table(“DADOS_UIS.txt”, header=TRUE)
#aprendi a usar o comnado acima pra importar dados

dados

subset(dados) # filtar dados com subset
subset(dados, MST=="widowed") #exemplo

#vamos tratando as variáveis de acordo com a ordem em que aparecem

#variável AGMT (idade nomomento da entrevista)
#histograma simples referente a idade no momento da entrevista
AGMT
qplot(AGMT, geom="histogram", 
      main = "Idade no Momento da Entrevista",
      xlab = "Idade",ylab = "frequencia",
      binwidth = 0.5,
      fill=I("blue"), 
      col=I("blue"), 
      alpha=I(.2))

#histograma sobre a mesma variável com a densidade empírica
ggplot(data=dados, aes(dados$AGMT)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(27, 68, by = 2), 
                 col="blue", 
                 fill="blue", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Idade no Momento da Entrevista") +
  labs(x="Idade", y="Count")

#variável HIGD (mais alto grau na escola)
#histograma simples
qplot(HIGD, geom="histogram", 
      main = "Mais alto grau na escola",
      xlab = "Grau",ylab = "frequencia",
      binwidth = 0.5,
      fill=I("blue"), 
      col=I("blue"), 
      alpha=I(.2))

#variável DEG (grau de escolaridade)
#Gráfico de setores referente ao grau de escolaridade das entrevistadas

DEG

bb = table(DEG)
cols <- rainbow(length(bb))
pielabels <-   paste(bb/length(DEG)*100,"%", sep="")
par(mar=c(2,2,2,2))
pie(bb, col=cols, main="Grau de Escolaridade", labels=pielabels)
legend("topright", c("Nenhum", 
                     "Primário", "Ensino Médio", "Graduação",
                     "Mestrado", "Doutorado"), cex=0.6, fill=cols)

#variável CHK (realização regular de exames médicos)
#Gráfico de setores referente a realização de exames médicos regulares

CHK

cc = table(CHK)
cols <- rainbow(length(cc))
pielabels <-   paste(cc/length(CHK)*100,"%", sep="")
par(mar=c(2,2,2,2))
pie(cc, col=cols, main="Realização Regular de Exames Médicos", labels=pielabels)
legend("topright", c("Sim","Não"), cex=1, fill=cols)

#variável AGP1 (idade na primeira gravidez)
#Histograma simples referente a idade na primeira gravidez
AGP1
qplot(AGP1, geom="histogram", 
      main = "Idade na primeira gravidez",
      xlab = "Idade",ylab = "frequencia",
      binwidth = 0.5,
      fill=I("blue"), 
      col=I("blue"), 
      alpha=I(.2))

#variável AGMN (idade na menarca)
#histograma simples referente a idade na menarca

AGMN
qplot(AGMN, geom="histogram", 
      main = "Idade na Menarca",
      xlab = "Idade",ylab = "frequencia",
      binwidth = 0.5,
      fill=I("blue"),
      col=I("blue"), 
      alpha=I(.2))

#histograma sobre a mesma variável com a densidade empírica

ggplot(data=dados, aes(dados$AGMN)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(8, 16, by = 1), 
                 col="blue", 
                 fill="blue", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Idade na Menarca") +
  labs(x="Idade", y="Count")

#variável NLV (número de natimortos, abortos; etc.)
#gráfico de dispersão referente ao número de abortos, natimortos;etc.

plot(NLV, main = "Número de Natimortos, abortos, etc.", col = "red",
     xlab = "mulheres", ylab = "fetos/bebês")


boxplot(dados$NLV ~ dados$DEG,
        main="Boxplot de NLV x Grau de Instrução",
        xlab="Grau de Instrução", ylab="N° abortos, natimortos...",
        col=c("yellow","green","red", "blue"))

#variável LIV (numero de nascidos vivos)
#gráfico de dispersão referente ao número de nascidos vivos

plot(LIV, main = "Número de Nascidos Vivos", col = "blue",
     xlab = "mulheres", ylab = "bebês")


boxplot(dados$LIV ~ dados$DEG,
        main="Boxplot de LIV x grau de Instrução",
        xlab="Grau de Instrução", ylab="N° nascidos vivos",
        col=c("yellow","green","red", "blue", "orange"))


#por ultimo fizemos uma tabela de contingencia com as informações NLV e DEG
#para tentar encontrar relações entre o grau de escolaridade e o número de filhos vivos
#é necessário utilizar a função factor pq estamos usando níveis de um fator
#caso não fizesse o R interpretaria como se eu estivesse usando números
#Mas na verdade vc tá usando níveis de um fator
#Pq são variáveis categóricas

ee = factor (NLV)
table(ee,DEG)

ff = factor (LIV)
table(ff,DEG)

#variavel WT (peso)
#primeiro convertemos a unidade de medida de massa em libras para quilos

WT
massa<-WT*0.453592

#depois, fizemos um boxplot
boxplot(massa,main = "Massa corporea em Kg", col = 520)

#depois fizemos um histograma
hist(massa, freq=FALSE, breaks=20, col=520, main = "Massa corporea em Kg")

#e agora guardamos esse histograma na variável pp
pp = hist(massa, freq=FALSE, breaks=20, col="blue")

#agora pedimos pra o R plotar os pontos médios das barras versus a densidade empírica

plot(pp$mids,pp$density, pch=20, lwd=3, main = "Estimação por máxima Verossimilhança")

#agora vamos estimar os parametros da distribuiÃ§Ã£o normal
# o pacote MASS uq estamos usando tem a funÃ§Ã£o para estimaÃ§Ã£o de verossimilhaÃ§a 
#para algumas distribuiÃ§Ãµes, e a normal Ã© uma delas
# entÃ£o vamos usar a funÃ§Ã£o fitdistr da seguinte forma:
#a estimaÃ§Ã£o usada foi estimaÃ§Ã£o por mÃ¡xima verossimilhanÃ§a

f = fitdistr(massa, densfun = "normal")   
# e agora rodando sÃ³ o f, teremos uma mÃ©dia e um desvio padrÃ£o

f

#agora vamos desenhar a curva da densidade de uma normal com esses parÃ¢metros. 
#Basta fazer:
#dnorm Ã© uma funÃ§Ã£o pronta do R que recebe um valor x e os parametros e retorna a 
#densidade naquele ponto
#lwd Ã© a grossura da linha
#add=TRUE Ã© pra ele colocar no ultimo grÃ¡fico

curve(dnorm(x,f$estimate[1],f$estimate[2]),col="red", lwd=3,add=TRUE)


#variavel AGLP (idade nomomento da ultima menstruação)
#histograma simples referente a idade na menarca
AGLP
qplot(AGLP, geom="histogram", 
      main = "Idade na Ultima Menstruação",
      xlab = "Idade",ylab = "frequencia",
      binwidth = 0.5,
      fill=I("blue"), 
      col=I("blue"), 
      alpha=I(.2))

#histograma sobre a mesma variável com a densidade empírica

ggplot(data=dados, aes(dados$AGLP)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(21, 56, by = 1), 
                 col="blue", 
                 fill="blue", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Idade na Ultima Menstruação") +
  labs(x="Idade", y="Count")

#variavel MST (estado civil)
#gráfico de setores referente ao estado civil das entrevistadas

MST

aa = table(MST)
cols <- rainbow(length(aa))
pielabels <-   paste(aa/length(MST)*100,"%", sep="")
par(mar=c(2,2,2,2))
pie(aa, col=cols, main="Estado Civil", labels=pielabels)
legend("topright", c("Casada", 
                     "Divorciada", "Separada", 
                     "ViÚva", "Nunca casou"), cex=0.8, fill=cols)






