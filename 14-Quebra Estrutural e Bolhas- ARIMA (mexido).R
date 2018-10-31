                    #Aula 14 - Quebra Estrutural e Bolhas

install.packages("strucchange")                       #Instala o pacote "strucchange"

library(strucchange)                                  #Carrega o pacote "strucchange"
library(readxl)                                       #Carrega o pacote "readxl"


BITCOIN <- read_excel("C:/Econometria/Bitcoin.xls")
View(Bitcoin)
Bitcoin <- ts <- ts(BITCOIN$Close, start = 2017, frequency = 365)

plot(Bitcoin)

#Teste de Chow

chow <- Fstats(Bitcoin~1)    #Executa o Teste de F de Chow
sctest(chow)                 #Retorna a Estatística de Teste e o p-valor

plot(Bitcoin)
lines(breakpoints(chow))

plot(chow)
    
#Teste Bai Perron

bp_ts <- breakpoints(Bitcoin ~ 1)

bp_ts

summary(bp_ts)

#ci_ts <- confint(bp_ts)

plot(Bitcoin)               
lines(bp_ts)            #Gráfico com os breakpoints


#Gráfico com as linhas de tendências para os três períodos

fm0 <- lm(Bitcoin ~ 1)
fm1 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 1))
fm2 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 2))
fm3 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 3))
fm4 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 4))
plot(Bitcoin)
lines(ts(fitted(fm0), start = 2017, frequency=365), col = 3)
lines(ts(fitted(fm1), start = 2017, frequency=365), col = 4)
lines(ts(fitted(fm2), start = 2017, frequency=365), col = 1)
lines(ts(fitted(fm3), start = 2017, frequency=365), col = 2)
lines(ts(fitted(fm4), start = 2017, frequency=365), col = 5)
lines(bp_ts)


#Estimar o Melhor Modelo ARIMA

#Modelo Integrado de Ordem 1

MIO1 <- diff(Bitcoin)
plot(MIO1)

#É estacionária?

#FAC e FACP

#Qual a ordem do modelo ARIMA(p,d,q)

#Quais combinações a serem estimadas?

#Os valores AIC e BIC dos modelos possíveis.

#O melhor modelo.

#Previsão para os 6 próximos meses do valor do Bitcoin utilizando o melhor modelo.



