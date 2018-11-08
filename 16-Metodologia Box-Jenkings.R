                    #Aula 16 - Metodologia Box- Jenkings

remove.packages("readxl")
install.packages("readxl", dependencies = T)
remove.packages("aTSA")
install.packages("aTSA", dependencies = T)
remove.packages("tseries")
install.packages("tseries", dependencies = T)

library(readxl)
library(aTSA)
library(tseries)
library("urca") 

BITCOIN <- na.omit(read_excel("C:/EconometriaA/Bitcoin.xlsx"))

Bitcoin <-  ts(log(BITCOIN$Close), start = 2014, frequency = 365)

plot(Bitcoin, type="l", main="Logaritmos do Preço do Bitcoin", ylab="Log Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")


#Criar FAC  e FACP

acf(log(BITCOIN$Close),lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(log(BITCOIN$Close),lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
summary(ur.df(Bitcoin, "none", lags = 1))

#Teste Philips-Perron
pp.test(Bitcoin)

#Teste KPSS
kpss.test(Bitcoin)

#Se não for estacionária, diferenciar a série

IntOrdem1 <- diff(log(BITCOIN$Close))
IntegradaOrdem1 <- ts(IntOrdem1, start = 2014, frequency = 365)

plot(IntegradaOrdem1, type="l", main="Primeira Diferança dos Logs do Bitcoin - LogReturn", ylab="Log Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")

#Verificar se a Série se tornou Estacionária

#FAC e FACP

acf(IntOrdem1,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(IntOrdem1,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
ur.df(IntegradaOrdem1, "none", lags = 1)

#Teste Philips-Perron
pp.test(IntegradaOrdem1)

#Teste KPSS
kpss.test(IntegradaOrdem1)


#Estimando Regressões e Tabelando Resultados
AR1 <- arima(IntegradaOrdem1, order = c(1,1,0))   #Estima o AR1 e salva os resultados como AR1
AR2 <- arima(IntegradaOrdem1, order = c(2,1,0))   #Estima o AR2 e salva os resultados como AR2
AR3 <- arima(IntegradaOrdem1, order = c(3,1,0))   #Estima o AR3 e salva os resultados como AR3
#COMPLETAR ATÉ AR21

estimacoes <- list(AR1, AR2,AR3,AR4,AR5,
                   AR6,AR7,AR8,AR9,AR10,
                   AR11,AR12,AR13,AR14,AR15,
                   AR16,AR17,AR18,AR19,AR20,AR21)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

AIC <- sapply(estimacoes, AIC)      #Cria Coluna com resultados AIC
BIC <- sapply(estimacoes, BIC)      #Cria Coluna com resultados BIC
Modelo <- c("AR1", "AR2","AR3","AR4","AR5",
            "AR6","AR7","AR8","AR9","AR10",
            "AR11","AR12","AR13","AR14","AR15",
            "AR16","AR17","AR18","AR19","AR20","AR21")   #cria coluna com nome dos modelos

Resultados <- data.frame(Modelo, AIC, BIC)  #Junta as três colunas acima num único resultado
View(Resultados)

#todo código acima pode ser resumido no código abaixo
#est1 <- data.frame()
#for (i in 1:21) {                 #Loop para os AR: ARIMA(i,0,0)
#  est1[i,1] <- paste("AR",i)      #Coluna com os nomes do Modelo
#  est1[i,2] <- AIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores AIC
#  est1[i,3] <- BIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores BIC
#}
#Resultados <- data.frame(rbind(est1))  
#colnames(Resultados) <- c("Modelo","AIC","BIC")

#Efetuar teste ARCH-LM para o melhor modelo

AR1 <- arima(IntegradaOrdem1, order = c(1,1,0))
MA1 <- arima(IntegradaOrdem1, order = c(0,1,1))
AR1

#Previsões

predict(AR1,15)

library(forecast)
forecast(AR1,15)

plot(forecast(AR1,5), type="o", xlim=c(2018.75,2018.85), ylim=c(-0.03,0.06))
grid(col = "black", lty = "dotted")

arch.test(AR1)

#Modelando a Variância

residuos <- AR1$residuals
plot(residuos, type="o", main="Residuos do AR1")
grid(col = "black", lty = "dotted")

#FAC  e FACP  dos Residuos

acf(residuos,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(residuos,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")


GARCH202=garch(residuos,c(20,2),trace=F)



