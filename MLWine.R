set.seed(1234567890)
library( neuralnet )
library( hydroGOF )
library( leaps )
library( arules )
library( BBmisc)
#red wine
dadosRed <- read.csv("~/Documents/Dev/MLWine/Datasets/winequality-red.csv",header=TRUE,sep=";",dec=".")
#white wine
dadosWhite <- read.csv("~/Documents/Dev/MLWine/Datasets/winequality-white.csv",header=TRUE,sep=";",dec=".")

#junta os dados
dados <- rbind(dadosRed, dadosWhite)

#randomize dados
dadosR <- dados[sample(nrow(dados)),]
dadosR[,1:11] <- normalize(dadosR[,1:11], method="range", range=c(0,1))

#casos para treino:
dadosTreino <- dadosR[1:4500, ]

#casos para teste:
dadosTeste <- dadosR[4501:6497, ]

# defini????o das camadas de entrada e sa??da da RNA
funcao <- quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol
selecao1<-regsubsets(funcao,dadosR,nvmax=11)
summary(selecao1)

selecao2<-regsubsets(funcao,dadosR,nvmax=5)
summary(selecao2)

funcaoOpt <- quality ~ alcohol+volatile.acidity+sulphates+residual.sugar+total.sulfur.dioxide
funcaoOpt2 <- quality ~ alcohol+volatile.acidity+sulphates

# treinar a rede neuronal
rnaWine11 <- neuralnet( funcao, dadosTreino, lifesign="full", hidden = c(7,3), threshold = 0.1)
rnaWine12 <- neuralnet( funcaoOpt, dadosTreino, lifesign="full", hidden = c(7,3), threshold = 0.1)
rnaWine13 <- neuralnet( funcaoOpt2, dadosTreino, lifesign="full", hidden = c(7,3), threshold = 0.1)

rnaWine21 <- neuralnet( funcao, dadosTreino, hidden = c(8,4,2), threshold = 0.1)
rnaWine22 <- neuralnet( funcaoOpt, dadosTreino, hidden = c(8,4,2), threshold = 0.1)
rnaWine23 <- neuralnet( funcaoOpt2, dadosTreino, hidden = c(8,4,2), threshold = 0.1)

# definir variaveis de input para teste
dadosTeste1 <- subset(dadosTeste, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol"))
dadosTeste2 <- subset(dadosTeste, select = c("alcohol","volatile.acidity","sulphates","residual.sugar","total.sulfur.dioxide"))
dadosTeste3 <- subset(dadosTeste, select = c("alcohol","volatile.acidity","sulphates"))

# testar a rede com os novos casos
rnaWine11.resultados <- compute(rnaWine11,dadosTeste1)
rnaWine12.resultados <- compute(rnaWine12,dadosTeste2)
rnaWine13.resultados <- compute(rnaWine13,dadosTeste3)
rnaWine21.resultados <- compute(rnaWine21,dadosTeste1)
rnaWine22.resultados <- compute(rnaWine22,dadosTeste2)
rnaWine23.resultados <- compute(rnaWine23,dadosTeste3)

# comparar resultados
resultados11 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine11.resultados$net.result)
resultados12 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine12.resultados$net.result)
resultados13 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine13.resultados$net.result)
resultados21 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine21.resultados$net.result)
resultados22 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine22.resultados$net.result)
resultados23 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine23.resultados$net.result)

# resultados arredondados
resultados11$previsao <- round(resultados11$previsao, digits=0)
resultados12$previsao <- round(resultados12$previsao, digits=0)
resultados13$previsao <- round(resultados13$previsao, digits=0)
resultados21$previsao <- round(resultados21$previsao, digits=0)
resultados22$previsao <- round(resultados22$previsao, digits=0)
resultados23$previsao <- round(resultados23$previsao, digits=0)

# calcular o RMSE
rmse(c(dadosTeste$quality),c(resultados11$previsao))
rmse(c(dadosTeste$quality),c(resultados12$previsao))
rmse(c(dadosTeste$quality),c(resultados13$previsao))
rmse(c(dadosTeste$quality),c(resultados21$previsao))
rmse(c(dadosTeste$quality),c(resultados22$previsao))
rmse(c(dadosTeste$quality),c(resultados23$previsao))