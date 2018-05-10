set.seed(1234567890)
library( neuralnet )
library( hydroGOF )
library( leaps )
library( arules )
library( BBmisc)
#red wine
dadosRed <- read.csv("~/MLWine/datasets/winequality-red.csv",header=TRUE,sep=";",dec=".")
#white wine
dadosWhite <- read.csv("~/MLWine/datasets/winequality-white.csv",header=TRUE,sep=";",dec=".")

#junta os dados
dados <- rbind(dadosRed, dadosWhite)

#randomize dados
dadosR <- dados[sample(nrow(dados)),]
dadosR[,1:11] <- normalize(dadosR[,1:11], method="range", range=c(0,1))
dadosR[,12] <- dadosR[,12]*0.1

#casos para treino:
dadosTreino <- dadosR[1:4500, ]

#casos para teste:
dadosTeste <- dadosR[4501:6497, ]

# definição das camadas de entrada e saída da RNA
funcao <- quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol
funcaoOpt <- quality ~ alcohol+volatile.acidity+sulphates+residual.sugar+total.sulfur.dioxide
funcaoOpt2 <- quality ~ alcohol+volatile.acidity+sulphates

selecao1<-regsubsets(funcao,dadosR,nvmax=11)
summary(selecao1)
selecao2<-regsubsets(funcao,dadosR,nvmax=5)
summary(selecao2)

# treinar a rede neuronal 
# only using 'funcao' at this moment
rnaWine1 <- neuralnet( funcao, dadosTreino, lifesign="full", hidden = c(7,4), threshold = 0.1)
rnaWine2 <- neuralnet( funcao, dadosTreino, lifesign="full", hidden = c(10,8), threshold = 0.1)
rnaWine3 <- neuralnet( funcao, dadosTreino, lifesign="full", hidden = c(3,3), threshold = 0.1)

# definir variaveis de input para teste
dadosTeste1 <- subset(dadosTeste, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol"))
dadosTeste2 <- subset(dadosTeste, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol"))
dadosTeste3 <- subset(dadosTeste, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol"))

# testar a rede com os novos casos
rnaWine1.resultados <- compute(rnaWine1,dadosTeste1)
rnaWine2.resultados <- compute(rnaWine2,dadosTeste2)
rnaWine3.resultados <- compute(rnaWine3,dadosTeste3)

# comparar resultados
resultados1 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine1.resultados$net.result)
resultados2 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine2.resultados$net.result)
resultados3 <- data.frame(atual = dadosTeste$quality, previsao = rnaWine3.resultados$net.result)

# resultados arredondados
resultados1$previsao <- round(resultados1$previsao, digits=1)
resultados2$previsao <- round(resultados2$previsao, digits=1)
resultados3$previsao <- round(resultados3$previsao, digits=1)

# calcular o RMSE
rmse(c(dadosTeste$quality),c(resultados1$previsao))
rmse(c(dadosTeste$quality),c(resultados2$previsao))
rmse(c(dadosTeste$quality),c(resultados3$previsao))
