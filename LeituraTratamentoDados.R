library(scales)


#Leitura do arquivo de dados
pastaArquivos <- "Volume10min"
arquivoDadosTreinamento <- paste(pastaArquivos, "DadosTreinamento.csv", sep = "/") 
arquivoDadosTeste  <- paste(pastaArquivos, "DadosTeste.csv", sep = "/")  

dadosTreinamento <- read.csv2(arquivoDadosTreinamento, sep = ";", dec = ",", na.strings = c("I/O Timeout", "Bad Input", "#VALOR!"))

dadosTeste <- read.csv2(arquivoDadosTeste, sep = ";", dec = ",", na.strings = c("I/O Timeout", "Bad Input", "#VALOR!"))


#Removendo coluna Data que não utilizada no aprendizado da estrutura
dadosTreinamento <- dadosTreinamento[, !toupper(colnames(dadosTreinamento)) == "DATA"]


datasTeste <- dadosTeste[1:nrow(dadosTeste), 1]
datasTesteCompleto <- dadosTeste[2:nrow(dadosTeste), 1]
datasTesteCompleto2niveis <- dadosTeste[3:nrow(dadosTeste), 1]
dadosTeste <- dadosTeste[, !toupper(colnames(dadosTeste)) == "DATA"]


#Conversão de todas as colunas para o tipo numeric. Necessário na pontuação (fit)
#dadosTreinamento <- data.frame(apply(dadosTreinamento, 2, as.numeric))
#dadosTeste <- data.frame(apply(dadosTeste, 2, as.numeric))


#Todos os valores na escala de 0 a 1
for (i in 1: ncol(dadosTreinamento)) {
  dadosTeste[, i] <-  rescale(dadosTeste[, i], from=c(min(dadosTreinamento[, i]), max(dadosTreinamento[, i])));
  dadosTreinamento[, i] <-  rescale(dadosTreinamento[, i]);
}

variaveisLag <- paste("lag", names(dadosTreinamento), sep = "_")
variaveisLag2 <- paste("lag_lag", names(dadosTreinamento), sep = "_")

################################
#Montando conjunto de dados com as variáveis lag
dadosTreinamentoCompleto <- cbind(dadosTreinamento[2:nrow(dadosTreinamento), ], dadosTreinamento[1:nrow(dadosTreinamento)-1, ])
names(dadosTreinamentoCompleto) <- c(names(dadosTreinamento), variaveisLag)

dadosTesteCompleto <- cbind(dadosTeste[2:nrow(dadosTeste), ], dadosTeste[1:nrow(dadosTeste)-1, ])
names(dadosTesteCompleto) <- c(names(dadosTeste), variaveisLag)


################################
#Montando conjunto de dados com as variáveis lag 2 niveis
dadosTreinamentoCompleto2niveis <- cbind(dadosTreinamentoCompleto[2:nrow(dadosTreinamentoCompleto), ], dadosTreinamento[2:nrow(dadosTreinamento)-2, ])
names(dadosTreinamentoCompleto2niveis) <- c(names(dadosTreinamentoCompleto), variaveisLag2)

dadosTesteCompleto2niveis <- cbind(dadosTesteCompleto[2:nrow(dadosTesteCompleto), ], dadosTeste[2:nrow(dadosTeste)-2, ])
names(dadosTesteCompleto2niveis) <- c(names(dadosTesteCompleto), variaveisLag2)



################################
#Removendo espurios e dados inválidos
datasTeste <- datasTeste[complete.cases(dadosTeste)]
dadosTeste <- dadosTeste[complete.cases(dadosTeste),]

datasTesteCompleto <- datasTesteCompleto[complete.cases(dadosTesteCompleto)]
dadosTesteCompleto <- dadosTesteCompleto[complete.cases(dadosTesteCompleto),]

datasTesteCompleto2niveis <- datasTesteCompleto2niveis[complete.cases(dadosTesteCompleto2niveis)]
dadosTesteCompleto2niveis <- dadosTesteCompleto2niveis[complete.cases(dadosTesteCompleto2niveis),]


linhasTesteOk <- apply(dadosTeste < -10 | dadosTeste > 10, 1, sum) == 0
datasTeste <- datasTeste[linhasTesteOk]
dadosTeste <- dadosTeste[linhasTesteOk,]

linhasTesteCompletoOk <- apply(dadosTesteCompleto < -10 | dadosTesteCompleto > 10, 1, sum) == 0
datasTesteCompleto <- datasTesteCompleto[linhasTesteCompletoOk]
dadosTesteCompleto <- dadosTesteCompleto[linhasTesteCompletoOk,]


linhasTesteCompleto2niveisOk <- apply(dadosTesteCompleto2niveis < -10 | dadosTesteCompleto2niveis > 10, 1, sum) == 0
datasTesteCompleto2niveis <- datasTesteCompleto2niveis[linhasTesteCompleto2niveisOk]
dadosTesteCompleto2niveis <- dadosTesteCompleto2niveis[linhasTesteCompleto2niveisOk,]
