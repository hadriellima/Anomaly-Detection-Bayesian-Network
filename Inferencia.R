###########################
# Configurações
dadosTesteInferencia <- dadosTesteCompleto2niveis
fittedTesteInferencia <- fittedCompleto
###########################

falhas <- list()
todosAlvos <- names(dadosTreinamento)
for (alvo in todosAlvos){
  falhas <- append(falhas, alvo)
  estaFuncionando = 0;
  teste <- matrix(nrow = nrow(dadosTesteInferencia), ncol=6)
  for (i in 1:nrow(dadosTesteInferencia)) {
    if(estaFuncionando <= 1) {
      previsto <- predict(fittedTesteInferencia, alvo, dadosTesteInferencia[i, ])
    } else {
      previsto <- predict(fitted, alvo, dadosTesteInferencia[i, 1:ncol(dadosTeste)])
    }
    
      
    observado <- dadosTesteInferencia[i, alvo]
    diferenca <- abs(observado-previsto)
    
    inferenciaNet <- model2network("[S][O|S]")
    
    parametros_S <- matrix(c(0.9, 0.1), ncol = 2, dimnames = list(NULL, c("working", "broken")))
    parametros_O <- list(coef = matrix(c(previsto, 0.0001*previsto), ncol = 2, dimnames = list(c("(Intercept)"), NULL)), sd = c(0.1, 10000))
    
    inferenciaFit = custom.fit(inferenciaNet, dist = list(O = parametros_O, S = parametros_S))
    
    teste[i, 1] <- observado
    teste[i, 2] <- previsto
    teste[i, 3] <- diferenca
    teste[i, 4] <- cpquery(inferenciaFit, event = (S=="working"), evidence = list(O=observado), method = "lw")
    teste[i, 5] <- cpquery(inferenciaFit, event = (S=="broken"), evidence = list(O=observado), method = "lw")
    
    if(teste[i, 4] > 0.5) {
      if(estaFuncionando >= 2){
        falhas <- append(falhas, paste("Fim", datasTesteCompleto2niveis[i], sep = ": "))
      }  
      estaFuncionando <- 0
    } else {
      estaFuncionando <- estaFuncionando + 1
      if(estaFuncionando == 2) {
        falhas <- append(falhas, paste("Inicio", datasTesteCompleto2niveis[i], sep = ": "))
      }
    }
    
    teste[i, 6] = estaFuncionando
    
  }
  
  write.table(teste, paste(pastaArquivos, "/testeInferencia_", alvo,".csv", sep = "") , sep=";", dec = ",")
}

write.table(unlist(falhas), paste(pastaArquivos, "/falhas.csv", sep = "") , sep=";", dec = ",")
