
alvo <- "PM.3"
#pred <- predict(fittedCompleto, alvo, dadosTesteCompleto)
#result <- cbind(dadosTesteCompleto[, alvo], pred, abs(dadosTesteCompleto[, alvo]-pred)) 
#write.table(result, "resultado.csv", sep=";", dec = ",")
#write.table(datasTeste, "datas.csv", sep=";", dec = ",")

estaFuncionando = TRUE;
for (i in 1:nrow(dadosTesteCompleto)) {
  if(estaFuncionando) {
    previsto <- predict(fittedCompleto, alvo, dadosTesteCompleto[i, ])
  } else {
    previsto <- predict(fitted, alvo, dadosTesteCompleto[i, 1:ncol(dadosTeste)])
  }
  
  observado <- dadosTesteCompleto[i, alvo]
  diferenca <- abs(observado-previsto)
  
  inferenciaNet <- model2network("[S][O|S]")
  
  parametros_S <- matrix(c(0.9, 0.1), ncol = 2, dimnames = list(NULL, c("working", "broken")))
  parametros_O <- list(coef = matrix(c(previsto, 0.0001*previsto), ncol = 2, dimnames = list(c("(Intercept)"), NULL)), sd = c(0.1, 10000))
  
  inferenciaFit = custom.fit(inferenciaNet, dist = list(O = parametros_O, S = parametros_S))
  
  #cpquery(inferenciaFit, event = (S=="working"),  evidence = (O == observado & X == previsto))
  #table(cpdist(inferenciaFit, "S",  evidence = (O == observado)))
  
  teste[i, 1] <- observado
  teste[i, 2] <- previsto
  teste[i, 3] <- diferenca
  teste[i, 4] <- cpquery(inferenciaFit, event = (S=="working"), evidence = list(O=observado), method = "lw")
  teste[i, 5] <- cpquery(inferenciaFit, event = (S=="broken"), evidence = list(O=observado), method = "lw")
  
  if(teste[i, 4] > 0.5) {
    estaFuncionando = TRUE;
  } else {
    estaFuncionando = FALSE
  }
  
  teste[i, 6] = estaFuncionando
  
  #teste[i, 4] <- cpquery(inferenciaFit, (S=="working"), (O>=observado-0.01) & (O<=observado+0.01))
  #teste[i, 5] <- cpquery(inferenciaFit, (S=="broken"), (O>=observado-0.01) & (O<=observado+0.01))
}

write.table(teste, paste(pastaArquivos, "/testeInferencia_", alvo,".csv", sep = "") , sep=";", dec = ",")

#table(cpdist(inferenciaFit, "S", evidence = list(O=0.19), method = "lw"))
#cpquery(inferenciaFit, event = (S=="working"), evidence = list(O=0.19), method = "lw")
