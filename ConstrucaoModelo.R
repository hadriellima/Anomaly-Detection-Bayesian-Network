library(bnlearn)

#Configuraçoes
NUM_PERTUBACOES <- 5
NUM_ALTERACOES_PERTURBACAO <- 3



#Aprendizado da estrutura
estrutura <- hc(dadosTreinamento, score = "bge", restart=NUM_PERTUBACOES, perturb = NUM_ALTERACOES_PERTURBACAO)



#####################################################
 
#Inclusão do modelo temporal
#Criando nós do aspecto temporal
lag_nos <- variaveisLag


#Criando Arcos do aspecto temporal
lag_arcos <- lapply(nodes(estrutura), function(x) {matrix(c(paste("lag", x, sep = "_"), x), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))})

#Criando rede
estruturaCompleta <- empty.graph(c(nodes(estrutura), lag_nos))
arcs(estruturaCompleta) <- rbind(arcs(estrutura), do.call(rbind, lag_arcos))


#####################################################

#Inclusão do modelo temporal 2 niveis atraso
#Criando nós do aspecto temporal
lag_lag_nos <- variaveisLag2


#Criando Arcos do aspecto temporal
lag_lag_arcos <- lapply(nodes(estrutura), function(x) {matrix(c(paste("lag_lag", x, sep = "_"), x), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))})

#Criando rede
estruturaCompleta2niveis <- empty.graph(c(nodes(estruturaCompleta), lag_lag_nos))
arcs(estruturaCompleta2niveis) <- rbind(arcs(estruturaCompleta), do.call(rbind, lag_lag_arcos))


##############################
#Teste sem os nós Oi e Si

#Estimativa dos parâmetros
fitted = bn.fit(estrutura, dadosTreinamento,  method = "mle")
fittedCompleto = bn.fit(estruturaCompleta, dadosTreinamentoCompleto,  method = "mle")
fittedCompleto2niveis = bn.fit(estruturaCompleta2niveis, dadosTreinamentoCompleto2niveis,  method = "mle")



