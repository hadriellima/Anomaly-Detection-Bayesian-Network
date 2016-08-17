library(bnlearn)

#Configuraçoes
NUM_PERTUBACOES <- 2
NUM_ALTERACOES_PERTURBACAO <- 1



#Aprendizado da estrutura
estrutura <- hc(dadosTreinamento, score = "bge")#, restart=NUM_PERTUBACOES, perturb = NUM_ALTERACOES_PERTURBACAO)


##################################
#Acho que esta função retorna o Essential Graph. Encontrei um artigo o chamando de CPDAG. Pesquisar se realmente é isto
#teste <- cpdag(estrutura)
#Estudarei este mecanismo posteriormente. Montarei o aspecto temporal sobre a estrutura retornada 


#####################################################
 
#Inclusão do modelo temporal
#Criando nós do aspecto temporal
lag_nos <- variaveisLag


#Criando Arcos do aspecto temporal
lag_arcos <- lapply(nodes(estrutura), function(x) {matrix(c(paste("lag", x, sep = "_"), x), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))})

#Criando rede
estruturaCompleta <- empty.graph(c(nodes(estrutura), lag_nos))
arcs(estruturaCompleta) <- rbind(arcs(estrutura), do.call(rbind, lag_arcos))



##############################
#Teste sem os nós Oi e Si

#Estimativa dos parâmetros
fitted = bn.fit(estrutura, dadosTreinamento,  method = "mle")
fittedCompleto = bn.fit(estruturaCompleta, dadosTreinamentoCompleto,  method = "mle")



