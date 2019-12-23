library(simmer)
library(simmer.plot)
library(gridExtra)
library(parallel)
library(rstudioapi)
#Setando a localiza��o dos arquivos, importante para importar as planilhas
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

set.seed(1234)

doubleQueue <- function(n = 1, lambda1 = 1, lambda2 = 1, mu1 = 1, mu2 = 1, priorityQueue = 0){
  
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function(){rexp(1, mu1)}) %>%
      release("server")
    
    priorityCustomer <-
      trajectory("Custome with priority's path") %>%
      seize("server") %>%
      timeout(function(){rexp(1, mu2)}) %>%
      release("server")
    
    server <- mclapply(1:100, function(i) {
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() {rexp(1, lambda1)}) %>%
      
      add_generator("Custome with priority", priorityCustomer, function() {rexp(1, lambda2)}, priority = priorityQueue) %>%
      run(100)
    }, mc.set.seed=FALSE)
      get_queue_size(server, "server")
      print("----------------------------CUSTOMES----------------------------------")
      print("arrivals")
      arrivals <- get_mon_arrivals(server)
      print(transform(arrivals, waiting_time = end_time - start_time - activity_time))
      
      print("----------------------------SERVER----------------------------------")
      print("resources")
      resources <- get_mon_resources(server)
      print(transform(resources))
      
      
      print("--------------------------------------------------------------")
      
    
}

singleQueue <- function(lambda = 1, mu = 1){
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function(){rexp(1, mu)}) %>%
      release("server")
    
    server <- mclapply(1:100, function(i) {
      simmer("server") %>%
        add_resource("server") %>%
        add_generator("Customer", customer, function() {rexp(1, lambda)}) %>%
        run(2000)
      
    }, mc.set.seed=FALSE)
    
    
    print("----------------------------CUSTOMER----------------------------------")
    print("arrivals")
    arrivals <- get_mon_arrivals(server) %>%
      # dplyr::group_by(replication) %>%
      # dplyr::summarise( mean = mean(end_time - start_time))  %>%
      transform(waiting_time = end_time - start_time - activity_time)
    print(arrivals)
    # print(t.test(arrivals[["mean"]]))
    
    
    print("----------------------------SERVER----------------------------------")
    print("resources")
    resources <- get_mon_resources(server)
      # dplyr::group_by(replication)
    print(transform(resources))
    
    
    print("---------------------------MEANS-----------------------------------")
    mean_waiting_time <- mean(arrivals$waiting_time)
    
    print(paste0("E[W] = ", mean_waiting_time))
    
    mean_queue <- mean(resources$queue)
    
    print(paste0("E[Nq] = ", mean_queue))
    
    print("--------------------------------------------------------------")
    return(data.frame(mean_waiting_time, mean_queue))
  
}

#####################################
#  Fila sem Prioridade - Cen�rio 1  #
#####################################
lambdas=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9)
mu=1
n=length(lambdas)

#inicialiando o DataFrame
c1 = singleQueue(lambda = lambdas[1], mu = mu)
novo_frame <- data.frame("lambda"=lambdas[1],"E.W"=c1$mean_waiting_time,"E.Nq."=c1$mean_queue)
#realizando os experimentos restantes
for(i in 2:n){
  c1 = singleQueue(lambda = lambdas[i], mu = mu)
  novo_frame[i,]<- data.frame("lambda"=lambdas[i],c1$mean_waiting_time,c1$mean_queue)
}

#Carregar dados analiticos
anal_c1 <- read.csv(file="cenario1.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
head(anal_c1)

#Plotar os gr�ficos
plot(novo_frame$lambda,novo_frame$E.W, type='l', ylab = 'E[W]', xlab = 'lambda',lwd = 3, main="Fila sem Prioridade - Cen�rio 1", col="red")
lines(novo_frame$lambda,anal_c1$E.W, col="blue",lwd = 3)
legend("topleft", c("Simula��o","Anal�tico"),fill=c("red","blue"))
#Resultado do Cen�rio 1 sem prioridade
novo_frame

#####################################
#  Fila sem Prioridade - Cen�rio 2  #
#####################################
#r <-doubleQueue(n = 10, lambda1 = 0.05, lambda2 = 0.2 , mu1 = 1, mu2 = 0.5, priorityQueue = 1)
lambdas1=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6)
mu1=1
lambdas2=c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2)
mi2=0.5
n=length(lambdas1)

#ler dados anal�ticos
anal_c2 <- read.csv(file="cenario2.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
head(anal_c2)

#plotar analitico
plot(anal_c2$lambda1, anal_c2$E.W1, type='l', ylab = 'E[W]', xlab = 'lambda',lwd = 3, main="Analitico s/ pri - Cen�rio 2", col="red")
plot(anal_c2$Lambda2, anal_c2$E.W2, type='h',col="blue",lwd = 3)
legend("topleft", c("A_Fila1", "A_Fila2"),fill=c("red", "blue"))
layout(1)
