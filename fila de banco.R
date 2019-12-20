library(simmer)
library(simmer.plot)
library(gridExtra)

set.seed(1933)

doubleQueue <- function(n = 1, lambda1 = 1, lambda2 = 1, mu1 = 1, mu2 = 1, priorityQueue = 0){
  i <- 1
  while(i<n){
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function() c(0,rexp(1, mu1),-1)) %>%
      release("server")
    
    priority <-
      trajectory("Custome with priority's path") %>%
      seize("server") %>%
      timeout(function() c(0,rexp(1, mu2),-1)) %>%
      release("server")
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() c(0,rexp(100000, lambda1),-1)) %>%
      add_generator("Custome with priority", priority, function() c(0,rexp(100000, lambda2),-1), priority = priorityQueue) %>%
      run(100)
    arrivals <- get_mon_arrivals(server)
    return(arrivals)
    i = i+1
  }
}

singleQueue <- function(n = 1, lambda = 1, mu1 = 1){
  i <- 1
  while(i<n){
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function() rexp(1, mu1)) %>%
      release("server")
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() rexp(1, lambda1)) %>%
      run(100)
    
    i = i+1
  }
  
}

r <-doubleQueue(n = 10, lambda1 = 0.05, lambda2 = 0.2 , mu1 = 1, mu2 = 0.5, priorityQueue = 0)

r

singleQueue(1)