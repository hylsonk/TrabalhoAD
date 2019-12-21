library(simmer)
library(simmer.plot)
library(gridExtra)

doubleQueue <- function(n = 1, lambda1 = 1, lambda2 = 1, mu1 = 1, mu2 = 1, priorityQueue = 0){
  i <- 1
  while(i<n){
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(rexp(1, mu1)) %>%
      release("server")
    
    priority <-
      trajectory("Custome with priority's path") %>%
      seize("server") %>%
      timeout(rexp(1, mu2)) %>%
      release("server")
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() {rexp(100, lambda1)}) %>%
      add_generator("Custome with priority", priority, function() {rexp(100, lambda2)}) %>%
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

r <-doubleQueue(n = 10, lambda1 = 0.05, lambda2 = 0.2 , mu1 = 1, mu2 = 0.5, priorityQueue = 1)

r

singleQueue(1)