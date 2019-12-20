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
      timeout(function() rexp(1, mu1)) %>%
      release("server")
    
    priority <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function() rexp(1, mu2)) %>%
      release("server")
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() rexp(1, lambda1)) %>%
      add_generator("Custome with priority", priority, function() rexp(1, lambda2), priority = priorityQueue) %>%
      run(500)
    
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
      run(500)
    
    i = i+1
  }
}

doubleQueue(1)
singleQueue(1)