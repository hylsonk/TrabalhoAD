library(simmer)
library(simmer.plot)
library(gridExtra)

doubleQueue <- function(n = 1, lambda1 = 1, lambda2 = 1, mu1 = 1, mu2 = 1, priorityQueue = 0){
  i <- 1
  while(i<n){
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
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() {rexp(1, lambda1)}) %>%
      add_generator("Custome with priority", priorityCustomer, function() {rexp(1, lambda2)}, priority = priorityQueue) %>%
      run(1000)
    
      print("----------------------------CUSTOMER----------------------------------")
      print("arrivals")
      arrivals <- get_mon_arrivals(server)
      print(transform(arrivals, waiting_time = end_time - start_time - activity_time))
      
      print("----------------------------SERVER----------------------------------")
      print("resources")
      resources <- get_mon_resources(server)
      print(transform(resources))
      
      
      print("--------------------------------------------------------------")
    
    i = i+1
  }
}

singleQueue <- function(n = 1, lambda = 1, mu = 1){
  i <- 1
  while(i<n){
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function(){rexp(1, mu)}) %>%
      release("server")
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() {rexp(1, lambda)}) %>%
      run(1000)
    
    print("----------------------------CUSTOMER----------------------------------")
    print("arrivals")
    arrivals <- get_mon_arrivals(server)
    print(transform(arrivals, waiting_time = end_time - start_time - activity_time))
    
    print("----------------------------SERVER----------------------------------")
    print("resources")
    resources <- get_mon_resources(server)
    print(transform(resources))
    
    
    print("--------------------------------------------------------------")
    
    i = i+1
  }
  
}

# r <-doubleQueue(n = 10, lambda1 = 0.05, lambda2 = 0.2 , mu1 = 1, mu2 = 0.5, priorityQueue = 1)

q <- singleQueue(n= 10, lambda = 0.05, mu = 0.5)

q