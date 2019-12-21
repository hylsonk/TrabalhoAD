library(simmer)
library(simmer.plot)
library(gridExtra)
library(parallel)

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
    
      print("----------------------------CUSTOMERs----------------------------------")
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
    customer <-
      trajectory("Customer's path") %>%
      seize("server") %>%
      timeout(function(){rexp(1, mu)}) %>%
      release("server")
    
    server <- mclapply(1:100, function(i) {
      simmer("server") %>%
        add_resource("server") %>%
        add_generator("Customer", customer, function() {rexp(1, lambda)}) %>%
        run(100/lambda)
      
    }, mc.set.seed=FALSE)
    
    
    print("----------------------------CUSTOMER----------------------------------")
    print("arrivals")
    arrivals <- get_mon_arrivals(server) %>%
    transform(waiting_time = end_time - start_time - activity_time)
    print(arrivals)
    print("means")
    
    mean_waiting_time <- mean(arrivals$waiting_time)
    
    print(paste0("E[W] = ", mean_waiting_time))
    
    
    print("----------------------------SERVER----------------------------------")
    print("resources")
    resources <- get_mon_resources(server)
    print(transform(resources))
    
    # print(aggregate(cbind(server, queue) ~ resource, get_mon_resources(server), mean))
    
    print("--------------------------------------------------------------")
  
}

# r <-doubleQueue(n = 10, lambda1 = 0.05, lambda2 = 0.2 , mu1 = 1, mu2 = 0.5, priorityQueue = 1)

q <- singleQueue(n = 10, lambda = 0.2, mu = 0.5)

q