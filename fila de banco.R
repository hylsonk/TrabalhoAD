library(simmer)
library(simmer.plot)
library(gridExtra)
library(parallel)

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
    
    server <- mclapply(1:5000, function(i) {
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() {rexp(1, lambda1)}) %>%
      add_generator("Custome with priority", priorityCustomer, function() {rexp(1, lambda2)}, priority = priorityQueue) %>%
      run(100)
    }, mc.set.seed=FALSE)
    
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
    
    server <- mclapply(1:5000, function(i) {
      simmer("server") %>%
        add_resource("server") %>%
        add_generator("Customer", customer, function() {rexp(1, lambda)}) %>%
        run(100/lambda)
      
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
  
}

# r <-doubleQueue(n = 10, lambda1 = 0.05, lambda2 = 0.2 , mu1 = 1, mu2 = 0.5, priorityQueue = 1)

q <- singleQueue(lambda = 0.05, mu = 1)

q