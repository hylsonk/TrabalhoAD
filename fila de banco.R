library(simmer)
library(simmer.plot)
library(gridExtra)

set.seed(1933)




start <- function(n = 100){
  i <- 1
  while(i<10){
    server <- simmer()
    
    customer <-
      trajectory("Customer's path") %>%
      set_attribute("start_time", function() {now(server)}) %>%
      log_(function() {
        paste("Queue is", get_queue_count(server, "server"), "on arrival")
      }) %>%
      seize("server") %>%
      log_(function() {paste("Waited", now(server) - get_attribute(server, "start_time"))}) %>%
      set_attribute("start_server", function() {now(server)}) %>%
      log_(function() {
        paste("serving", get_attribute(server, "start_server"))
      }) %>%
      timeout(function() rexp(1, 1/20)) %>%
      release("server") %>%
      log_(function() {paste("served", now(server) - get_attribute(server, "start_server"))}) %>%
      log_("Completed")
    
    priority <-
      trajectory("Customer's path") %>%
      set_attribute("start_time", function() {now(server)}) %>%
      log_(function() {
        paste("Queue is", get_queue_count(server, "server"), "on arrival")
      }) %>%
      seize("server") %>%
      log_(function() {paste("Waited", now(server) - get_attribute(server, "start_time"))}) %>%
      set_attribute("start_server", function() {now(server)}) %>%
      log_(function() {
        paste("serving", get_attribute(server, "start_server"))
      }) %>%
      timeout(function() rexp(1, 1/20)) %>%
      release("server") %>%
      log_(function() {paste("served", now(server) - get_attribute(server, "start_server"))}) %>%
      log_("Completed")
    
    server <-
      simmer("server") %>%
      add_resource("server") %>%
      add_generator("Customer", customer, function() {c(0, rexp(100000, 1/10), -1)}) %>%
      add_generator("Custome with priority", priority, function() {c(0, rexp(100000, 1/10), -1)}, priority= 1)
    server %>% run(500)
    
    resources <- get_mon_resources(server)
    arrivals <- get_mon_arrivals(server)
    
    # p1=plot(resources, metric = "usage", "counter", items="server", step=TRUE)
    
    p2=plot(arrivals, metric = "waiting_time")
    
    grid.arrange(p2)
    
    i = i+1
  }
}

start(10)