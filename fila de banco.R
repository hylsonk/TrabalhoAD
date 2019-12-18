library(simmer)

set.seed(1933)

bank <- simmer()

customer <-
  trajectory("Customer's path") %>%
  set_attribute("start_time", function() {now(bank)}) %>%
  log_(function() {
    paste("Queue is", get_queue_count(bank, "counter"), "on arrival")
  }) %>%
  seize("counter") %>%
  log_(function() {paste("Waited", now(bank) - get_attribute(bank, "start_time"))}) %>%
  set_attribute("start_server", function() {now(bank)}) %>%
  log_(function() {
    paste("serving", get_attribute(bank, "start_server"))
  }) %>%
  timeout(function() rexp(1, 1/20)) %>%
  release("counter") %>%
  log_(function() {paste("served", now(bank) - get_attribute(bank, "start_server"))}) %>%
  log_("Completed")

priority <-
  trajectory("Customer's path") %>%
  set_attribute("start_time", function() {now(bank)}) %>%
  log_(function() {
    paste("Queue is", get_queue_count(bank, "counter"), "on arrival")
  }) %>%
  seize("counter") %>%
  log_(function() {paste("Waited", now(bank) - get_attribute(bank, "start_time"))}) %>%
  timeout(12) %>%
  release("counter") %>%
  log_("Completed")

bank <-
  simmer("bank") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(10, 1/10), -1)}) %>%
  add_generator("Custome with priority", priority, function() {c(0, rexp(10, 1/10), -1)}, priority = 1)

bank %>% run(until = 900)

resources <- get_mon_resources(bank)
arrivals <- get_mon_arrivals(bank)

p1=plot(resources, metric = "usage", "counter", items="server", step = TRUE)

p2=plot(arrivals, metric = "flow_time",
        items = c( "queue", "server"))
library(gridExtra)
grid.arrange(p1, p2)