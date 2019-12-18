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
  timeout(12) %>%
  release("counter") %>%
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
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)}) %>%
  add_generator("Custome with priority", priority, function() {c(0, rexp(4, 1/10), -1)}, priority = 1)

bank %>% run(until = 1000)