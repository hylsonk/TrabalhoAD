library(simmer)
library(simmer.plot)


# create patient trajectory

patient <- trajectory(name = "Patient Path", verbose = T)

patient

## draw model

patient %>% 
  
  seize("nurse",1) %>% ## need to define resources 
  timeout(function() rnorm(1,15)) %>% 
  release("nurse",1) %>%
  
  seize("doctor",1) %>% ## need to define resources 
  timeout(function() rnorm(1,20)) %>% 
  release("doctor",1) %>%
  
  seize("administration",1) %>% ## need to define resources 
  timeout(function() rnorm(1,5)) %>% 
  release("administration",1) 


time1 = Sys.time()


envs <- lapply(1:100, function(i) {
  simmer("outpatient clinic") %>%
    add_resource("nurse",3)%>%
    add_resource("doctor",4)%>%
    add_resource("administration",2) %>%
    add_generator(name = "sim_patient_" , 
                  trajectory = patient,
                  distribution = function() rnorm(1,5,0.5)) %>%
    run(100) %>%
    wrap()
})

time2 = Sys.time()

time2-time1


resources <- get_mon_resources(envs)
p1=plot(resources, metric = "utilization")


p2=plot(resources, metric = "usage", c("nurse", "doctor","administration"),
     items = c( "queue", "server"))

arrivals <- get_mon_arrivals(envs)


p3=plot(arrivals, metric = "waiting_time")
p4=plot(arrivals, metric = "activity_time")

library(gridExtra)
grid.arrange(p1,p2,p3,p4)

