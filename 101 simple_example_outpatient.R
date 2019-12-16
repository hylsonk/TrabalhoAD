library(simmer)
library(simmer.plot)

# create environment

env <- simmer("outpatient clinic")

env

## add resources

env %>%
  add_resource("nurse",3)%>%
  add_resource("doctor",4)%>%
  add_resource("administration",2)
  
env

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

## create patient generator
??add_generator
env %>%
  add_generator(name_prefix = "sim_patient_" , 
                trajectory = patient,
                distribution = function() rnorm(1,5,0.5))

env

env %>% run(until = 600)


## plot the model

plot(patient)
plot(patient, verbose = T)

## look under the hood

myarrivals = get_mon_arrivals(env)
env
119-74
45 - (38 + 2 + 3 + 1)

myarrivals_per_resource = get_mon_arrivals(env, per_resource = T)

myresources = get_mon_resources(env)

