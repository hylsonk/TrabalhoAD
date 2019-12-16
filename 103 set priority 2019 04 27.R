## same priority

library(simmer)
library(simmer.plot)



envs <- lapply(1:10, function(i) { 
  
  
  env <- simmer("hospital")
  
  patient <- trajectory("patients' path") %>%
    
    
    branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.5,0.5)), 
           continue=c(T,T), 
           trajectory("A") %>%
             
             
             # static / dynamic values
             
             ## static
  # c(1st value = priority, preemptible level must be larger, restart=T/F)
             set_prioritization(values = c(3,7,T)) %>%
         
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
           
        ## give label no. 1 to patients passed through this path
             set_attribute("Arm",1)  %>%
           log_(message = "Priority A Patient finished Arm 1 !" )
           
           ,
           trajectory("B") %>%
             
             
             # static values
    
             set_prioritization(values = c(3,7,T)) %>%
              
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             set_attribute("Arm",2) %>%## give label no. 2 to patients passed through this path
  log_(message = "Priority B Patient finished Arm 2!" )
           
    )
  
  
  
  
  
  
  env %>%

        add_resource("doctor", 2) %>%

        add_generator("patient", patient, function() 5, mon=2)
  
  
  env %>% 
    run(until=600)
  
})


#arriv<-get_mon_arrivals(envs)
#resourc<-get_mon_resources(envs)
attr<-get_mon_attributes(envs)
#arr_per_res<-get_mon_arrivals(envs, T)


table(attr$value)
table(attr$replication, attr$value)


## Diffrent priority B has more priority

library(simmer)
library(simmer.plot)



envs <- lapply(1:10, function(i) { 
  
  
  env <- simmer("hospital")
  
  patient <- trajectory("patients' path") %>%
    
    
    branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.5,0.5)), 
           continue=c(T,T), 
           trajectory("A") %>%
             
             
             
             set_prioritization(values = c(3,7,T)) %>%
             
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             ## give label no. 1 to patients passed through this path
             set_attribute("Arm",1)  %>%
             log_(message = "Priority A Patient finished Arm 1 !" )
           
           ,
           trajectory("B") %>%
             
             
           
             set_prioritization(values = c(5,7,T)) %>%
             
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             set_attribute("Arm",2) %>%## give label no. 2 to patients passed through this path
             log_(message = "Priority B Patient finished Arm 2!" )
           
    )
  
  
  
  
  
  
  env %>%
    
    add_resource("doctor", 2) %>%
    
    add_generator("patient", patient, function() 5, mon=2)
  
  
  env %>% 
    run(until=600)
  
})


#arriv<-get_mon_arrivals(envs)
#resourc<-get_mon_resources(envs)
attr_d<-get_mon_attributes(envs)
#arr_per_res<-get_mon_arrivals(envs, T)


table(attr_d$value)
table(attr_d$replication, attr_d$value)
