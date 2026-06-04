library(rjags)
library(coda)
library(dplyr)
library(future)

set.seed(123)

y<-rnorm(
  n=50,
  mean=10,
  sd=2
)

y %>% summary()



model_str_multi<-"model{
  for(i in 1:N){
    y[i]~dnorm(mu,tau)
  }
  mu~dnorm(0,1)
  
  tau~dgamma(1,0.1)
  
  sigma<-sqrt(1/tau)
  
}"



datalist_multi <-list(
  y=y,
  N=length(y)
)


init_mult<-list(
  mu = rnorm(1,0,1),
  tau = rgamma(1,1,1)
)


params_multi<-c("mu","sigma","tau")



set.seed(123)

j_model_multi<-jags.model(
  textConnection(model_str_multi),
  data = datalist_multi,
  inits = init_mult,
  n.chains = 3
)


update(j_model_multi,1000)

samples_multi<-coda.samples(
  model=j_model_multi,
  variable.names = params_multi,
  n.iter = 5000
)
plot(samples_multi)

summary(samples_multi)

