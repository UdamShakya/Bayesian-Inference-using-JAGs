library(rjags)
library(coda)
library(dplyr)
library(future)


#first we define the likelihood and prior
#Y|theta ~ Bin(n,theta)
#theta~ Beta(1,1)

model_str<-"model{
  y~dbin(theta,n)
  theta ~ dbeta(1,1)
  
}"

#specify the data for JAGS
dataList <- list(y=12,n=20)

#specify the paramters fro monitor

params<-c("theta")


#give initial values

inits<-function(){
  list(theta=runif(1))
}


#compile the model

set.seed(123)

availableCores()

j_model<-jags.model(
  textConnection(model_str),
  data = dataList,
  inits = inits,
  n.chains = 3
)


#run the burn-in period

update(j_model,1000)


#generate posteriror samples

samples<-coda.samples(
  model=j_model,
  variable.names = params,
  n.iter = 5000
)

post<-do.call(rbind,samples)




plot(samples)

#plot(post)


#checkin independacy of samples, if much more correlated samples are there...do thinning
#based on the ACF plot we can define the thin value
#or we can take obs from multiple chains(3 cahins)

autocorr.plot(samples)


#now thinning

nrow(post)
thinned_sample<-post[seq(1,15000,by=10)]

thinned_sample<-as.mcmc(thinned_sample)

plot(thinned_sample)

autocorr.plot(thinned_sample)



#calculating independent samples

samples[1] %>% effectiveSize()
samples[2] %>% effectiveSize()
samples[3] %>% effectiveSize()



thinned_sample %>% effectiveSize()


#gelman d statistic


samples %>% gelman.diag()


#posteriro inference

thinned_sample %>% mean()
thinned_sample %>% median()
thinned_sample %>% sd()




