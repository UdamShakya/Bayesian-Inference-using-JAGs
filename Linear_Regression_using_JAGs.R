library(dplyr)
library(rjags)
library(coda)
library(gapminder)

installed.packages("coda")

dataset = gapminder
summary(dataset)

df_2007 <- dataset %>% filter(year == 2007)

hist(df_2007$gdpPercap)
df_2007 %>% head

df_2007 %>% glimpse()
cor.test(df_2007$lifeExp,df_2007$gdpPercap)
cor.test(df_2007$lifeExp,df_2007$pop)

linear_model<- lm(lifeExp ~log(gdpPercap), data= df_2007)
linear_model %>% summary()

plot(df_2007$gdpPercap %>% log(),df_2007$lifeExp)


model_string <- "
model{

for (i in 1:n){

    lifeExp[i] ~dnorm(mu[i],tau)
    
    mu[i]<- beta0 + 
            beta1*x[i]
  }

  beta0~dnorm(0,0.000001)
  beta1~dnorm(0,0.000001)

  tau ~ dgamma(5/2.0, 5*10.0/2.0) # prior sample size= 5, prior guesses for variance =10
  sigma2 <- 1/tau
  sigma <- sqrt(sigma2)

}
"

# very non informative priors for beta0 and beta1 since variance is very low and for gamma prior alpha= n0/2 , b0=(n0*s0^2)/2,
#based on prior sample size and variances 


jags_data<- list(
  lifeExp = df_2007$lifeExp,
  x =  log(df_2007$gdpPercap),
  n = nrow(df_2007)
)

init <-function(){
  list(
    beta0=rnorm(1,0),
    beta1=rnorm(1,0),
    tau=rgamma(1,1,1)
  )
  }

params <- c("beta0","beta1","tau")

mod <- jags.model(
  textConnection(model_string),
  data = jags_data,
  inits = init,
  n.chains= 3
)

update(mod,500)

samples_multi<-coda.samples(
  model=mod,
  variable.names = params,
  n.iter = 10000
)
plot(samples_multi)

samples_multi %>% gelman.diag()

autocorr.plot(samples_multi)
effectiveSize(samples_multi)



