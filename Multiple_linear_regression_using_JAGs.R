library(dplyr)
library(rjags)
library(coda)
library(gapminder)
library(ggplot2)


dataset=gapminder
dataset %>% summary()
df_2007 <- dataset %>% filter(year == 2007)

hist(df_2007$gdpPercap)
df_2007 %>% head

cor.test(df_2007$lifeExp,df_2007$gdpPercap)
cor.test(df_2007$lifeExp,df_2007$pop)

plot(df_2007$pop %>% log() ,df_2007$lifeExp)
multi_lm <- lm(
  lifeExp ~ log(gdpPercap) + log(pop),
  data = df_2007
)

summary(multi_lm)


model_string_multi <- "
model {

  for (i in 1:n) {

    lifeExp[i] ~ dnorm(mu[i], tau)

    mu[i] <- beta0 +
             beta1 * x1[i] +
             beta2 * x2[i]
  }

  beta0 ~ dnorm(0, 0.000001)
  beta1 ~ dnorm(0, 0.000001)
  beta2 ~ dnorm(0, 0.000001)

  tau ~ dgamma(5/2.0, 5*10.0/2.0)
  sigma2 <- 1 / tau
  sigma <- sqrt(sigma2)

}
"
jags_data <- list(
  lifeExp = df_2007$lifeExp,
  x1 = log(df_2007$gdpPercap),  
  x2 = log(df_2007$pop),        
  n = nrow(df_2007)
)


init_multi <- function() {
  list(
    beta0 = rnorm(1, 0),
    beta1 = rnorm(1, 0),
    beta2 = rnorm(1, 0),
    tau   = rgamma(1, 1, 1)
  )
}

params_multi <- c("beta0", "beta1", "beta2", "tau")

mod_multi <- jags.model(
  textConnection(model_string_multi),
  data     = jags_data,
  inits    = init_multi,
  n.chains = 3
)

update(mod_multi, 500)

samples_multi <- coda.samples(
  model          = mod_multi,
  variable.names = params_multi,
  n.iter         = 10000
)

summary(samples_multi)
gelman.diag(samples_multi)
effectiveSize(samples_multi)
samples_multi %>% autocorr.plot()
samples_multic <- do.call(rbind,samples_multi)

# thinning samples 
thin <-50 
thin_index <-seq(1,nrow(samples_multic),thin)


sample_multic_thinned <- samples_multic[thin_index,] # a matrix cant apply summary or autocorrelation to this so need to convert it into a sample 

sample_multi_thinned<- as.mcmc(sample_multic_thinned) #sample now 

sample_multi_thinned %>% autocorr.plot()

# residual analysis
x1<- cbind(rep(1.0, jags_data$n),jags_data$x1)
x2<-cbind(rep(1.0,jags_data$n),jags_data$x2)
head(x1)
head(x2)

#posterior mean 

pm_params_multi<-colMeans(sample_multic_thinned)
pm_params_multi

yhat2<- pm_params_multi[1]+pm_params_multi[2]*jags_data$x1 + pm_params_multi[3]*jags_data$x2
resid2 <- jags_data$lifeExp -yhat2
plot(resid2) # features of regression residuals are normally distributed constant variance and no patterns can be seen in the plot of residuals . some of them are below 20 which measn there are some outliers 


# qq plot 
qqnorm(resid2)
qqline(resid2,col="red",lwd=2) # we are not happy with this model as the plot strays away from the line so as a solution to it we add another predictor variable to it 


# model 3 which follows a t distribution instead of a normal distribution

model3_string ="
model {
  
  for (i in 1:n) {
    
    lifeExp[i] ~ dt(mu[i], tau, df)
    
    mu[i] <- beta0 +
      beta1 * x1[i] +
      beta2 * x2[i]
  }
  
  for (i in 1:3) {
  beta[i]~ dnorm (0.0 , 1.0/1.0e6)
  }
  
  df= nu + 2.0 # we want degrees of freedom to be greater than 2 to guearantee the existence of mean and variance 
  nu ~dexp(1.0)
  
  
  
  tau ~ dgamma(5/2.0, 5*10.0/2.0)
  sigma2 <- 1 / tau
  sigma <- sqrt(1.0/tau *df/(df-2.0))
  
}
"
dic.samples(mod_multi,1000)
