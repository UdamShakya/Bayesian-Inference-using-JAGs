library(dplyr)
library(rjags)
library(coda)
library(gapminder)
library(ggplot2)

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
 dic.samples(mod,1000)
samples_multi<-coda.samples(
  model=mod,
  variable.names = params,
  n.iter = 50000
)

sample_csample <- do.call(rbind,samples_multi)
sample_csample

plot(samples_multi)

samples_multi %>% gelman.diag()

autocorr.plot(samples_multi)
effectiveSize(samples_multi) # out of the 30000 samples mcmc samples are not independent , so this represents the amount of independent samples which provides information on the variables

summary(samples_multi) # here what does naive SE and time series SE we Calculate the mean and variance through monte carlo integration . the monte carlo error is the naive Se and after removing the autocorrelation the monte carlo error is the time series SE.since after doing the the number of real independent samples number decreases therefore the time series Se is greater than the Naive Se


#thinning to get rid of autocorrelation of the variables 

thin <- 100

thin_ind <- seq(1,nrow(sample_csample),thin) # thinning sequence 

sample_csample_thinned <- sample_csample[thin_ind,] # a matrix cant apply summary or autocorrelation to this so need to convert it into a sample 

sample_thinned<- as.mcmc(sample_csample_thinned) #sample now 

summary(sample_thinned)
sample_thinned %>% autocorr.plot()
# run the simulation for more number of iterations and keep the thinning number high
sample_thinned %>% effectiveSize()

#residual analysis - difference between the predicted value and the observed value


x<- cbind(rep(1.0, jags_data$n),jags_data$x)
head(x)

#posterior mean 

pm_params1<-colMeans(sample_csample_thinned)
pm_params1

yhat1<- pm_params1[1]+pm_params1[2]*jags_data$x
resid1 <- jags_data$lifeExp -yhat1
plot(resid1) # features of regression residuals are normally distributed constant variance and no patterns can be seen in the plot of residuals . some of them are below 20 which measn there are some outliers 


# qq plot 
qqnorm(resid1)
qqline(resid1,col="red",lwd=2) # we are not happy with this model as the plot strays away from the line so as a solution to it we add another predictor variable to it 
