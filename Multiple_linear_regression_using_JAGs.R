library(dplyr)
library(rjags)
library(coda)
library(gapminder)


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
