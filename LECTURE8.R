
############################################################
####                                                    ####  
####  BIOL4558, Topico 8                               ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                    ####
####        &                           ####
####  Raymond L. Tremblay       ####
####  Universidad de Puerto Rico                   ####
####                                                    #### 
############################################################


############################################################
####  Stochasticity and Uncertainty                     ####
############################################################



#############
# Generación de números aleatorios!


#####
# define an arbitrary distribution

box <- c(rep(1,10),rep(2,5),rep(3,2))  # define what's in the lottery ball machine (10 "1" balls, 5 "2" balls and 2 "3" balls)

box
barplot(table(box)/sum(table(box)),ylab="probability",xlab="possibility")  # visualize the distribution of possibilities


################
# Distribuciones discretas
################


#######
# Ejemplo: distribución binomial (distribución de lanzamiento de moneda)


# graficar la distribución binomial!
xvals <- seq(0,10,1)

probs <- dbinom(xvals,10,prob=0.3)
#Cual es la suma de la probabilidades?

names(probs) <- xvals
               
barplot(probs,ylab="Probabilidades",xlab="Posibilidades",main="Distribución binomial (discreta)")



library(tidyverse)
caras=c(4,7, 6, 8, 5, 4,3,7,7,2,
        9, 3, 5,4, 5, 4, 4,2,6,5)
caras=as_tibble(caras)
caras

ggplot(caras, aes(value))+
  geom_histogram(aes(y=..density..))

#########
# Distribución de Poisson

xvals <- seq(0,10,1)
probs <- dpois(xvals,lambda=2.2)     # Distribución Poisson El lambda es una función que determina las probabilidades
names(probs) <- xvals
               
barplot(probs,ylab="Probabilidades",xlab="Possibilidades",main="Distribución Poisson (discreta)")



################
# DISTRIBUCIONES CONTINUAS
#################

##########
# Distribución uniforme

lower = 0
upper = 10

curve(dunif(x,lower,upper),0,10,ylab="Probabilidad (densidad)",xlab="Possibilidades",main="Distribución uniforme (continuous)",ylim=c(0,1))   # Distribución uniforme


#########
# Distribución normal

mean = 7.1
stdev = 1.9

curve(dnorm(x,mean,stdev),0,15,ylab="Probabilidades (densidad)",xlab="Possibilidades",main="Distribución normal (continua)")  


#########
# Distribución normal

x=rnorm(10000, 0,1 )
x=as.tibble(x)
#x

ggplot(x, aes(value))+
  geom_density()


############
# Log-normal distribution

meanlog = 1.4
stdevlog = 0.6

curve(dlnorm(x,meanlog,stdevlog),0,15,ylab="Probability (density)",xlab="Possibilities",main="Lognormal distribution (continuous)")   # probability density


##########
# Beta distribution

shape1 = 10
shape2 = 4

curve(dbeta(x,shape1,shape2),0,1,ylab="Probability (density)",xlab="Possibilities",main="Beta distribution (continuous)")   # probability density


################
# Random number generation!


### Binomial random number generator
rbinom(1,size=10,prob=0.5)    # note: "size" is the number of coin flips, and "prob" is the probability of coming up 'heads'

### Poisson random number generator
rpois(1,lambda=4.1)     # note: "lambda" represents the mean (and variance!) of the Poisson distribution

### Uniform random number generator
runif(1,min=1,max=3.5)   # "min" and "max" are pretty obvious!

### Normal random number generator
rnorm(1,mean=3,sd=4.1)   # normal distribution is defined by "mean" and "sd" (standard deviation).

### lognormal random number generator (like normal distribution, but can not go below zero)
rlnorm(1,meanlog=0.5,sdlog=0.2)    # lognormal distribution is defined by "meanlog", the mean on the log scale and "sdlog" (standard deviation on the log scale).

### beta random number generator (bounded between 0 and 1- just like survival rate!)
rbeta(1,shape1=10,shape2=3)  # beta distribution is defined by "shape1" and "shape2", which together define the mean and spread within the range from 0 to 1.

## Para información sobr ela distribución beta vea los sigyuientes enlaces

# 1. https://keisan.casio.com/exec/system/1180573226
# 2. https://stats.stackexchange.com/questions/376634/how-to-pick-starting-parameters-for-massfitdist-with-the-beta-distribution





########
# muestra de una distribución arbitraria

distribution <- c(5,3,5,4,3,6,4,5,5,1,6,5,4,3,6,6,4,2,8,4,4,5,2)     # make up a set of possibilities
hist(distribution,freq = F, ylab="Probability",xlab="Possibilities")  # visualize distribution
sample(distribution,1)  # take one random sample from this distribution!


#############
# Demostración: ¡use datos para determinar una distribución!
#############


#############
# Made-up canvasback data- average number of eggs hatched per female for 20 years

hatch_perfem <- c(3.05, 1.45, 0.99, 3.24, 1.49, 1.70, 1.66, 2.32, 0.83, 2.41,
2.33, 1.68, 1.43, 2.74, 2.05, 3.13, 1.90, 3.69, 1.55, 2.79)

hist(hatch_perfem)


############
# Trate de identificar una distribución logarítmica normal de números aleatorios para representar la 'canvasback duck' data

## first, plot a histogram of the data from the 20-year study
hist(hatch_perfem,freq=F,main="Histogram of avg number hatched per female",xlab="possibilities",ylab="probability",xlim=c(0,10),ylim=c(0,1))

## ahora, superponga una distribución de probabilidad logarítmica normal con parámetros arbitrarios (meanlog y sdlog). Esto es sólo un punto de partida.

curve(dlnorm(x,meanlog=1.5,sdlog=0.39),col="green",lty=2,lwd=2,add=T)

curve(dlnorm(x,meanlog=1.8,sdlog=0.39),col="green",lty=2,lwd=2,add=T) 

curve(dlnorm(x,meanlog=2.0,sdlog=0.39),col="red",lty=2,lwd=2,add=T) # try a different value...

#### ¡Siga cambiando el valor de 'meanlog' hasta que encuentre los mejores parámetros que se ajusten a los datos!

#### Una vez que encuentre los parámetros que mejor se ajusten, genere 5 números aleatorios a partir de esta distribución usando la función "rlnorm ()" en R. 

rlnorm(5,meanlog=1.5,sdlog=0.39)    # ¡por ejemplo! (¡recuerde cambiar el parámetro "meanlog" por el valor que identificó anteriormente!)


hatch_5_years=hatch_perfem/5
fitdistr(hatch_5_years,"log-normal")
data=rlnorm(1000, -0.92822498, 0.38756090)
exp(data)  # USe exp to convert he values from lognotmal to numbers again.  


#c=log(2)
#exp(c)
beta_mom <- function(x) {

  m_x <- mean(x, na.rm = TRUE)
  s_x <- sd(x, na.rm = TRUE)

  alpha <- m_x*((m_x*(1 - m_x)/s_x^2) - 1)
  beta <- (1 - m_x)*((m_x*(1 - m_x)/s_x^2) - 1)

  return(list(alpha = alpha, beta = beta))

}

Larva_supevivencia=c(0.184, 0.113, 0.355, 0.344, 0.295, 0.226, 0.207, 0.178, 0.171, 0.257)
beta_mom(Larva_supevivencia)

library(fitdistrplus)
fit=fitdist(Larva_supevivencia,"beta")
fit
plot(fit, las=1)
