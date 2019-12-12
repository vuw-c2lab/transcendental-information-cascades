

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')
if (!require(tuneR)) install.packages('tuneR')

setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")

# plot combined data ------

combined_data <- rbind(wien_primes[,c(1,4,5)],wien_primes_b4[,c(1,4,5)],wien_primes_b8[,c(1,4,5)],wien_primes_b16[,c(1,4,5)],wien_primes_b32[,c(1,4,5)],wien_primes_b36[,c(1,4,5)])
ggplot(data=combined_data,
       aes(x=index, y=ShannonWiener, colour=run)) +
  geom_line() + theme_apa() + scale_x_continuous(trans='log10')

combined_data <- rbind(wien_primes[,c(2,4,5)],wien_primes_b4[,c(2,4,5)],wien_primes_b8[,c(2,4,5)],wien_primes_b16[,c(2,4,5)],wien_primes_b32[,c(2,4,5)],wien_primes_b36[,c(2,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=Pielou, colour=run)) +
  geom_line() + theme_apa() + scale_x_continuous(trans='log10')

combined_data <- rbind(wien_primes[,c(6,4,5)],wien_primes_b4[,c(6,4,5)],wien_primes_b8[,c(6,4,5)],wien_primes_b16[,c(6,4,5)],wien_primes_b32[,c(6,4,5)],wien_primes_b36[,c(6,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=csumP, colour=run)) +
  geom_line() + theme_apa(legend.pos ="none") + scale_x_continuous(trans='log10')

combined_data <- rbind(wien_primes[,c(7,4,5)],wien_primes_b4[,c(7,4,5)],wien_primes_b8[,c(7,4,5)],wien_primes_b16[,c(7,4,5)],wien_primes_b32[,c(7,4,5)],wien_primes_b36[,c(7,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=csumS, colour=run)) +
  geom_line() + theme_apa(legend.pos ="none") + scale_x_continuous(trans='log10')


combined_data <- rbind(wien_primes_random[,c(1,4,5)],wien_prandom_numbers[,c(1,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=ShannonWiener, colour=run)) +
  geom_line() + theme_apa() + scale_x_continuous(trans='log10')

combined_data <- rbind(wien_primes_random[,c(2,4,5)],wien_prandom_numbers[,c(2,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=Pielou, colour=run)) +
  geom_line() + theme_apa() + scale_x_continuous(trans='log10')

combined_data <- rbind(wien_primes_random[,c(6,4,5)],wien_prandom_numbers[,c(6,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=csumP, colour=run)) +
  geom_line() + theme_apa(legend.pos ="none") + scale_x_continuous(trans='log10')

combined_data <- rbind(wien_primes_random[,c(7,4,5)],wien_prandom_numbers[,c(7,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=csumS, colour=run)) +
  geom_line() + theme_apa(legend.pos ="none") + scale_x_continuous(trans='log10')






# compare to the prime number theorem -------
# 
primes50k <- read_delim("data/primes50k.txt", 
                        ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
np <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/prime_counts.txt")
np<-as.vector(np$np)

nmax<-max(primes50k$X2)
primelist <- primes50k$X2

# intgnd <- function(x) {1/log(x)}        # logarithmic integral
# np <- c(0)                              # number of primes up to n
# gp <- rep(0,9)                          # Gauss' early estimate
# li <- c(NULL)                           # Gauss' later estimate
# 
# for (n in 1:nmax) {
#   if(n > 1) {
#     i <- which(primelist[1:length(primelist)] <= n)
#     np <- append(np,max(i))
#     li <- append(li,integrate(intgnd,2,n)$value)
#   }
#   if(n >= 10) gp <- append(gp, n/log(n))
# }

# plot(np,type="s",lwd=2,xlab="Number (n)",ylab="Count of primes < n")
# lines(10:nmax,gp[10:nmax],col="red")
# lines(li,col="blue")


eq = function(x,y){(y)  / (x/(log(x)))}
p_count <- eq(x=c(1:max(primes50k$X2)),y=np)
plot(p_count,type='l',log='x')

eq2 = function(x,y){
  (y) / (integrate(function(t) 1/log(t),lower=2,upper=x, rel.tol=1e-5)$value)
}

p_count2 <- c()
for(t in 2:max(primes50k$X2)){
  p_count2 <- c(p_count2,eq2(x=t,y=np[t]))
}
plot(p_count2,type='l',log='x')

plot_data <- as.data.frame(p_count)
plot_data$idx <- c(1:nrow(plot_data))


ggplot(data=plot_data,
       aes(x=idx, y=p_count)) +
  geom_line() + theme_apa() + scale_x_continuous(trans='log10')

plot_data <- as.data.frame(p_count2)
plot_data$idx <- c(1:nrow(plot_data))


ggplot(data=plot_data,
       aes(x=idx, y=p_count2)) +
  geom_line() + theme_apa() + scale_x_continuous(trans='log10')

prime_approx <- data.frame(pc1=p_count,pc2=c(Inf,p_count2))
prime_approx.m <- reshape2::melt(prime_approx)
ggplot(data=prime_approx.m,
       aes(x=c(1:nrow(prime_approx),1:nrow(prime_approx)), y=value, group=variable)) +
  geom_line(linetype=c(rep("dashed",nrow(prime_approx)),rep("dotted",nrow(prime_approx)))) + 
  theme_apa() + scale_x_continuous(trans='log10') + 
  #geom_text(x=10, y=1.5, label=paste0(expression(pi(x)))) + geom_text(x=10, y=0.5, label=paste0(expression(pi(x))))
  NULL

plot_data$ShWiener <- 0
plot_data$Pielou <- 1

plot_data$ShWiener4 <- 0
plot_data$Pielou4<- 1

plot_data$ShWiener8 <- 0
plot_data$Pielou8 <- 1

plot_data$ShWiener16 <- 0
plot_data$Pielou16 <- 1

plot_data$ShWiener32 <- 0
plot_data$Pielou32 <- 1

plot_data$ShWiener36 <- 0
plot_data$Pielou36 <- 1

plot_data$ShWienerTailRem1 <- 0
plot_data$PielouTailRem1 <- 1

plot_data$np <- c(0,diff(np))
plot_data$np_orig <- np


ShWienerIdx <- 0
for (i in 1:nrow(plot_data)){
  if(plot_data$np[i]>0){
    ShWienerIdx <- ShWienerIdx + 1
  }
  if(ShWienerIdx>0){
    plot_data$ShWiener[i] <- wien_primes$ShannonWiener[ShWienerIdx]
    plot_data$Pielou[i] <- wien_primes$Pielou[ShWienerIdx]
    
    plot_data$ShWiener4[i] <- wien_primes_b4$ShannonWiener[ShWienerIdx]
    plot_data$Pielou4[i] <- wien_primes_b4$Pielou[ShWienerIdx]
    
    plot_data$ShWiener8[i] <- wien_primes_b8$ShannonWiener[ShWienerIdx]
    plot_data$Pielou8[i] <- wien_primes_b8$Pielou[ShWienerIdx]
    
    plot_data$ShWiener16[i] <- wien_primes_b16$ShannonWiener[ShWienerIdx]
    plot_data$Pielou16[i] <- wien_primes_b16$Pielou[ShWienerIdx]
    
    plot_data$ShWiener32[i] <- wien_primes_b32$ShannonWiener[ShWienerIdx]
    plot_data$Pielou32[i] <- wien_primes_b32$Pielou[ShWienerIdx]
    
    plot_data$ShWiener36[i] <- wien_primes_b36$ShannonWiener[ShWienerIdx]
    plot_data$Pielou36[i] <- wien_primes_b36$Pielou[ShWienerIdx]
    
    plot_data$ShWienerTailRem1[i] <- wien_primes_tailRem1$ShannonWiener[ShWienerIdx]
    plot_data$PielouTailRem1[i] <- wien_primes_tailRem1$Pielou[ShWienerIdx]
  } else {
    
  }
}

prime_approx <- data.frame(pc1=p_count,pc2=c(Inf,p_count2),eve=plot_data$Pielou,eve4=plot_data$Pielou4,eve8=plot_data$Pielou8,eve16=plot_data$Pielou16,eve32=plot_data$Pielou32,eve36=plot_data$Pielou36)
prime_approx.m <- reshape2::melt(prime_approx)
ggplot(data=prime_approx.m,
       aes(x=c(1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx)
               ), y=value, group=variable)) +
  geom_line(linetype=c(rep("solid",nrow(prime_approx)),
                       rep("dashed",nrow(prime_approx)),
                       rep("dotted",nrow(prime_approx)),
                       rep("twodash",nrow(prime_approx)),
                       rep("longdash",nrow(prime_approx)),
                       rep("F1",nrow(prime_approx)),
                       rep("1F",nrow(prime_approx)),
                       rep("12345678",nrow(prime_approx))
                       )) + 
  theme_apa() + scale_x_continuous(trans='log10')

prime_approx <- data.frame(pc1=p_count,pc2=c(Inf,p_count2),eve=plot_data$ShWiener,eve4=plot_data$ShWiener4,eve8=plot_data$ShWiener8,eve16=plot_data$ShWiener16,eve32=plot_data$ShWiener32,eve36=plot_data$ShWiener36)
prime_approx.m <- reshape2::melt(prime_approx)
ggplot(data=prime_approx.m,
       aes(x=c(1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx)
       ), y=value, group=variable)) +
  geom_line(linetype=c(rep("solid",nrow(prime_approx)),
                       rep("dashed",nrow(prime_approx)),
                       rep("dotted",nrow(prime_approx)),
                       rep("twodash",nrow(prime_approx)),
                       rep("longdash",nrow(prime_approx)),
                       rep("F1",nrow(prime_approx)),
                       rep("1F",nrow(prime_approx)),
                       rep("12345678",nrow(prime_approx))
  )) + 
  theme_apa() + scale_x_continuous(trans='log10')
  #geom_text(x=10, y=1.5, label=paste0(expression(pi(x)))) + geom_text(x=10, y=0.5, label=paste0(expression(pi(x))))





# test granger causality ------
library(lmtest)
set.seed(20160227)
grangertest(eve ~ pc1, order = 1, data = prime_approx)




# pertubation analysis ------

A <- as.matrix(readr::read_csv("../../output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/TICmatrix.csv"))
colnames(A) <- c(1:ncol(A))
rownames(A) <- c(1:ncol(A))

B <- as.matrix(readr::read_csv("../../output/2019-11-15-letter-to-n-study/primes50k_tailRemove1-discrete-tokenised-2019-12-11-21-27-11/createTIC/TICmatrix.csv"))
colnames(B) <- c(1:ncol(B))
rownames(B) <- c(1:ncol(B))



### characteristic polynomial and eigenvalue study
library(polynom)
library(pracma)
options(scipen=1000)

#A <- matrix(c(1,1,0,0,1,0,0,0,1), 3, 3, byrow=TRUE)

matA <- A[1:5000,1:5000]
eValsA <- eigen(matA,symmetric = T)$values

matB <- B[1:5000,1:5000]
eValsB <- eigen(matB,symmetric = T)$values
#eigenvalue difference
plot(eValsA - eValsB,type='l')
eDiffSum <- sum(abs(eValsA - eValsB))
plot(ecdf(eValsA - eValsB))

plot(eVals)
plot(eVals,log='x')
plot(diff(eVals),type='l')
plot(ecdf(diff(eVals)))
#A <- matrix(c(1,1,0,0,1,0,0,0,1), 3, 3, byrow=TRUE)
#pol <- charpoly(mat, info = TRUE)
#polyA <- polynomial(rev(pol$cp))
#plot(polyA,xlim = c(-1000,1000))

specCount <- plyr::count(coord_primes$specificity)
plot(specCount,type='l')
plot(specCount,type='l',log='xy')


# CHAOS TESTING -----------------------------------------------------------


n <- 1
R <- seq(2.5,4,length=1000)
f <- expression(a*x*(1-x))
data <- matrix(0,200,1001)

for(a in R){
  x <- runif(1) # random initial condition
  ## first converge to attractor
  for(i in 1:200){
    x <- eval(f)
  } # collect points on attractor
  for(i in 1:200){
    x <- eval(f)
    data[i,n] <- x
  }
  n <- n+1
}

data <- data[,1:1000]
plot(R,data[1,], pch=".", xlab="a", ylab="X")
for(i in 2:200) points(R,data[i,],pch=".")

### chaos visual example 2 (cobweb)

f <- function(x,r) r*x*(1-x)
#f <- as.function(polyA)
#f <- function(x,r) r*x^10
bounce <- function(f,init=4,n=10,cobweb=T,timeseries=F,dom=NULL,...){
  iterates <- NULL
  x0 <- init
  for(t in 1:n){
    x1<- f(x0,...)
    iterates[t] <- x1
    x0 <- x1
  }
  
  if(cobweb & !timeseries){
    if(is.null(dom)){
      a <- range(c(init,iterates))[1]-0.5
      b <- range(c(init,iterates))[2]+0.5} else
      {a <- dom[1];b <- dom[2]}
    curve(f(x,...),a,b);grid(col=1);abline(0,1,lty=2)
    lines(c(init,iterates), f(c(init,iterates),...),t="s")
    points(c(init,iterates), f(c(init,iterates),...))
  }
  
  if(timeseries){
    plot(0:n,c(init,iterates),t="l")
  }
  
}
bounce(f,0.1,r=3.7,n=100)

bounce(f,0.1,r=3.8282,n=100,timeseries=T)

#bounce(f,0.1,n=100)

#bounce(f,0.1,n=100,timeseries=T)




# similarity with dtw Dynamic Time Warping (DTW) ---------
library(dtw)

#query<-tail(prime_approx$eve,100000)
#template<-tail(prime_approx$pc1,100000)
#alignment<-dtw(query,template,keep=TRUE)
#plot(alignment,type="threeway")


# MODEL fitting -------
library(splines)
library(caret)

x <- 1:nrow(prime_approx)
prime_approx$x <- x
y <- prime_approx$eve

#fit linear model
model <- lm(eve ~ x, data = prime_approx)

# Make predictions
predictions <- model %>% predict(prime_approx)
# Model performance
data.frame(
  RMSE = RMSE(predictions, prime_approx$eve),
  R2 = R2(predictions, prime_approx$eve)
)

ggplot(prime_approx, aes(x, eve) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

#polynomial
# Build the model
model <- lm(eve ~ poly(x, 5, raw = TRUE), data = prime_approx)
# Make predictions
predictions <- model %>% predict(prime_approx)
# Model performance
data.frame(
  RMSE = RMSE(predictions, prime_approx$eve),
  R2 = R2(predictions, prime_approx$eve)
)

ggplot(prime_approx, aes(x, eve) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))


#log transform
# Build the model
model <- lm(eve ~ log(x), data = prime_approx)
# Make predictions
predictions <- model %>% predict(prime_approx)
# Model performance
data.frame(
  RMSE = RMSE(predictions, prime_approx$eve),
  R2 = R2(predictions, prime_approx$eve)
)

ggplot(prime_approx, aes(x, eve) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x))

#splines
# Build the model
knots <- quantile(prime_approx$x, p = c(0.25, 0.5, 0.75))
model <- lm (eve ~ bs(x, knots = knots), data = prime_approx)
# Make predictions
predictions <- model %>% predict(prime_approx)
# Model performance
data.frame(
  RMSE = RMSE(predictions, prime_approx$eve),
  R2 = R2(predictions, prime_approx$eve)
)
ggplot(prime_approx, aes(x, eve) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))


#GAM
library(mgcv)
# Build the model
model <- gam(eve ~ s(x), data = prime_approx)
# Make predictions
predictions <- model %>% predict(prime_approx)
# Model performance
data.frame(
  RMSE = RMSE(predictions, prime_approx$eve),
  R2 = R2(predictions, prime_approx$eve)
)

ggplot(prime_approx, aes(x, eve) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))


#arima
library(forecast)
ar1 <- Arima(prime_approx$eve, order = c(1,0,0))
ar2 <- Arima(prime_approx$eve, order = c(3,0,0))
ar3 <- Arima(prime_approx$eve, order = c(1,0,1))
ar4 <- Arima(prime_approx$eve, order = c(1,1,1))

fittedData <- data.frame(x=x,y=fitted(ar3))

plot(fitted(ar4),type='l')
ggplot(prime_approx, aes(x, eve) ) +
  geom_point() +
  geom_line(color='blue', data = fittedData, aes(x=x,y=y))
#stat_smooth(method = gam, formula = y ~ s(x))

#### more
# fit non-linear model
m <- nls(y ~ a*x/(b+x))

p_count2.b <- c(100000,p_count2)
p_count2.b[2] <- 100000
cor(p_count2.b,predict(m))
plot(x,y)
lines(x,predict(m),lty=2,col="red",lwd=3)

library(drc)
plot(y ~ x, type='l', log='x')
#g <- rep(1:2, c(21, 20))
#fit1 <- drm(zd ~ rd, fct = LL2.5(), subset = g == 1)
fit1 <- drm(y ~ x, fct = LL2.5())
#fit1 <- drm(y ~ x, fct = LL2.4())
#fit1 <- drm(y ~ x, fct = W2.4(fixed=c(1,NA,1,NA)))
lines(fitted(fit1),lty=2,col="red",lwd=3)
cor(y,predict(fit1))
summary(fit1)

#EM mixture model
library(mixtools)
wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55, 80), sigma = 5)
b <- npEM(y, mu0=2) # iid in pairs
plot(b) # Now only 4 plots, one for each block


#time series analysis
if(!require(Kendall)){install.packages("Kendall")}
if(!require(trend)){install.packages("trend")}
TS <- ts(prime_approx$eve,frequency = 7)
plot(stats::decompose(TS))

MK = MannKendall(TS)
summary(MK)
SMK = SeasonalMannKendall(TS)
summary(SMK)
sens.slope(TS)
sea.sens.slope(TS)
pettitt.test(TS)

plot(diff(prime_approx$eve36 - prime_approx$pc2),type='l')
lines(diff(prime_approx$eve16 - prime_approx$pc2),col="red")

