

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')
if (!require(tuneR)) install.packages('tuneR')
library(ggthemes)
library(tseriesChaos)
library(nonlinearTseries)
library(ggpubr)
library(latex2exp)
library(ggrepel)
library(scales)
library(GGally)
library(rinform)
library(PerformanceAnalytics)
library(fractal)

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}


setwd("/home/STAFF/luczakma/researchdata2/fair/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")

# plot combined data ------

combined_data <- rbind(wien_primes[,c(1,4,5)],wien_primes_b4[,c(1,4,5)],wien_primes_b8[,c(1,4,5)],wien_primes_b16[,c(1,4,5)],wien_primes_b32[,c(1,4,5)],wien_primes_b36[,c(1,4,5)])
ggplot(data=combined_data,
       aes(x=index, y=ShannonWiener, colour=run)) +
  geom_line() + theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

combined_data <- rbind(wien_primes[,c(2,4,5)],wien_primes_b4[,c(2,4,5)],wien_primes_b8[,c(2,4,5)],wien_primes_b16[,c(2,4,5)],wien_primes_b32[,c(2,4,5)],wien_primes_b36[,c(2,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=Pielou, colour=run)) +
  geom_line() + theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

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
  geom_line() + theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

combined_data <- rbind(wien_primes_random[,c(2,4,5)],wien_prandom_numbers[,c(2,4,5)])

ggplot(data=combined_data,
       aes(x=index, y=Pielou, colour=run)) +
  geom_line() + theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

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
#np<-as.vector(np$np)

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
p_count <- unlist(p_count)
plot(p_count,type='l',log='x')

eq2 = function(x,y){
  (y) / (integrate(function(t) 1/log(t),lower=2,upper=x, rel.tol=1e-5)$value)
}

p_count2 <- c()
for(t in 2:max(primes50k$X2)){
  p_count2 <- c(p_count2,eq2(x=t,y=np[t,1]))
}
p_count2 <- unlist(p_count2)
plot(p_count2,type='l',log='x')

plot_data <- as.data.frame(p_count)
plot_data$idx <- c(1:nrow(plot_data))


ggplot(data=plot_data,
       aes(x=idx, y=p_count)) +
  geom_line() + theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

plot_data <- as.data.frame(p_count2)
plot_data$idx <- c(1:nrow(plot_data))


ggplot(data=plot_data,
       aes(x=idx, y=p_count2)) +
  geom_line() + theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

prime_approx <- data.frame(pc1=p_count,pc2=c(Inf,p_count2))
prime_approx.m <- reshape2::melt(prime_approx)
ggplot(data=prime_approx.m,
       aes(x=c(1:nrow(prime_approx),1:nrow(prime_approx)), y=value, group=variable)) +
  geom_line(linetype=c(rep("dashed",nrow(prime_approx)),rep("dotted",nrow(prime_approx)))) + 
  theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind() +
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

plot_data$ShWienerRandOrder <- 0
plot_data$PielouRandOrder <- 1

plot_data$ShWienerRandNums <- 0
plot_data$PielouRandNums <- 1

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
    
    plot_data$ShWienerRandOrder[i] <- wien_primes_random_order$ShannonWiener[ShWienerIdx]
    plot_data$ShWienerRandOrder[i] <- wien_primes_random_order$Pielou[ShWienerIdx]
    
    plot_data$ShWienerRandNums[i] <- wien_prandom_numbers$ShannonWiener[ShWienerIdx]
    plot_data$ShWienerRandNums[i] <- wien_prandom_numbers$Pielou[ShWienerIdx]
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
  theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()
ggplot(data=prime_approx.m,
       aes(x=c(1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx)
       ), y=value, colour=variable)) +
  geom_line() + 
  theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

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
  theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()

ggplot(data=prime_approx.m,
       aes(x=c(1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx),
               1:nrow(prime_approx)
       ), y=value, colour=variable)) +
  geom_line() + 
  theme_clean() + scale_x_continuous(trans='log10') + scale_colour_colorblind()
#geom_text(x=10, y=1.5, label=paste0(expression(pi(x)))) + geom_text(x=10, y=0.5, label=paste0(expression(pi(x))))





# test granger causality ------
library(lmtest)
set.seed(20160227)
grangertest(eve ~ pc1, order = 1, data = prime_approx)




# pertubation analysis ------

### characteristic polynomial and eigenvalue study of A vs B
library(polynom)
library(pracma)
library(Matrix)
library(RSpectra)
library(nonpar)
options(scipen=100000)

# how many eigenvalues?
k <- 200

dirs <- list.dirs("../../output/2019-11-15-letter-to-n-study",full.names = F,recursive = F)
dirs<-c(dirs[21:60],dirs[93:112])

# this is amtrix A
A <- as.matrix(readr::read_csv("../../output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/TICmatrix.csv"))
colnames(A) <- c(1:ncol(A))
rownames(A) <- c(1:ncol(A))
A2 = as(A, "dsCMatrix")
rm(A)

print("### for A")
eValsA_small <- eigs(A2, k, opts = list(retvec = TRUE), sigma = 0)
print(paste0("smallest EV: ",tail(eValsA_small$values,1)))
eValsA <- eigs(A2, k, opts = list(retvec = TRUE))
print(paste0("largest EV: ",eValsA$values[1]))
print(paste0("Condition number (with abs): ",abs(eValsA$values[1])/abs(tail(eValsA_small$values,1))))
print(paste0("Condition number: ",eValsA$values[1]/tail(eValsA_small$values,1)))
print("###")
readr::write_csv(as.data.frame(eValsA$values),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")
readr::write_csv(as.data.frame(eValsA$vectors),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvecs.csv")
readr::write_csv(as.data.frame(eValsA_small$values),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals_small.csv")
readr::write_csv(as.data.frame(eValsA_small$vectors),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvecs_small.csv")
readr::write_csv(as.data.frame(diff(eValsA$values)),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigengaps.csv")
readr::write_csv(rbind(data.frame(x="Condition number (with abs): ",y=abs(eValsA$values[1])/abs(tail(eValsA_small$values,1))),
                       data.frame(x="Condition number: ",y=eValsA$values[1]/tail(eValsA_small$values,1)),
                       data.frame(x="smallest EV :",y=tail(eValsA_small$values,1))),
                 "/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenMeta.csv")

for(i in dirs){
  # this is matric A+epsilon (pertubated matrix A, rare event removed)
  B <- as.matrix(readr::read_csv(paste0("../../output/2019-11-15-letter-to-n-study/",i,"/createTIC/TICmatrix.csv")))
  colnames(B) <- c(1:ncol(B))
  rownames(B) <- c(1:ncol(B))
  B2 = as(B, "dsCMatrix")
  rm(B)
  
  #A <- matrix(c(1,1,0,0,1,0,0,0,1), 3, 3, byrow=TRUE)
  #matB <- B[1:5000,1:5000]
  # eValsB <- eigen(B,symmetric = T)$values
  
  print(paste0("### for B",i))
  eValsB_small <- eigs(B2, k, opts = list(retvec = TRUE), sigma = 0)
  print(paste0("smallest EV: ",tail(eValsB_small$values,1)))
  eValsB <- eigs(B2, k, opts = list(retvec = TRUE))
  print(paste0("largest EV: ",eValsB$values[1]))
  print(paste0("Condition number (with abs): ",abs(eValsB$values[1])/abs(tail(eValsB_small$values,1))))
  print(paste0("Condition number: ",eValsB$values[1]/tail(eValsB_small$values,1)))
  print("###")
  
  #eigenvalue difference
  png(filename = paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"evalDiff.png"))
  plot(eValsA$values - eValsB$values,type='l',main=paste0("Eigenvalue Difference Tail Remove ",i))
  dev.off()
  eDiffSum <- sum(abs(eValsA$values - eValsB$values))
  print(paste0("Eigenvalue difference evalA-evalB: ",i," ",eDiffSum))
  png(filename = paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"evalDiff_CDF.png"))
  plot(ecdf(eValsA$values - eValsB$values),main=paste0("Eigenvalue Difference CDF Tail Remove ",i))
  dev.off()
  
  readr::write_csv(as.data.frame(eValsB$values),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvals.csv"))
  readr::write_csv(as.data.frame(eValsB$vectors),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvecs.csv"))
  readr::write_csv(as.data.frame(eValsB_small$values),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvals_small.csv"))
  readr::write_csv(as.data.frame(eValsB_small$vectors),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvecs_small.csv"))
  readr::write_csv(as.data.frame(diff(eValsB$values)),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigengaps.csv"))
  readr::write_csv(rbind(data.frame(x="Condition number (with abs): ",y=abs(eValsB$values[1])/abs(tail(eValsB_small$values,1))),
                         data.frame(x="Condition number: ",y=eValsB$values[1]/tail(eValsB_small$values,1)),
                         data.frame(x=":smalles EV ",y=tail(eValsB_small$values,1))),
                   paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenMeta.csv"))
  
}


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







# rinform
# KL Divergence
# 

p <- Dist(tokenProbsPrimes$freq)
q <- Dist(tokenProbsRandoms$freq)
# for probability
shannon_relative_entropy(p, q, b = 2)

# for series
xs <- coord_primes$diversity
ys <- coord_prandom_numbers$diversity
re <- relative_entropy(xs, ys, local = T)
mutual_info(matrix(c(xs,ys),ncol = 2))
#t(re)
plot(re,type='l')

# Block Entropy
# build symbol sequence 

sets <- list()

symbolSeq <- apply(nodes,1,function(x){
  if(length(sets)==0){
    sets[[as.character(x[2])]] <<- 1
    which(names(sets) == x[2])
  } else if(is.null(sets[[x[2]]])){
    sets[[as.character(x[2])]] <<- 1
    which(names(sets) == x[2])
  }else{
    which(names(sets) == x[2])
  }
})
write_csv(as.data.frame(unlist(symbolSeq)),"~/Downloads/testseq-primes-random-order.csv")
series <- symbolSeq
#N <- c(1:5)
block_entropy(series, k = 2)


# differences??? ------

summary(primesResData)
summary(randomResData)

x <- 1:nrow(plot_data)
y <- plot_data$ShWiener

d<-data.frame(x,y)
logEstimate <- lm(y~log(x),data=d)
plot(x,y,type='l')
lines(x=names(fitted(logEstimate)),y=fitted(logEstimate))

# Model performance
data.frame(
  RMSE = RMSE(fitted(logEstimate)[-c(1:2)], p_count2[-1]),
  R2 = R2(fitted(logEstimate)[-1], p_count2[-1])
)

#d<-data.frame(t=x,y=y)
#fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = d)

cor(x,y)
cov(x,y)
mean(x)
mean(y)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)




plot(unlist(primesEvalVar),primesResData$lapSpec, pch=20)
chart.Correlation(cbind(unlist(primesEvalVar),primesResData$lapSpec))

plot(unlist(randEvalVar),randomResData$lapSpec, pch=20)
chart.Correlation(cbind(unlist(randEvalVar),randomResData$lapSpec))

plot(unlist(primesRandEvalVar),randomPrimesResData$lapSpec, pch=20)
chart.Correlation(cbind(unlist(primesRandEvalVar),randomPrimesResData$lapSpec))

plot(unlist(primesRandOrderEvalVar),randomOrderPrimesResData$lapSpec, pch=20)
chart.Correlation(cbind(unlist(primesRandEvalVar),randomPrimesResData$lapSpec))

plot(unlist(primesEvalVar),primesResData$hurstCoeffSpec, pch=20)
plot(unlist(randEvalVar),randomResData$hurstCoeffSpec, pch=20)
plot(unlist(primesRandEvalVar),randomPrimesResData$hurstCoeffSpec, pch=20)
plot(unlist(primesRandOrderEvalVar),randomOrderPrimesResData$hurstCoeffSpec, pch=20)

plot(density(diff(primesResData$largestEV)))
plot(density(randomResData$largestEV))
plot(density(randomPrimesResData$largestEV))
plot(density(diff(randomOrderPrimesResData$largestEV)))

plot(ecdf(primesResData$largestEV))
plot(ecdf(randomResData$largestEV))
plot(ecdf(randomPrimesResData$largestEV))
plot(ecdf(randomOrderPrimesResData$largestEV))

plot(density(cumsum(primesResData$largestEV-mean(primesResData$largestEV))),type='l')

plot(density(primesResData$conditionNumber))
plot(density(randomResData$conditionNumber))
plot(density(randomPrimesResData$conditionNumber))
plot(density(randomOrderPrimesResData$conditionNumber))

plot(ecdf(primesResData$conditionNumber))
plot(ecdf(randomResData$conditionNumber))
plot(ecdf(randomPrimesResData$conditionNumber))
plot(ecdf(randomOrderPrimesResData$conditionNumber))

#ks test  no good due to small sample
ks.test(diff(primesResData$largestEV),diff(randomOrderPrimesResData$largestEV))

nonpar::cucconi.test(diff(primesResData$largestEV),diff(randomOrderPrimesResData$largestEV))


plot(density(cumsum(randomResData$largestEV-mean(randomResData$largestEV))),type='l')
plot(density(cumsum(randomPrimesResData$largestEV-mean(randomPrimesResData$largestEV))),type='l')


plot(primesResData$setProb,primesResData$lapSpec)
plot(randomResData$setProb,randomResData$lapSpec)
plot(randomPrimesResData$setProb,randomPrimesResData$lapSpec)
plot(randomOrderPrimesResData$setProb,randomOrderPrimesResData$lapSpec)


#log transform
# Build the model
model <- lm(lr ~ log(setProb), data = resData)
# Make predictions
predictions <- model %>% predict(resData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, resData$lr),
  R2 = R2(predictions, resData$lr)
)

ggplot(resData, aes(setProb, lr) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x))

#splines
# Build the model
knots <- quantile(primesResData$setProb, p = c(0.25, 0.5, 0.75))
model <- lm (lr ~ bs(setProb, knots = knots), data = primesResData)
# Make predictions
predictions <- model %>% predict(primesResData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, primesResData$lr),
  R2 = R2(predictions, primesResData$lr)
)
ggplot(primesResData, aes(setProb, lr) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))



# #powerlaw
library(poweRlaw)

m_bl = poweRlaw::conpl$new(randomPrimesResData$lr)
est = estimate_xmin(m_bl)
m_bl$setXmin(est)
m_bl$setPars(estimate_pars(m_bl))
# print(paste0(fils[i]," alpha: ",m_bl$pars," xMin: ",m_bl$xmin))
# #lognormal
m_ln = conlnorm$new(randomPrimesResData$lr)
est = estimate_xmin(m_ln)
m_ln$setXmin(est)
m_ln$setPars(estimate_pars(m_ln))
# # #poissoin
m_exp = conexp$new(randomPrimesResData$lr)
est = estimate_xmin(m_exp)
m_exp$setXmin(est)
m_exp$setPars(estimate_pars(m_exp))
# # 
# # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
plot(m_bl, ylab="CDF",pch=20)
text(0.00005,0.01,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
lines(m_bl, col="red")
lines(m_ln, col="green")
lines(m_exp, col="blue")


ggplot(resData, 
       aes(x=resData$setProb,y=resData$hurstCoeffSpec, color=runData)) + scale_x_continuous(trans='log10') +
  #geom_line() +
  #geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  #scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_smooth(span = 0.6) +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#56B4E9", "primesrandomorderB50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Hurst coefficient", colour="Dataset") +
  #annotate(geom="text", x=-5.5, y=min(resData$hurstCoeffDiv)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  #annotate(geom="text", x=6, y=min(resData$hurstCoeffDiv)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=resData$setProb,y=resData$lapSpec, color=runData)) + scale_x_continuous(trans='log10') +
  #geom_line() +
  #geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  #scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_smooth(span = 0.6) +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#56B4E9", "primesrandomorderB50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage", colour="Dataset") +
  #annotate(geom="text", x=-5.5, y=min(resData$hurstCoeffDiv)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  #annotate(geom="text", x=6, y=min(resData$hurstCoeffDiv)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=resData$setProb,y=resData$recPercSpec, color=runData)) + scale_x_continuous(trans='log10') +
  #geom_line() +
  #geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  #scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_smooth(span = 0.6) +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#56B4E9", "primesrandomorderB50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence ratio", colour="Dataset") +
  #annotate(geom="text", x=-5.5, y=min(resData$hurstCoeffDiv)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  #annotate(geom="text", x=6, y=min(resData$hurstCoeffDiv)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL





chart.Correlation(cbind(primesResData$recPercSpec,randomPrimesResData$recPercSpec,randomResData$recPercSpec))
chart.Correlation(cbind(primesResData$recDetSpec,randomPrimesResData$recDetSpec,randomResData$recDetSpec))
chart.Correlation(cbind(primesResData$recLmeanSpec,randomPrimesResData$recLmeanSpec,randomResData$recLmeanSpec))
chart.Correlation(cbind(primesResData$recRatioSpec,randomPrimesResData$recRatioSpec,randomResData$recRatioSpec))
chart.Correlation(cbind(primesResData$recVmeanSpec,randomPrimesResData$recVmeanSpec,randomResData$recVmeanSpec))
chart.Correlation(cbind(primesResData$recTrendSpec,randomPrimesResData$recTrendSpec,randomResData$recTrendSpec))
chart.Correlation(cbind(primesResData$recRateSpecMean,randomPrimesResData$recRateSpecMean,randomResData$recRateSpecMean))
chart.Correlation(cbind(primesResData$recEntropySpec,randomPrimesResData$recEntropySpec,randomResData$recEntropySpec))




foo <- origResData$hurstCoeffSpec[1]-resData$hurstCoeffSpec[which(resData$runData=="random50k")]
diff(foo)
plot(foo,type='l')


resData2 <- rbind(resData,origResData)

ggplot(resData2, 
       aes(x=resData2$axisTicks,y=resData2$lapDiv, color=runData)) +# scale_x_continuous(trans='log10') +
  #geom_line() +
  #geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  #scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_smooth(span = 0.6) +
  geom_label_repel(aes(label = resData2$axisTicks, fill=resData2$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#56B4E9", "primesrandomorderB50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage", colour="Dataset") +
  #annotate(geom="text", x=-5.5, y=min(resData$hurstCoeffDiv)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  #annotate(geom="text", x=6, y=min(resData$hurstCoeffDiv)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL


toDiff <- list(colnames(origResData)[c(4:63)])

resDiffData <- resData[0,]

for(orig in 1:nrow(origResData)){
  runData <- origResData$runData[orig]
  
  subRes <- resData[which(resData$runData==runData),]
  
  for(differ in toDiff){
    subRes[,differ] <- subRes[,differ] - c(origResData[orig,differ])
  }
  
  resDiffData <- rbind(resDiffData,subRes)
}

primeRows <- which(resDiffData$runData=="primes50k")
primeRandomOrderRows <- which(resDiffData$runData=="primesrandomorder50k")
primeRandomOrderBRows <- which(resDiffData$runData=="primesrandomorderB50k")
randomNumbersRows <- which(resDiffData$runData=="random50k")


resDiffData$lr2[primeRandomOrderRows]



resDf <- data.frame(it1 = scale(resDiffData$largestEV),
                    it2 = scale(resDiffData$hurstCoeffSpec))
meansDiamondPlot(resDf)

scaledResData <- resDiffData

for(stp in c(4:63,68)){
  scaledResData[,stp] <- c(scale(scaledResData[,stp]))
}

meansComparisonDiamondPlot(scaledResData,
                           items=unlist(toDiff),
                           compareBy = 'runData',
                           conf.level = .99)



interp_data <- interp(x=resDiffData$axisTicks[primeRows],
                      y=resDiffData$recEntropyDiv[primeRows],
                      z=resDiffData$hurstCoeffDiv[primeRows])

p <- plot_ly(x=interp_data$x, y=interp_data$y, z = interp_data$z) %>% add_surface()
p

lines3D(resData$axisTicks[primeRows],
        resData$mean_div[primeRows],
        resData$hurstCoeffDiv[primeRows])

plot_ly(x=resData$recEntropyDiv,
        y=resData$max_div,
        z = resData$hurstCoeffDiv, 
        type = "scatter3d",mode="points",opacity=0.5, color = resData$runData) #%>%
  # layout(
  #   title = "Discriminant Analysis",
  #   scene = list(
  #     xaxis = list(title = "Token Set Recurrence Entropy"),
  #     yaxis = list(title = "Max Token Set Diversity"),
  #     zaxis = list(title = "Hurst Coefficient of Token Set Diversity")
  #   ))



interp_data <- interp(x=coord_primes_random$t,y=coord_primes_random$specificity,z=coord_primes_random$diversity)

p <- plot_ly(x=interp_data$x, y=interp_data$y, z = interp_data$z) %>% add_surface()
p


plot(x=scale(resDiffData$recRatioDiv[primeRows]),y=scale(resDiffData$recTrendSpec[primeRows]))
cor(scale(resDiffData$recRatioDiv[primeRows]),scale(resDiffData$recTrendSpec[primeRows]))
X <- matrix(c(scale(resDiffData$recRatioDiv[primeRows]),scale(resDiffData$recTrendSpec[primeRows])),ncol = 2)
chull(X)
plot(X, cex = 0.5)
hpts <- chull(X)
hpts <- c(hpts, hpts[1])
lines(X[hpts, ])

library(DescTools)

ggplot(filter(resDiffData,runData=="primes50k"), aes(setProb,lr2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
AUC(x=scale(resDiffData$setProb[primeRows]),y=scale(resDiffData$lr2[primeRows]), method="spline")



plo <- ggplot(resData, 
              aes(x=resData$setProb,y=resData$lr2, color=runData)) + scale_x_continuous(trans='log10') +
  #geom_line() +
  #geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  #scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + 
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#56B4E9", "primesrandomorderB50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "LR", colour="Dataset") +
  #annotate(geom="text", x=-5.5, y=min(resData$hurstCoeffDiv)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  #annotate(geom="text", x=6, y=min(resData$hurstCoeffDiv)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() + stat_smooth(method = gam, formula = y ~ s(x)) +
  NULL
plo

eGap <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigengaps.csv")
eGap2 <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/primes50k_tailRemove_1-discrete-tokenised-2019-12-13-09-03-55-eigengaps.csv")

meansComparisonDiamondPlot(scaledResData,
                           items=unlist(toDiff),
                           compareBy = 'runData',
                           conf.level = .99)
library(BEST)
priors <- list(muM = 6, muSD = 2)
BESTout <- BESTmcmc(primesResData$lapSpec, randomResData$lapSpec, priors=priors, parallel=FALSE)
plotAll(BESTout)

sm.density.compare(resData$recVmeanDiv, as.factor(resData$runData), xlab="feature x")
colfill<-c(2:(2+length(levels(as.factor(resData$runData))))) 
legend(locator(1), levels(as.factor(resData$runData)), fill=colfill) 

library(magrittr)
library(glmnet)
library(pROC)

x_cont <- as.matrix(df[,1:6])
y_cont <- as.matrix(resData$runData)

ridge1 <- glmnet(x = x_cont, y = y_cont, alpha = 0, family = "multinomial")
plot(ridge1, xvar = "lambda")

ridge1_cv <- cv.glmnet(x = x_cont, y = y_cont,
                       ## type.measure: loss to use for cross-validation.
                       type.measure = "mse",
                       ## K = 10 is the default.
                       nfold = 10,
                       ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                       alpha = 0, family = "multinomial")
## Penalty vs CV MSE plot
plot(ridge1_cv)
