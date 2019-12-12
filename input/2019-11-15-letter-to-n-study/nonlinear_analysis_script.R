
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')
if (!require(tuneR)) install.packages('tuneR')

setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")




# DATA ANALYSIS -----------------------------------------------------------




#primes combined plotting
#


coord_primes <- read.csv("../../output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/TICCoordinates.csv",stringsAsFactors = F)
coord_primes_b4 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base4-dataset-50k-discrete-tokenised-2019-11-21-20-52-25/createTIC/TICCoordinates.csv",stringsAsFactors = F)
coord_primes_b8 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base8-dataset-50k-discrete-tokenised-2019-11-21-20-59-13/createTIC/TICCoordinates.csv",stringsAsFactors = F)
coord_primes_b16 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base16-dataset-50k-discrete-tokenised-2019-11-21-21-08-56/createTIC/TICCoordinates.csv",stringsAsFactors = F)
coord_primes_b32 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base32-dataset-50k-discrete-tokenised-2019-11-25-17-34-48/createTIC/TICCoordinates.csv",stringsAsFactors = F)
coord_primes_b36 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base36-dataset-50k-discrete-tokenised-2019-11-25-17-57-06/createTIC/TICCoordinates.csv",stringsAsFactors = F)


coord_primes_random <- read.csv("../../output/2019-11-15-letter-to-n-study/primes50k-random-discrete-tokenised-2019-11-21-20-42-39/createTIC/TICCoordinates.csv",stringsAsFactors = F)
coord_prandom_numbers <- read.csv("../../output/2019-11-15-letter-to-n-study/randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27/createTIC/TICCoordinates.csv",stringsAsFactors = F)


coord_primes_tailRem1 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes50k_tailRemove1-discrete-tokenised-2019-12-11-21-27-11/createTIC/TICCoordinates.csv",stringsAsFactors = F)

wien_primes <- read.csv("../../output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_primes_b4 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base4-dataset-50k-discrete-tokenised-2019-11-21-20-52-25/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_primes_b8 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base8-dataset-50k-discrete-tokenised-2019-11-21-20-59-13/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_primes_b16 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base16-dataset-50k-discrete-tokenised-2019-11-21-21-08-56/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_primes_b32 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base32-dataset-50k-discrete-tokenised-2019-11-25-17-34-48/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_primes_b36 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes-base36-dataset-50k-discrete-tokenised-2019-11-25-17-57-06/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)

wien_primes$run <- "primes"
wien_primes_b4$run <- "primes_b4"
wien_primes_b8$run <- "primes_b8"
wien_primes_b16$run <- "primes_b16"
wien_primes_b32$run <- "primes_b32"
wien_primes_b36$run <- "primes_b36"

wien_primes$index <- c(1:50000)
wien_primes_b4$index <- c(1:50000)
wien_primes_b8$index <- c(1:50000)
wien_primes_b16$index <- c(1:50000)
wien_primes_b32$index <- c(1:50000)
wien_primes_b36$index <- c(1:50000)

wien_primes$csumP <- c(0,cumsum(diff(wien_primes$Pielou)))
wien_primes_b4$csumP <- c(0,cumsum(diff(wien_primes_b4$Pielou)))
wien_primes_b8$csumP <- c(0,cumsum(diff(wien_primes_b8$Pielou)))
wien_primes_b16$csumP <- c(0,cumsum(diff(wien_primes_b16$Pielou)))
wien_primes_b32$csumP <- c(0,cumsum(diff(wien_primes_b32$Pielou)))
wien_primes_b36$csumP <- c(0,cumsum(diff(wien_primes_b36$Pielou)))

wien_primes$csumS <- c(0,cumsum(diff(wien_primes$ShannonWiener)))
wien_primes_b4$csumS <- c(0,cumsum(diff(wien_primes_b4$ShannonWiener)))
wien_primes_b8$csumS <- c(0,cumsum(diff(wien_primes_b8$ShannonWiener)))
wien_primes_b16$csumS <- c(0,cumsum(diff(wien_primes_b16$ShannonWiener)))
wien_primes_b32$csumS <- c(0,cumsum(diff(wien_primes_b32$ShannonWiener)))
wien_primes_b36$csumS <- c(0,cumsum(diff(wien_primes_b36$ShannonWiener)))

wien_primes_random <- read.csv("../../output/2019-11-15-letter-to-n-study/primes50k-random-discrete-tokenised-2019-11-21-20-42-39/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_prandom_numbers <- read.csv("../../output/2019-11-15-letter-to-n-study/randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)

wien_primes_random$run <- "primes_random"
wien_prandom_numbers$run <- "random_numbers"
wien_primes_random$index <- c(1:50000)
wien_prandom_numbers$index <- c(1:50000)
wien_primes_random$csumP <- c(0,cumsum(diff(wien_primes_random$Pielou)))
wien_prandom_numbers$csumP <- c(0,cumsum(diff(wien_prandom_numbers$Pielou)))
wien_primes_random$csumS <- c(0,cumsum(diff(wien_primes_random$ShannonWiener)))
wien_prandom_numbers$csumS <- c(0,cumsum(diff(wien_prandom_numbers$ShannonWiener)))


wien_primes_tailRem1 <- read.csv("../../output/2019-11-15-letter-to-n-study/primes50k_tailRemove1-discrete-tokenised-2019-12-11-21-27-11/createTIC/TICInfoTheory2.csv",stringsAsFactors = F)
wien_primes_tailRem1$run <- "primes"
wien_primes_tailRem1$index <- c(1:50000)

###  NONLINEAR

library(tseriesChaos)

myts <- ts(prime_approx$eve)
stplot(myts, m=3, d=8, idt=1, mdt=250)

library(nonlinearTseries)
library(plot3D)
library(plotly)

#eDim <- nonlinearTseries::embeddingDims(wien_primes$ShannonWiener)
takens <- nonlinearTseries::buildTakens(diff(head(coord_primes_tailRem1$specificity,50000)), embedding.dim = 3, time.lag = 1)
plot(takens, pch='.')
lines3D(takens[,1],takens[,2],takens[,3])
takens.df <- as.data.frame(takens)
plot_ly(takens.df,x=~V1,y=~V2,z=~V3, type = "scatter3d",mode="lines",opacity=0.2)

ikeda.map=ikedaMap(n.sample = 1000, n.transient=10, do.plot=TRUE)
henon.map=henon(n.sample = 1000, n.transient=10,do.plot=TRUE,
                start=c(-0.006423277,-0.473545134))

r.ts = rossler(time=seq(0,30,by = 0.01))
lor=lorenz(time=seq(0,30,by = 0.01))
sinai.map = sinaiMap(n.sample = 1000, n.transient=10,do.plot=TRUE)

plot(ts(ikeda.map$x))
plot(ts(henon.map$x))
plot(lor$time,lor$x,type="l")
plot(ts(sinai.map$x))
plot(r.ts,type = 'l')


takens <- nonlinearTseries::buildTakens(diff(head(coord_primes$diversity,50000)), embedding.dim = 3, time.lag = 1)
plot(takens, pch='.')
lines3D(takens[,1],takens[,2],takens[,3])

#rqa.analysis=rqa(takens=takens,radius = 3,time.lag = 2)
rqa.analysis=rqa(time.series = diff(head(coord_primes$specificity,50000)),radius = 3,time.lag = 1)
plot(rqa.analysis)
maxLyapunov(diff(head(coord_primes$specificity,50000)),radius = 3)
#recurr(diff(coord_primes$diversity), m=3, d=2, start.time=0, end.time=2000)