# Step 1 - construct original dataset from input data txt file (e.g. primes5k.txt) using the TICCreate via input/run.R -----

# Step 2 - pertubation data generation -----
# build the pertubation data from that original data set
setwd("/home/STAFF/luczakma/researchdata2/fair/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")

library(readr)

links <- readr::read_csv("../../output/2019-11-15-letter-to-n-study/primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43/createTIC/links.csv")
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)


nodes <- readr::read_csv("../../output/2019-11-15-letter-to-n-study/primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43/createTIC/nodes.csv")
setProbs <- plyr::count(nodes$tokens)
setProbs$prob <- as.numeric(setProbs$freq)/nrow(nodes)
distr <- plyr::count(setProbs$freq)
#plot(distr,type='l')

primes50k <- read_delim("data/primesrandomorder50k.txt", 
                        ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

comProbs <- apply(setProbs,1,function(x){
  tokens <- unlist(strsplit(as.character(x[1]),", "))
  product <- c()
  lowToken <- list(NULL,NULL)
  highToken <- list(NULL,NULL)
  for(i in tokens){
    if(is.null(lowToken[[2]])){
      lowToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
      lowToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
    } else if(lowToken[[2]] > tokenProbs[which(tokenProbs$x==i),3]){
      lowToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
      lowToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
    }
    
    if(is.null(highToken[[2]])){
      highToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
      highToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
    } else if(highToken[[2]] < tokenProbs[which(tokenProbs$x==i),3]){
      highToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
      highToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
    }
    
    product <- c(product,tokenProbs[which(tokenProbs$x==i),3])
  }
  res <- list(prod(product),lowToken[[1]],highToken[[1]])
  res
})

setProbs$tokenprobs <- sapply(comProbs,function(x){
  x[[1]]
})

setProbs$lowToken <- sapply(comProbs,function(x){
  x[[2]]
})

setProbs$highToken <- sapply(comProbs,function(x){
  x[[3]]
})

setProbs$overallprob <- setProbs$tokenprobs * setProbs$prob

setProbs <- setProbs[order(setProbs$prob),]

# 1 remove the item with lowest probabiliyty from the set with lowest set probability (do for bottom 5 sets)
# 
for(i in 1:20){
  primes50k_tailRemove1 <- primes50k
  nodesToRem <- which(nodes$tokens == setProbs$x[i])
  print(nodesToRem)
  for(j in 1:length(nodesToRem)){
    tokens <- unlist(strsplit(as.character(primes50k_tailRemove1[nodesToRem[j],3]),", "))
    tokens <- tokens[-which(tokens == setProbs$lowToken[i])]
    primes50k_tailRemove1[nodesToRem[j],3] <- paste0(tokens,collapse = ", ")
  }
  write_delim(as.data.frame(primes50k_tailRemove1),paste0("data/primesrandomorder50k_tailRemove_",i,".txt"),delim = ";",col_names = F)
}

# 4 remove item with lowest probability from set with lowest co bined token probability
# 
# setProbs <- setProbs[order(setProbs$tokenprobs),]
# for(i in 1:20){
#   primes50k_tailRemove1 <- primes50k
#   nodesToRem <- which(nodes$tokens == setProbs$x[i])
#   print(nodesToRem)
#   for(j in 1:length(nodesToRem)){
#     tokens <- unlist(strsplit(as.character(primes50k_tailRemove1[nodesToRem[j],3]),", "))
#     tokens <- tokens[-which(tokens == setProbs$lowToken[i])]
#     primes50k_tailRemove1[nodesToRem[j],3] <- paste0(tokens,collapse = ", ")
#   }
#   write_delim(as.data.frame(primes50k_tailRemove1),paste0("data/primes50k_tailRemove2_",i,".txt"),delim = ";",col_names = F)
# }

# 2 remove item with highest probability from set with highest set probability
# 
setProbs <- setProbs[order(-setProbs$prob),]

# 1 remove the item with lowest probabiliyty from the set with lowest set probability (do for bottom 5 sets)
# 
for(i in 1:20){
  primes50k_tailRemove1 <- primes50k
  nodesToRem <- which(nodes$tokens == setProbs$x[i])
  print(nodesToRem)
  for(j in 1:length(nodesToRem)){
    tokens <- unlist(strsplit(as.character(primes50k_tailRemove1[nodesToRem[j],3]),", "))
    tokens <- tokens[-which(tokens == setProbs$lowToken[i])]
    primes50k_tailRemove1[nodesToRem[j],3] <- paste0(tokens,collapse = ", ")
  }
  write_delim(as.data.frame(primes50k_tailRemove1),paste0("data/primesrandomorder50k_headRemove_",i,".txt"),delim = ";",col_names = F)
}


# Step 3 - construct pertubation TIC datasets from input data txt files created in step before (e.g. primes5k.txt) using the TICCreate via input/run.R -> PIPELINE PRIMES WITH k-th head removed ----

# Step 4 - eigendecomposition -----
# eigendecomposition for all the new TICs
# read in dirs from the TIC output directory and perform decomposition for all in there
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
#readr::write_csv(as.data.frame(eValsA$vectors),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvecs.csv")
readr::write_csv(as.data.frame(eValsA_small$values),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals_small.csv")
#readr::write_csv(as.data.frame(eValsA_small$vectors),"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvecs_small.csv")
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
  #png(filename = paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"evalDiff.png"))
  #plot(eValsA$values - eValsB$values,type='l',main=paste0("Eigenvalue Difference Tail Remove ",i))
  #dev.off()
  #eDiffSum <- sum(abs(eValsA$values - eValsB$values))
  #print(paste0("Eigenvalue difference evalA-evalB: ",i," ",eDiffSum))
  #png(filename = paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"evalDiff_CDF.png"))
  #plot(ecdf(eValsA$values - eValsB$values),main=paste0("Eigenvalue Difference CDF Tail Remove ",i))
  #dev.off()
  
  readr::write_csv(as.data.frame(eValsB$values),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvals.csv"))
  #readr::write_csv(as.data.frame(eValsB$vectors),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvecs.csv"))
  readr::write_csv(as.data.frame(eValsB_small$values),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvals_small.csv"))
  #readr::write_csv(as.data.frame(eValsB_small$vectors),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenvecs_small.csv"))
  readr::write_csv(as.data.frame(diff(eValsB$values)),paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigengaps.csv"))
  readr::write_csv(rbind(data.frame(x="Condition number (with abs): ",y=abs(eValsB$values[1])/abs(tail(eValsB_small$values,1))),
                         data.frame(x="Condition number: ",y=eValsB$values[1]/tail(eValsB_small$values,1)),
                         data.frame(x=":smalles EV ",y=tail(eValsB_small$values,1))),
                   paste0("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/",i,"-eigenMeta.csv"))
  
}


# Step 5 - result data generation for pertubatiuon data -----

library(tidyverse)
library(poweRlaw)
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
library(akima)
library(caret)
library(BEST)
library(mgcv)
options(scipen = 100000)

set.seed(3011)
RNGkind("L'Ecuyer-CMRG")

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

setwd("/home/STAFF/luczakma/researchdata2/fair/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")
primes50k <- read_delim("data/primes50k.txt", 
                        ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
np <- read_csv("data/prime_counts.txt")
#np<-as.vector(np$np)

nmax<-max(primes50k$X2)
primelist <- primes50k$X2

eq = function(x,y){(y)  / (x/(log(x)))}
p_count <- eq(x=c(1:max(primes50k$X2)),y=np)
p_count <- unlist(p_count)

eq2 = function(x,y){
  (y) / (integrate(function(t) 1/log(t),lower=2,upper=x, rel.tol=1e-5)$value)
}
# p_count2 <- c()
# for(t in 2:max(primes50k$X2)){
#   p_count2 <- c(p_count2,eq2(x=t,y=np[t,1]))
# }
# p_count2 <- unlist(p_count2)
p_count2 <- read_csv("data/p_count2.csv")
p_count2 <- c(p_count2$p_count2)

setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")


fils <- list.files(".",full.names = F, pattern = "-eigenvals.csv")
fils <- fils[-which(fils == "primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")]
fils <- fils[-which(fils == "random50k-discrete-tokenised-2020-01-28-10-32-09-eigenvals.csv")]
fils <- fils[-which(fils == "randomprimes50k-discrete-tokenised-2020-01-28-11-16-26-eigenvals.csv")]
fils <- fils[-which(fils == "primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43-eigenvals.csv")]
fils <- fils[-which(fils == "primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenvals.csv")]
fils <- fils[-which(fils == "primesrandomorderC50k-discrete-tokenised-2020-01-28-08-39-47-eigenvals.csv")]

primeseVals <- matrix(nrow = 200,ncol = 40)
primesRandeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrdereVals <- matrix(nrow = 200,ncol = 40)
primesRandOrderBeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrderCeVals <- matrix(nrow = 200,ncol = 40)
randeVals <- matrix(nrow = 200,ncol = 40)

for(fil in 1:length(fils)){
  varName <- unlist(strsplit(fils[fil],"-"))[1]
  assign(paste0("R",fil),
         read_csv(paste0(fils[fil])))
  
  tmpRunId <- unlist(strsplit(varName,"_"))
  tmpeVals <- read_csv(paste0(fils[fil]))
  tmpeVals <- unlist(tmpeVals[,1])
  names(tmpeVals) <- NULL
  if(tmpRunId[1] == "random50k"){
    if(tmpRunId[2]=="headRemove") randeVals[,(41-as.numeric(tmpRunId[3]))] <- tmpeVals
    else randeVals[,as.numeric(tmpRunId[3])] <- tmpeVals
  } else if(tmpRunId[1] == "primesrandom50k"){
    if(tmpRunId[2]=="headRemove") primesRandeVals[,(41-as.numeric(tmpRunId[3]))] <- tmpeVals
    else primesRandeVals[,as.numeric(tmpRunId[3])] <- tmpeVals
  } else if(tmpRunId[1] == "primesrandomorder50k"){
    if(tmpRunId[2]=="headRemove") primesRandOrdereVals[,(41-as.numeric(tmpRunId[3]))] <- tmpeVals
    else primesRandOrdereVals[,as.numeric(tmpRunId[3])] <- tmpeVals
  } else if(tmpRunId[1] == "primesrandomorderB50k"){
    if(tmpRunId[2]=="headRemove") primesRandOrdereVals[,(41-as.numeric(tmpRunId[3]))] <- tmpeVals
    else primesRandOrdereVals[,as.numeric(tmpRunId[3])] <- tmpeVals
  } else if(tmpRunId[1] == "primesrandomorderC50k"){
    if(tmpRunId[2]=="headRemove") primesRandOrdereVals[,(41-as.numeric(tmpRunId[3]))] <- tmpeVals
    else primesRandOrdereVals[,as.numeric(tmpRunId[3])] <- tmpeVals
  } else{
    if(tmpRunId[2]=="headRemove") primeseVals[,(41-as.numeric(tmpRunId[3]))] <- tmpeVals
    else primeseVals[,as.numeric(tmpRunId[3])] <- tmpeVals
  }
}

primes.orig.evals <- read_csv("primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")
primes.orig.meta <- read_csv("primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenMeta.csv")

orig.evals <- read_csv("primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")
orig.meta <- read_csv("primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenMeta.csv")

primesEvalVar <- list()
primesRandEvalVar <- list()
primesRandOrderEvalVar <- list()
primesRandOrderBEvalVar <- list()
primesRandOrderCEvalVar <- list()
randEvalVar <- list()

for(k in 1:40){
  primesEvalVar[k] <- var(primes.orig.evals,primeseVals[,k])
}

primes.rand.evals <- read_csv("randomprimes50k-discrete-tokenised-2020-01-28-11-16-26-eigenvals.csv")
primes.rand.meta <- read_csv("randomprimes50k-discrete-tokenised-2020-01-28-11-16-26-eigenMeta.csv")

for(k in 1:40){
  primesRandEvalVar[k] <- var(primes.rand.evals,primesRandeVals[,k])
}

primes.randorder.evals <- read_csv("primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43-eigenvals.csv")
primes.randorder.meta <- read_csv("primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43-eigenMeta.csv")

for(k in 1:40){
  primesRandOrderEvalVar[k] <- var(primes.randorder.evals,primesRandOrdereVals[,k])
}

primes.randorderB.evals <- read_csv("primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenvals.csv")
primes.randorderB.meta <- read_csv("primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenMeta.csv")

for(k in 1:40){
  primesRandOrderBEvalVar[k] <- var(primes.randorderB.evals,primesRandOrderBeVals[,k])
}

primes.randorderC.evals <- read_csv("primesrandomorderC50k-discrete-tokenised-2020-01-28-08-39-47-eigenvals.csv")
primes.randorderC.meta <- read_csv("primesrandomorderC50k-discrete-tokenised-2020-01-28-08-39-47-eigenMeta.csv")

for(k in 1:40){
  primesRandOrderCEvalVar[k] <- var(primes.randorderC.evals,primesRandOrderCeVals[,k])
}

randnums.evals <- read_csv("random50k-discrete-tokenised-2020-01-28-10-32-09-eigenvals.csv")
randnums.meta <- read_csv("random50k-discrete-tokenised-2020-01-28-10-32-09-eigenMeta.csv")

for(k in 1:40){
  randEvalVar[k] <- var(randnums.evals,randeVals[,k])
}

resData <- data.frame(runData=character(0),
                      runType=character(0),
                      runId=numeric(0),
                      pLawAlpha=numeric(0),
                      pLawXMin=numeric(0),
                      logNormMu=numeric(0),
                      logNormSigma=numeric(0),
                      logNormXMin=numeric(0),
                      conditionNumber=numeric(0),
                      smallestEV=numeric(0),
                      largestEV=numeric(0),
                      lapSpec=numeric(0),
                      hurstCoeffSpec=numeric(0),
                      lapDiv=numeric(0),
                      hurstCoeffDiv=numeric(0),
                      eDimSpec=numeric(0),
                      eDimDiv=numeric(0),
                      #corrDimSpec=numeric(0),
                      #corrDimDiv=numeric(0),
                      recPercSpec=numeric(0),
                      recDetSpec=numeric(0),
                      recLamSpec=numeric(0),
                      recRatioSpec=numeric(0),
                      recLmaxSpec=numeric(0),
                      recLmeanSpec=numeric(0),
                      recVmaxSpec=numeric(0),
                      recVmeanSpec=numeric(0),
                      recEntropySpec=numeric(0),
                      recTrendSpec=numeric(0),
                      recRateSpecMin=numeric(0),
                      recRateSpecMax=numeric(0),
                      recRateSpecMean=numeric(0),
                      recRateSpecMedian=numeric(0),
                      recRateSpecSd=numeric(0),
                      recPercDiv=numeric(0),
                      recDetDiv=numeric(0),
                      recLamDiv=numeric(0),
                      recRatioDiv=numeric(0),
                      recLmaxDiv=numeric(0),
                      recLmeanDiv=numeric(0),
                      recVmaxDiv=numeric(0),
                      recVmeanDiv=numeric(0),
                      recEntropyDiv=numeric(0),
                      recTrendDiv=numeric(0),
                      recRateDivMin=numeric(0),
                      recRateDivMax=numeric(0),
                      recRateDivMean=numeric(0),
                      recRateDivMedian=numeric(0),
                      recRateDivSd=numeric(0),
                      ent_log_interc=numeric(0),
                      ent_log_sl=numeric(0),
                      piel_log_interc=numeric(0),
                      piel_log_sl=numeric(0),
                      ent_log_interc_prime_count=numeric(0),
                      ent_log_sl_prime_count=numeric(0),
                      piel_log_interc_prime_count=numeric(0),
                      piel_log_sl_prime_count=numeric(0),
                      pcount_dist_wiener=numeric(0),
                      pcount2_dist_wiener=numeric(0),
                      pcount_dist_piel=numeric(0),
                      pcount2_dist_piel=numeric(0),
                      max_spec=numeric(0),
                      max_div=numeric(0),
                      mean_spec=numeric(0),
                      mean_div=numeric(0))

for(i in 1:length(fils)){
  # compute the set probability
  
  runId <- as.numeric(unlist(strsplit(unlist(strsplit(fils[i],"-"))[1],"_"))[3])
  runType <- unlist(strsplit(unlist(strsplit(fils[i],"-"))[1],"_"))[2]
  runData <- unlist(strsplit(unlist(strsplit(fils[i],"-"))[1],"_"))[1]
  setwd("/home/STAFF/luczakma/researchdata2/fair/transcendental-information-cascades/")
  links <- readr::read_csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/links.csv"),progress = F,col_types = cols())
  tokenProbs <- plyr::count(as.character(links$token))
  tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)
  
  
  nodes <- readr::read_csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/nodes.csv"),progress = F,col_types = cols())
  nodes <- nodes[which(nodes$tokens != ""),]
  setProbs <- plyr::count(nodes$tokens)
  setProbs$prob <- as.numeric(setProbs$freq)/nrow(nodes)
  distr <- plyr::count(setProbs$freq)
  plot(distr,type='l')
  
  infoT <- readr::read_csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/TICInfoTheory2.csv"),progress = F,col_types = cols())
  x <- 1:nrow(infoT)
  y <- c(infoT[,1])
  d<-data.frame(x=x,y=y)
  logEstimate <- lm(unlist(y)~log(x),data=d)
  ent_log_interc <- logEstimate$coefficients[1]
  ent_log_sl<- logEstimate$coefficients[2]
  y <- infoT[,2]
  d<-data.frame(x=x,y=y)
  logEstimate <- lm(unlist(y)~log(x),data=d)
  piel_log_interc <- logEstimate$coefficients[1]
  piel_log_sl<- logEstimate$coefficients[2]
  
  # dist from prime numver counting
  plot_data <- data.frame(p_count=p_count)
  plot_data$p_count2 <- c(Inf,p_count2)
  plot_data$idx <- c(1:nrow(plot_data))
  plot_data$ShWiener <- 0
  plot_data$Pielou <- 1
  plot_data$np <- c(0,diff(np$np))
  plot_data$np_orig <- c(np$np)
  
  plot_data$ShWiener <- c(0,infoT$ShannonWiener[np$np[-1]])
  plot_data$Pielou <- c(1,infoT$Pielou[np$np[-1]])
  
  x <- 1:nrow(plot_data)
  y <- plot_data$ShWiener
  d<-data.frame(x,y)
  logEstimate <- lm(y~log(x),data=d)
  ent_log_interc_prime_count <- logEstimate$coefficients[1]
  ent_log_sl_prime_count<- logEstimate$coefficients[2]
  
  pcount_dist_wiener <- RMSE(fitted(logEstimate), p_count)#mean(abs(fitted(logEstimate)-p_count))
  pcount2_dist_wiener <- RMSE(fitted(logEstimate)[-c(1:2)], p_count2[-1])#mean(abs(fitted(logEstimate)[-c(1:2)]-p_count2[-1]))
  
  y <- plot_data$Pielou
  d<-data.frame(x,y)
  logEstimate <- lm(y~log(x),data=d)
  piel_log_interc_prime_count <- logEstimate$coefficients[1]
  piel_log_sl_prime_count <- logEstimate$coefficients[2]
  
  pcount_dist_piel <- RMSE(fitted(logEstimate), p_count)
  pcount2_dist_piel <- RMSE(fitted(logEstimate)[-c(1:2)], p_count2[-1])
  
  coords <- read.csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/TICCoordinates.csv"),stringsAsFactors = F)
  max_spec <- max(coords$specificity)
  mean_spec <- mean(coords$specificity)
  max_div <- max(coords$diversity)
  mean_div <- mean(coords$diversity)
  
  #convert to random walk like time series
  specSeries <- cumsum(coords$specificity-mean(coords$specificity))
  specSeriesOrig <- coords$specificity
  #specSeries <- coords$specificity
  #specSeries <- log(coords$specificity)
  divSeries <- cumsum(coords$diversity-mean(coords$diversity))
  divSeriesOrig <- coords$diversity
  #divSeries <- coords$diversity
  #divSeries <- log(coords$diversity)
  #
  eDimSpec <- estimateEmbeddingDim(specSeries)
  eDimDiv <- estimateEmbeddingDim(divSeries)
  
  #corrDimSpec <- nonlinearTseries::corrDim(specSeries,min.embedding.dim=3,max.embedding.dim = eDimSpec, min.radius = 1, max.radius = 50, do.plot = F)
  #corrDimSpec <- estimate(corrDimSpec,regression.range=c(1,50000))
  #corrDimDiv <- nonlinearTseries::corrDim(divSeries,min.embedding.dim=3,max.embedding.dim = eDimDiv, min.radius = 1, max.radius = 50, do.plot = F)
  #corrDimDiv <- estimate(corrDimDiv,regression.range=c(1,50000))
  #infoDimSpec <- fractal::infoDim(specSeries,eDimSpec)
  #infoDimDiv <- fractal::infoDim(divSeries,eDimDiv)
  
  hurstSpec <- fractal::hurstSpec(specSeries,method = "smoothed",sdf.method = "multitaper")
  hurstCoeffSpec <- as.numeric(hurstSpec)
  #z <- fractal::lyapunov(cumsum(coords$specificity-mean(coords$specificity)))
  #z <- fractal::lyapunov(specSeries)
  #lapSpec <- max(c(unlist(z[[1]]),unlist(z[[2]]),unlist(z[[3]])))
  ml <- maxLyapunov(specSeries,radius = 50,min.embedding.dim = 3,max.embedding.dim = eDimSpec, max.time.steps = 5000)
  ml.est <- estimate(ml,do.plot=F,fit.lty=1,fit.lwd=5)
  lapSpec <- ml.est
  #corrDimSpec <- fractal::corrDim(cumsum(coords$specificity-mean(coords$specificity)))
  
  #hurstSpec <- fractal::hurstSpec()
  hurstSpec <- fractal::hurstSpec(divSeries,method = "smoothed",sdf.method = "multitaper")
  hurstCoeffDiv <- as.numeric(hurstSpec)
  #z <- fractal::lyapunov(cumsum(coords$diversity-mean(coords$diversity)))
  #z <- fractal::lyapunov(divSeries)
  #lapDiv <- max(c(unlist(z[[1]]),unlist(z[[2]]),unlist(z[[3]])))
  ml <- maxLyapunov(divSeries,radius = 50,min.embedding.dim = 3,max.embedding.dim = eDimDiv, max.time.steps = 5000)
  ml.est <- estimate(ml,do.plot=F,fit.lty=1,fit.lwd=5)
  lapDiv <- ml.est
  
  rqa.analysis=rqa(time.series = specSeries,radius = 50,time.lag = 1)
  recPercSpec <- rqa.analysis$REC
  recDetSpec <- rqa.analysis$DET
  recLamSpec <- rqa.analysis$LAM
  recRatioSpec <- rqa.analysis$RATIO
  recLmaxSpec <- rqa.analysis$Lmax
  recLmeanSpec <- rqa.analysis$Lmean
  recVmaxSpec <- rqa.analysis$Vmax
  recVmeanSpec <- rqa.analysis$Vmean
  recEntropySpec <- rqa.analysis$ENTR
  recTrendSpec <- rqa.analysis$TREND
  recRateSpecMin <- min(rqa.analysis$recurrenceRate)
  recRateSpecMax <- max(rqa.analysis$recurrenceRate)
  recRateSpecMean <- mean(rqa.analysis$recurrenceRate)
  recRateSpecMedian <- median(rqa.analysis$recurrenceRate)
  recRateSpecSd <- sd(rqa.analysis$recurrenceRate)
  rqa.analysis=rqa(time.series = divSeries,radius = 50,time.lag = 1)
  recPercDiv <- rqa.analysis$REC
  recDetDiv <- rqa.analysis$DET
  recLamDiv <- rqa.analysis$LAM
  recRatioDiv <- rqa.analysis$RATIO
  recLmaxDiv <- rqa.analysis$Lmax
  recLmeanDiv <- rqa.analysis$Lmean
  recVmaxDiv <- rqa.analysis$Vmax
  recVmeanDiv <- rqa.analysis$Vmean
  recEntropyDiv <- rqa.analysis$ENTR
  recTrendDiv <- rqa.analysis$TREND
  recRateDivMin <- min(rqa.analysis$recurrenceRate)
  recRateDivMax <- max(rqa.analysis$recurrenceRate)
  recRateDivMean <- mean(rqa.analysis$recurrenceRate)
  recRateDivMedian <- median(rqa.analysis$recurrenceRate)
  recRateDivSd <- sd(rqa.analysis$recurrenceRate)
  
  setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")
  
  meta <- read_csv(paste0(gsub("\\-eigenvals\\.csv","",fils[i]),"-eigenMeta.csv"),progress = F,col_types = cols())
  evals <- read_csv(fils[i],progress = F,col_types = cols())
  colnames(evals) <- c("x")
  
  comProbs <- apply(setProbs,1,function(x){
    tokens <- unlist(strsplit(as.character(x[1]),", "))
    product <- c()
    lowToken <- list(NULL,NULL)
    highToken <- list(NULL,NULL)
    for(i in tokens){
      if(is.null(lowToken[[2]])){
        lowToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
        lowToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
      } else if(lowToken[[2]] > tokenProbs[which(tokenProbs$x==i),3]){
        lowToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
        lowToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
      }
      
      if(is.null(highToken[[2]])){
        highToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
        highToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
      } else if(highToken[[2]] < tokenProbs[which(tokenProbs$x==i),3]){
        highToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
        highToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
      }
      
      product <- c(product,tokenProbs[which(tokenProbs$x==i),3])
    }
    res <- list(prod(product),lowToken[[1]],highToken[[1]])
    res
  })
  
  setProbs$tokenprobs <- sapply(comProbs,function(x){
    x[[1]]
  })
  
  setProbs$lowToken <- sapply(comProbs,function(x){
    x[[2]]
  })
  
  setProbs$highToken <- sapply(comProbs,function(x){
    x[[3]]
  })
  
  setProbs$overallprob <- setProbs$tokenprobs * setProbs$prob
  write_csv(setProbs,paste0(gsub("\\-eigenvals\\.csv","",fils[i]),"-setProbs.csv"))
  write_csv(plot_data,paste0(gsub("\\-eigenvals\\.csv","",fils[i]),"-primeCountInfoTheory.csv"))
  # plot(scale(setProbs$prob)[,1],c(1:length(setProbs$prob)))
  # hist(scale(setProbs$prob)[,1])
  # plot(ecdf(scale(setProbs$prob)[,1]))
  
  # #powerlaw
  
  # m_bl = poweRlaw::conpl$new(setProbs$prob)
  # est = estimate_xmin(m_bl)
  # m_bl$setXmin(est)
  # m_bl$setPars(estimate_pars(m_bl))
  # print(paste0(fils[i]," alpha: ",m_bl$pars," xMin: ",m_bl$xmin))
  # #lognormal
  # m_ln = conlnorm$new(setProbs$prob)
  # est = estimate_xmin(m_ln)
  # m_ln$setXmin(est)
  # m_ln$setPars(estimate_pars(m_ln))
  # # #poissoin
  # m_exp = conexp$new(setProbs$prob)
  # est = estimate_xmin(m_exp)
  # m_exp$setXmin(est)
  # m_exp$setPars(estimate_pars(m_exp))
  # # 
  # # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
  # plot(m_bl, ylab="CDF",pch=20)
  # text(0.00005,0.01,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
  # lines(m_bl, col="red")
  # lines(m_ln, col="green")
  # lines(m_exp, col="blue")
  
  # m = conpl$new(setProbs$prob)
  # est = estimate_xmin(m)
  # m$setXmin(est)
  # plot(setProbs$prob, dist_pdf(m, setProbs$prob), type="b")
  # plot(setProbs$prob, dist_cdf(m, setProbs$prob), type="b")
  
  # bs = bootstrap(m_bl, no_of_sims=100, threads=2)
  # sd(bs$bootstraps[,2])
  # sd(bs$bootstraps[,3])
  # plot(bs, trim=0.1)
  # hist(bs$bootstraps[,2], breaks="fd")
  # hist(bs$bootstraps[,3], breaks="fd")
  # 
  # plot(bs$bootstraps[,2], bs$bootstraps[,3])
  # 
  # bs_p = bootstrap_p(m_bl)
  # plot(bs_p)
  # 
  # comp = compare_distributions(m_bl, m_ln)
  # plot(comp)
  # #dev.off()
  
  nextR <- get(paste0("R",i))
  m_bl = poweRlaw::conpl$new(diff(rev(nextR$`eValsB$values`)))
  est = estimate_xmin(m_bl)
  m_bl$setXmin(est)
  m_bl$setPars(estimate_pars(m_bl))
  print(paste0(fils[i]," PowerLaw alpha: ",m_bl$pars," xMin: ",m_bl$xmin))
  # #lognormal
  m_ln = conlnorm$new(diff(rev(nextR$`eValsB$values`)))
  est = estimate_xmin(m_ln)
  m_ln$setXmin(est)
  m_ln$setPars(estimate_pars(m_ln))
  print(paste0(fils[i]," LogNormal mu: ",m_ln$pars[1]," sigma: ",m_ln$pars[2]," xMin: ",m_ln$xmin))
  # #poissoin
  # m_exp = conexp$new(diff(rev(nextR$`eValsB$values`)))
  # est = estimate_xmin(m_exp)
  # m_exp$setXmin(est)
  # m_exp$setPars(estimate_pars(m_exp))
  #
  # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
  # plot(m_bl, ylab="CDF",pch=20)
  # text(0.00005,0.01,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
  # lines(m_bl, col="red")
  # lines(m_ln, col="green")
  # lines(m_exp, col="blue")
  #plot(ecdf(diff(orig.evals$`eValsA$values`) - diff(nextR$`eValsB$values`)),main=paste0("R",i))
  #
  
  resData <- rbind(resData,
                   data.frame(runData=runData,
                              runType=runType,
                              runId=runId,
                              pLawAlpha=m_bl$pars,
                              pLawXMin=m_bl$xmin,
                              logNormMu=m_ln$pars[1],
                              logNormSigma=m_ln$pars[2],
                              logNormXMin=m_ln$xmin,
                              conditionNumber=meta$y[1],
                              smallestEV=meta$y[3],
                              largestEV=evals$x[1],
                              lapSpec=lapSpec,
                              hurstCoeffSpec=hurstCoeffSpec,
                              lapDiv=lapDiv,
                              hurstCoeffDiv=hurstCoeffDiv,
                              eDimSpec=eDimSpec,
                              eDimDiv=eDimDiv,
                              #corrDimSpec=corrDimSpec,
                              #corrDimDiv=corrDimDiv,
                              recPercSpec=recPercSpec,
                              recDetSpec=recDetSpec,
                              recLamSpec=recLamSpec,
                              recRatioSpec=recRatioSpec,
                              recLmaxSpec=recLmaxSpec,
                              recLmeanSpec=recLmeanSpec,
                              recVmaxSpec=recVmaxSpec,
                              recVmeanSpec=recVmeanSpec,
                              recEntropySpec=recEntropySpec,
                              recTrendSpec=recTrendSpec,
                              recRateSpecMin=recRateSpecMin,
                              recRateSpecMax=recRateSpecMax,
                              recRateSpecMean=recRateSpecMean,
                              recRateSpecMedian=recRateSpecMedian,
                              recRateSpecSd=recRateSpecSd,
                              recPercDiv=recPercDiv,
                              recDetDiv=recDetDiv,
                              recLamDiv=recLamDiv,
                              recRatioDiv=recRatioDiv,
                              recLmaxDiv=recLmaxDiv,
                              recLmeanDiv=recLmeanDiv,
                              recVmaxDiv=recVmaxDiv,
                              recVmeanDiv=recVmeanDiv,
                              recEntropyDiv=recEntropyDiv,
                              recTrendDiv=recTrendDiv,
                              recRateDivMin=recRateDivMin,
                              recRateDivMax=recRateDivMax,
                              recRateDivMean=recRateDivMean,
                              recRateDivMedian=recRateDivMedian,
                              recRateDivSd=recRateDivSd,
                              ent_log_interc=ent_log_interc,
                              ent_log_sl=ent_log_sl,
                              piel_log_interc=piel_log_interc,
                              piel_log_sl=piel_log_sl,
                              ent_log_interc_prime_count=ent_log_interc_prime_count,
                              ent_log_sl_prime_count=ent_log_sl_prime_count,
                              piel_log_interc_prime_count=piel_log_interc_prime_count,
                              piel_log_sl_prime_count=piel_log_sl_prime_count,
                              pcount_dist_wiener=pcount_dist_wiener,
                              pcount2_dist_wiener=pcount2_dist_wiener,
                              pcount_dist_piel=pcount_dist_piel,
                              pcount2_dist_piel=pcount2_dist_piel,
                              max_spec=max_spec,
                              max_div=max_div,
                              mean_spec=mean_spec,
                              mean_div=mean_div)
  )
}

write_csv(resData,"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/resData.csv")



# Step 6 - result data generation for original data before pertubation -----
library(tidyverse)
library(poweRlaw)
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
library(akima)
library(caret)
library(BEST)
library(mgcv)
options(scipen = 100000)

set.seed(3011)
RNGkind("L'Ecuyer-CMRG")

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}


setwd("/home/STAFF/luczakma/researchdata2/fair/transcendental-information-cascades/input/2019-11-15-letter-to-n-study/")
primes50k <- read_delim("data/primes50k.txt", 
                        ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
np <- read_csv("data/prime_counts.txt")
#np<-as.vector(np$np)

nmax<-max(primes50k$X2)
primelist <- primes50k$X2

eq = function(x,y){(y)  / (x/(log(x)))}
p_count <- eq(x=c(1:max(primes50k$X2)),y=np)
p_count <- unlist(p_count)

eq2 = function(x,y){
  (y) / (integrate(function(t) 1/log(t),lower=2,upper=x, rel.tol=1e-5)$value)
}
# p_count2 <- c()
# for(t in 2:max(primes50k$X2)){
#   p_count2 <- c(p_count2,eq2(x=t,y=np[t,1]))
# }
# p_count2 <- unlist(p_count2)
p_count2 <- read_csv("data/p_count2.csv")
p_count2 <- c(p_count2$p_count2)

setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")


fils <- list.files(".",full.names = F, pattern = "-eigenvals.csv")
origs <- which(fils == "primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv" |
                 fils == "random50k-discrete-tokenised-2020-01-28-10-32-09-eigenvals.csv" |
                 fils == "randomprimes50k-discrete-tokenised-2020-01-28-11-16-26-eigenvals.csv" |
                 fils == "primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43-eigenvals.csv" |
                 fils == "primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenvals.csv" |
                 fils == "primesrandomorderC50k-discrete-tokenised-2020-01-28-08-39-47-eigenvals.csv")
fils <- fils[origs]

primeseVals <- matrix(nrow = 200,ncol = 40)
primesRandeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrdereVals <- matrix(nrow = 200,ncol = 40)
primesRandOrderBeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrderCeVals <- matrix(nrow = 200,ncol = 40)
randeVals <- matrix(nrow = 200,ncol = 40)

for(fil in 1:length(fils)){
  varName <- unlist(strsplit(fils[fil],"-"))[1]
  assign(paste0("ORIG",fil),
         read_csv(paste0(fils[fil])))
}


resData <- data.frame(runData=character(0),
                      runType=character(0),
                      runId=numeric(0),
                      pLawAlpha=numeric(0),
                      pLawXMin=numeric(0),
                      logNormMu=numeric(0),
                      logNormSigma=numeric(0),
                      logNormXMin=numeric(0),
                      conditionNumber=numeric(0),
                      smallestEV=numeric(0),
                      largestEV=numeric(0),
                      lapSpec=numeric(0),
                      hurstCoeffSpec=numeric(0),
                      lapDiv=numeric(0),
                      hurstCoeffDiv=numeric(0),
                      eDimSpec=numeric(0),
                      eDimDiv=numeric(0),
                      #corrDimSpec=numeric(0),
                      #corrDimDiv=numeric(0),
                      recPercSpec=numeric(0),
                      recDetSpec=numeric(0),
                      recLamSpec=numeric(0),
                      recRatioSpec=numeric(0),
                      recLmaxSpec=numeric(0),
                      recLmeanSpec=numeric(0),
                      recVmaxSpec=numeric(0),
                      recVmeanSpec=numeric(0),
                      recEntropySpec=numeric(0),
                      recTrendSpec=numeric(0),
                      recRateSpecMin=numeric(0),
                      recRateSpecMax=numeric(0),
                      recRateSpecMean=numeric(0),
                      recRateSpecMedian=numeric(0),
                      recRateSpecSd=numeric(0),
                      recPercDiv=numeric(0),
                      recDetDiv=numeric(0),
                      recLamDiv=numeric(0),
                      recRatioDiv=numeric(0),
                      recLmaxDiv=numeric(0),
                      recLmeanDiv=numeric(0),
                      recVmaxDiv=numeric(0),
                      recVmeanDiv=numeric(0),
                      recEntropyDiv=numeric(0),
                      recTrendDiv=numeric(0),
                      recRateDivMin=numeric(0),
                      recRateDivMax=numeric(0),
                      recRateDivMean=numeric(0),
                      recRateDivMedian=numeric(0),
                      recRateDivSd=numeric(0),
                      ent_log_interc=numeric(0),
                      ent_log_sl=numeric(0),
                      piel_log_interc=numeric(0),
                      piel_log_sl=numeric(0),
                      ent_log_interc_prime_count=numeric(0),
                      ent_log_sl_prime_count=numeric(0),
                      piel_log_interc_prime_count=numeric(0),
                      piel_log_sl_prime_count=numeric(0),
                      pcount_dist_wiener=numeric(0),
                      pcount2_dist_wiener=numeric(0),
                      pcount_dist_piel=numeric(0),
                      pcount2_dist_piel=numeric(0),
                      max_spec=numeric(0),
                      max_div=numeric(0),
                      mean_spec=numeric(0),
                      mean_div=numeric(0))



for(i in 1:length(fils)){
  # compute the set probability
  
  runId <- 0
  runType <- "orig"
  runData <- unlist(strsplit(fils[i],"-"))[1]
  
  setwd("/home/STAFF/luczakma/researchdata2/fair/transcendental-information-cascades/")
  links <- readr::read_csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/links.csv"),progress = F,col_types = cols())
  tokenProbs <- plyr::count(as.character(links$token))
  tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)
  
  
  nodes <- readr::read_csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/nodes.csv"),progress = F,col_types = cols())
  nodes <- nodes[which(nodes$tokens != ""),]
  setProbs <- plyr::count(nodes$tokens)
  setProbs$prob <- as.numeric(setProbs$freq)/nrow(nodes)
  distr <- plyr::count(setProbs$freq)
  plot(distr,type='l')
  
  infoT <- readr::read_csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/TICInfoTheory2.csv"),progress = F,col_types = cols())
  x <- 1:nrow(infoT)
  y <- infoT[,1]
  d<-data.frame(x=x,y=y)
  logEstimate <- lm(unlist(y)~log(x),data=d)
  ent_log_interc <- logEstimate$coefficients[1]
  ent_log_sl<- logEstimate$coefficients[2]
  y <- infoT[,2]
  d<-data.frame(x=x,y=y)
  logEstimate <- lm(unlist(y)~log(x),data=d)
  piel_log_interc <- logEstimate$coefficients[1]
  piel_log_sl<- logEstimate$coefficients[2]
  
  # dist from prime numver counting
  plot_data <- as.data.frame(p_count)
  plot_data$p_count2 <- c(Inf,p_count2)
  plot_data$idx <- c(1:nrow(plot_data))
  plot_data$ShWiener <- 0
  plot_data$Pielou <- 1
  plot_data$np <- c(0,diff(np$np))
  plot_data$np_orig <- c(np$np)
  
  plot_data$ShWiener <- c(0,infoT$ShannonWiener[np$np[-1]])
  plot_data$Pielou <- c(1,infoT$Pielou[np$np[-1]])
  
  x <- 1:nrow(plot_data)
  y <- plot_data$ShWiener
  d<-data.frame(x,y)
  logEstimate <- lm(y~log(x),data=d)
  ent_log_interc_prime_count <- logEstimate$coefficients[1]
  ent_log_sl_prime_count<- logEstimate$coefficients[2]
  
  pcount_dist_wiener <- RMSE(fitted(logEstimate), p_count)#mean(abs(fitted(logEstimate)-p_count))
  pcount2_dist_wiener <- RMSE(fitted(logEstimate)[-c(1:2)], p_count2[-1])#mean(abs(fitted(logEstimate)[-c(1:2)]-p_count2[-1]))
  
  y <- plot_data$Pielou
  d<-data.frame(x,y)
  logEstimate <- lm(y~log(x),data=d)
  piel_log_interc_prime_count <- logEstimate$coefficients[1]
  piel_log_sl_prime_count <- logEstimate$coefficients[2]
  
  pcount_dist_piel <- RMSE(fitted(logEstimate), p_count)
  pcount2_dist_piel <- RMSE(fitted(logEstimate)[-c(1:2)], p_count2[-1])
  
  coords <- read.csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/TICCoordinates.csv"),stringsAsFactors = F)
  max_spec <- max(coords$specificity)
  mean_spec <- mean(coords$specificity)
  max_div <- max(coords$diversity)
  mean_div <- mean(coords$diversity)
  
  #convert to random walk like time series
  specSeries <- cumsum(coords$specificity-mean(coords$specificity))
  specSeriesOrig <- coords$specificity
  #specSeries <- coords$specificity
  #specSeries <- log(coords$specificity)
  divSeries <- cumsum(coords$diversity-mean(coords$diversity))
  divSeriesOrig <- coords$diversity
  #divSeries <- coords$diversity
  #divSeries <- log(coords$diversity)
  
  eDimSpec <- estimateEmbeddingDim(specSeries)
  eDimDiv <- estimateEmbeddingDim(divSeries)
  
  #corrDimSpec <- nonlinearTseries::corrDim(specSeries,min.embedding.dim=3,max.embedding.dim = eDimSpec, min.radius = 1, max.radius = 50, do.plot = F)
  #corrDimSpec <- estimate(corrDimSpec,regression.range=c(1,50000))
  #corrDimDiv <- nonlinearTseries::corrDim(divSeries,min.embedding.dim=3,max.embedding.dim = eDimDiv, min.radius = 1, max.radius = 50, do.plot = F)
  #corrDimDiv <- estimate(corrDimDiv,regression.range=c(1,50000))
  #infoDimSpec <- fractal::infoDim(specSeries,eDimSpec)
  #infoDimDiv <- fractal::infoDim(divSeries,eDimDiv)
  
  hurstSpec <- fractal::hurstSpec(specSeries,method = "smoothed",sdf.method = "multitaper")
  hurstCoeffSpec <- as.numeric(hurstSpec)
  #z <- fractal::lyapunov(cumsum(coords$specificity-mean(coords$specificity)))
  #z <- fractal::lyapunov(specSeries)
  #lapSpec <- max(c(unlist(z[[1]]),unlist(z[[2]]),unlist(z[[3]])))
  ml <- maxLyapunov(specSeries,radius = 50,min.embedding.dim = 3,max.embedding.dim = eDimSpec, max.time.steps = 5000)
  ml.est <- estimate(ml,do.plot=F,fit.lty=1,fit.lwd=5)
  lapSpec <- ml.est
  #corrDimSpec <- fractal::corrDim(cumsum(coords$specificity-mean(coords$specificity)))
  
  #hurstSpec <- fractal::hurstSpec()
  hurstSpec <- fractal::hurstSpec(divSeries,method = "smoothed",sdf.method = "multitaper")
  hurstCoeffDiv <- as.numeric(hurstSpec)
  #z <- fractal::lyapunov(cumsum(coords$diversity-mean(coords$diversity)))
  #z <- fractal::lyapunov(divSeries)
  #lapDiv <- max(c(unlist(z[[1]]),unlist(z[[2]]),unlist(z[[3]])))
  ml <- maxLyapunov(divSeries,radius = 50,min.embedding.dim = 3,max.embedding.dim = eDimDiv, max.time.steps = 5000)
  ml.est <- estimate(ml,do.plot=F,fit.lty=1,fit.lwd=5)
  lapDiv <- ml.est
  
  rqa.analysis=rqa(time.series = specSeries,radius = 50,time.lag = 1)
  recPercSpec <- rqa.analysis$REC
  recDetSpec <- rqa.analysis$DET
  recLamSpec <- rqa.analysis$LAM
  recRatioSpec <- rqa.analysis$RATIO
  recLmaxSpec <- rqa.analysis$Lmax
  recLmeanSpec <- rqa.analysis$Lmean
  recVmaxSpec <- rqa.analysis$Vmax
  recVmeanSpec <- rqa.analysis$Vmean
  recEntropySpec <- rqa.analysis$ENTR
  recTrendSpec <- rqa.analysis$TREND
  recRateSpecMin <- min(rqa.analysis$recurrenceRate)
  recRateSpecMax <- max(rqa.analysis$recurrenceRate)
  recRateSpecMean <- mean(rqa.analysis$recurrenceRate)
  recRateSpecMedian <- median(rqa.analysis$recurrenceRate)
  recRateSpecSd <- sd(rqa.analysis$recurrenceRate)
  rqa.analysis=rqa(time.series = divSeries,radius = 50,time.lag = 1)
  recPercDiv <- rqa.analysis$REC
  recDetDiv <- rqa.analysis$DET
  recLamDiv <- rqa.analysis$LAM
  recRatioDiv <- rqa.analysis$RATIO
  recLmaxDiv <- rqa.analysis$Lmax
  recLmeanDiv <- rqa.analysis$Lmean
  recVmaxDiv <- rqa.analysis$Vmax
  recVmeanDiv <- rqa.analysis$Vmean
  recEntropyDiv <- rqa.analysis$ENTR
  recTrendDiv <- rqa.analysis$TREND
  recRateDivMin <- min(rqa.analysis$recurrenceRate)
  recRateDivMax <- max(rqa.analysis$recurrenceRate)
  recRateDivMean <- mean(rqa.analysis$recurrenceRate)
  recRateDivMedian <- median(rqa.analysis$recurrenceRate)
  recRateDivSd <- sd(rqa.analysis$recurrenceRate)
  
  setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")
  
  meta <- read_csv(paste0(gsub("\\-eigenvals\\.csv","",fils[i]),"-eigenMeta.csv"),progress = F,col_types = cols())
  evals <- read_csv(fils[i],progress = F,col_types = cols())
  colnames(evals) <- c("x")
  
  nextR <- get(paste0("ORIG",i))
  colnames(nextR) <- c("eVals")
  m_bl = poweRlaw::conpl$new(diff(rev(nextR$eVals)))
  est = estimate_xmin(m_bl)
  m_bl$setXmin(est)
  m_bl$setPars(estimate_pars(m_bl))
  print(paste0(fils[i]," PowerLaw alpha: ",m_bl$pars," xMin: ",m_bl$xmin))
  # #lognormal
  m_ln = conlnorm$new(diff(rev(nextR$eVals)))
  est = estimate_xmin(m_ln)
  m_ln$setXmin(est)
  m_ln$setPars(estimate_pars(m_ln))
  print(paste0(fils[i]," LogNormal mu: ",m_ln$pars[1]," sigma: ",m_ln$pars[2]," xMin: ",m_ln$xmin))
  #
  write_csv(plot_data,paste0(gsub("\\-eigenvals\\.csv","",fils[i]),"-primeCountInfoTheory.csv"))
  resData <- rbind(resData,
                   data.frame(runData=runData,
                              runType=runType,
                              runId=runId,
                              pLawAlpha=m_bl$pars,
                              pLawXMin=m_bl$xmin,
                              logNormMu=m_ln$pars[1],
                              logNormSigma=m_ln$pars[2],
                              logNormXMin=m_ln$xmin,
                              conditionNumber=meta$y[1],
                              smallestEV=meta$y[3],
                              largestEV=evals$x[1],
                              lapSpec=lapSpec,
                              hurstCoeffSpec=hurstCoeffSpec,
                              lapDiv=lapDiv,
                              hurstCoeffDiv=hurstCoeffDiv,
                              eDimSpec=eDimSpec,
                              eDimDiv=eDimDiv,
                              #corrDimSpec=corrDimSpec,
                              #corrDimDiv=corrDimDiv,
                              recPercSpec=recPercSpec,
                              recDetSpec=recDetSpec,
                              recLamSpec=recLamSpec,
                              recRatioSpec=recRatioSpec,
                              recLmaxSpec=recLmaxSpec,
                              recLmeanSpec=recLmeanSpec,
                              recVmaxSpec=recVmaxSpec,
                              recVmeanSpec=recVmeanSpec,
                              recEntropySpec=recEntropySpec,
                              recTrendSpec=recTrendSpec,
                              recRateSpecMin=recRateSpecMin,
                              recRateSpecMax=recRateSpecMax,
                              recRateSpecMean=recRateSpecMean,
                              recRateSpecMedian=recRateSpecMedian,
                              recRateSpecSd=recRateSpecSd,
                              recPercDiv=recPercDiv,
                              recDetDiv=recDetDiv,
                              recLamDiv=recLamDiv,
                              recRatioDiv=recRatioDiv,
                              recLmaxDiv=recLmaxDiv,
                              recLmeanDiv=recLmeanDiv,
                              recVmaxDiv=recVmaxDiv,
                              recVmeanDiv=recVmeanDiv,
                              recEntropyDiv=recEntropyDiv,
                              recTrendDiv=recTrendDiv,
                              recRateDivMin=recRateDivMin,
                              recRateDivMax=recRateDivMax,
                              recRateDivMean=recRateDivMean,
                              recRateDivMedian=recRateDivMedian,
                              recRateDivSd=recRateDivSd,
                              ent_log_interc=ent_log_interc,
                              ent_log_sl=ent_log_sl,
                              piel_log_interc=piel_log_interc,
                              piel_log_sl=piel_log_sl,
                              ent_log_interc_prime_count=ent_log_interc_prime_count,
                              ent_log_sl_prime_count=ent_log_sl_prime_count,
                              piel_log_interc_prime_count=piel_log_interc_prime_count,
                              piel_log_sl_prime_count=piel_log_sl_prime_count,
                              pcount_dist_wiener=pcount_dist_wiener,
                              pcount2_dist_wiener=pcount2_dist_wiener,
                              pcount_dist_piel=pcount_dist_piel,
                              pcount2_dist_piel=pcount2_dist_piel,
                              max_spec=max_spec,
                              max_div=max_div,
                              mean_spec=mean_spec,
                              mean_div=mean_div)
  )
}

write_csv(resData,"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/origResData.csv")
