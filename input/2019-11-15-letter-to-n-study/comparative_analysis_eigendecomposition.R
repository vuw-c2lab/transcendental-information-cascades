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
options(scipen = 10000)

set.seed(3011)
RNGkind("L'Ecuyer-CMRG")


# preprocessing the result dataset ------
setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")

fils <- list.files(".",full.names = F, pattern = "-eigenvals.csv")
fils <- fils[-which(fils == "primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")]
for(fil in 1:length(fils)){
  assign(paste0("R",fil),
          read_csv(paste0(fils[fil])))
}

orig.evals <- read_csv("primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")

# 
# plot(orig.evals$`eValsA$values` - R1$`eValsB$values`,pch='.')
# 
# plot(orig.evals$`eValsA$values`,R1$`eValsB$values`,pch='.')
# cor(orig.evals$`eValsA$values`,R1$`eValsB$values`)
# 
# plot(diff(rev(orig.evals$`eValsA$values`)))
# plot(ecdf(diff(rev(orig.evals$`eValsA$values`))))
# plot(diff(rev(R1$`eValsB$values`)))
# plot(ecdf(diff(rev(R1$`eValsB$values`))))
# 
# plot(diff(rev(orig.evals$`eValsA$values`)) - diff(rev(R1$`eValsB$values`)))
# plot(diff(orig.evals$`eValsA$values`) - diff(R1$`eValsB$values`))
# plot(diff(orig.evals$`eValsA$values`) - diff(R1$`eValsB$values`),type='l')
# 
# # cdf of eigengap difference between original and pertubed TIC matrix
# rho <- diff(orig.evals$`eValsA$values`) - diff(R1$`eValsB$values`)


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
                      lapDiv=numeric(0),
                      eDimSpec=numeric(0),
                      eDimDiv=numeric(0),
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
                      recRateDivSd=numeric(0))

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
  
  coords <- read.csv(paste0("output/2019-11-15-letter-to-n-study/",gsub("\\-eigenvals\\.csv","",fils[i]),"/createTIC/TICCoordinates.csv"),stringsAsFactors = F)
  lapSpec <-  maxLyapunov(diff(head(coords$specificity,50000)),min.embedding.dim=2,
                                    max.embedding.dim=12,
                                    time.lag=1,
                                    radius=2,theiler.window=3,
                                    min.neighs=10,min.ref.points=5,
                                    max.time.steps=500,do.plot=F)
  if(length(as.numeric(names(which(rowSums(lapSpec$s.function) > 0 & !is.infinite(rowSums(lapSpec$s.function))))))>0){
    toTest <- as.numeric(names(which(rowSums(lapSpec$s.function) > 0 & !is.infinite(rowSums(lapSpec$s.function)))))
    if(length(toTest) ==1){
      if(toTest[1]==12) toTest <- c(11,12)
      else toTest <- c(toTest,toTest[1]+1)
    }
    lapSpec <- estimate(lapSpec,
                        regression.range = c(10,490),
                        use.embeddings=toTest,
                        do.plot = F)
  } else {
    lapSpec <- Inf
  }
  
  
  lapDiv <- maxLyapunov(diff(head(coords$diversity,50000)),min.embedding.dim=2,
                                    max.embedding.dim=12,
                                    time.lag=1,
                                    radius=2,theiler.window=3,
                                    min.neighs=10,min.ref.points=5,
                                    max.time.steps=500,do.plot=F)
  if(length(as.numeric(names(which(rowSums(lapDiv$s.function) > 0 & !is.infinite(rowSums(lapDiv$s.function))))))>0){
    toTest <- as.numeric(names(which(rowSums(lapDiv$s.function) > 0 & !is.infinite(rowSums(lapDiv$s.function)))))
    if(length(toTest) ==1){
      if(toTest[1]==12) toTest <- c(11,12)
      else toTest <- c(toTest,toTest[1]+1)
    }
    lapDiv <- estimate(lapDiv,regression.range = c(10,490),use.embeddings=toTest,do.plot = F)
  } else {
    lapDiv <- Inf
  }
  
  eDimSpec <- estimateEmbeddingDim(diff(head(coords$specificity,50000)))
  eDimDiv <- estimateEmbeddingDim(diff(head(coords$diversity,50000)))
  rqa.analysis=rqa(time.series = diff(head(coords$specificity,50000)),radius = 3,time.lag = 1)
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
  rqa.analysis=rqa(time.series = diff(head(coords$diversity,50000)),radius = 3,time.lag = 1)
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
  plot(ecdf(diff(orig.evals$`eValsA$values`) - diff(nextR$`eValsB$values`)),main=paste0("R",i))
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
                     lapDiv=lapDiv,
                     eDimSpec=eDimSpec,
                     eDimDiv=eDimDiv,
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
                     recRateDivSd=recRateDivSd)
  )
}

write_csv(resData,"/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/resData.csv")


# get the original set probabiliy for primes and random ----
links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimes <- tokenProbs


nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/nodes.csv"),progress = F,col_types = cols())
nodes <- nodes[which(nodes$tokens != ""),]
setProbs <- plyr::count(nodes$tokens)
setProbs$prob <- as.numeric(setProbs$freq)/nrow(nodes)
distr <- plyr::count(setProbs$freq)

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

setProbsPrimes <- setProbs
setProbsPrimes <- setProbsPrimes[order(-setProbsPrimes$prob),]

links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsRandoms <- tokenProbs

nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27/createTIC/nodes.csv"),progress = F,col_types = cols())
nodes <- nodes[which(nodes$tokens != ""),]
setProbs <- plyr::count(nodes$tokens)
setProbs$prob <- as.numeric(setProbs$freq)/nrow(nodes)
distr <- plyr::count(setProbs$freq)

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

setProbsRandoms <- setProbs
setProbsRandoms <- setProbsRandoms[order(-setProbsRandoms$prob),]

# analysisng the result dataset -----

primesTailProbs <- rev(tail(setProbsPrimes$prob,20))
primesHeadProbs <- rev(head(setProbsPrimes$prob,20))
randomTailProbs <- rev(tail(setProbsRandoms$prob,20))
randomHeadProbs <- rev(head(setProbsRandoms$prob,20))

resData <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/resData.csv")

resData$runIdentifier <- paste(resData$runData,resData$runType)

primesResData <- resData[which(resData$runData=="primes50k"),]
primesResData[which(primesResData$runType=="headRemove"),]$runId <- 41-primesResData[which(primesResData$runType=="headRemove"),]$runId
primesResData <- primesResData[order(primesResData$runId),]
primesResData$axisTicks <- 0
primesResData$axisTicks[1:20] <- primesResData$runId[1:20] - 21
primesResData$axisTicks[21:40] <- primesResData$runId[21:40] - 20

randomResData <- resData[which(resData$runData=="random50k"),]
randomResData[which(randomResData$runType=="headRemove"),]$runId <- 41-randomResData[which(randomResData$runType=="headRemove"),]$runId
randomResData <- randomResData[order(randomResData$runId),]
randomResData$axisTicks <- 0
randomResData$axisTicks[1:20] <- randomResData$runId[1:20] - 21
randomResData$axisTicks[21:40] <- randomResData$runId[21:40] - 20

primesResData$setProb <- c(primesTailProbs,primesHeadProbs)
randomResData$setProb <- c(randomTailProbs,randomHeadProbs)

chart.Correlation(primesResData[,c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],histogram = T, pch=19, main="Primes")
chart.Correlation(randomResData[,c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],histogram = T, pch=19, main="Random numbers")

chart.Correlation(primesResData[which(primesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],histogram = T, pch=19)
chart.Correlation(primesResData[which(primesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],histogram = T, pch=19)

chart.Correlation(randomResData[which(randomResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],histogram = T, pch=19)
chart.Correlation(randomResData[which(randomResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],histogram = T, pch=19)


resData <- rbind(primesResData,randomResData)

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

# check which features differentiate random from primes

summary(primesResData)
summary(randomResData)

x <- primesResData$logNormSigma
y <- randomResData$logNormSigma
cor(x,y)
cov(x,y)
mean(x)
mean(y)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

ggpairs(primesResData[,c(2,4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],showStrips = T, title = "Primes")
ggpairs(randomResData[,c(2,4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40,48)],showStrips = T, title = "Random numbers")


# plot all correlations between random and primes data
par(mfrow=c(3,6))
chart.Correlation(cbind(primesResData$logNormSigma,randomResData$logNormSigma))
chart.Correlation(cbind(primesResData$logNormMu,randomResData$logNormMu))
chart.Correlation(cbind(primesResData$pLawAlpha,randomResData$pLawAlpha))
chart.Correlation(cbind(primesResData$conditionNumber,randomResData$conditionNumber))
chart.Correlation(cbind(primesResData$smallestEV,randomResData$smallestEV))
chart.Correlation(cbind(primesResData$largestEV,randomResData$largestEV))
chart.Correlation(cbind(primesResData$lapSpec,randomResData$lapSpec))
chart.Correlation(cbind(primesResData$recPercSpec,randomResData$recPercSpec))
chart.Correlation(cbind(primesResData$recPercDiv,randomResData$recPercDiv))
chart.Correlation(cbind(primesResData$recDetSpec,randomResData$recDetSpec))
chart.Correlation(cbind(primesResData$recDetDiv,randomResData$recDetDiv))
chart.Correlation(cbind(primesResData$recLamSpec,randomResData$recLamSpec))
chart.Correlation(cbind(primesResData$recLamDiv,randomResData$recLamDiv))
chart.Correlation(cbind(primesResData$recRatioSpec,randomResData$recRatioSpec))
chart.Correlation(cbind(primesResData$recRatioDiv,randomResData$recRatioDiv))
chart.Correlation(cbind(primesResData$recVmeanSpec,randomResData$recVmeanSpec))
chart.Correlation(cbind(primesResData$recVmeanDiv,randomResData$recVmeanDiv))
chart.Correlation(cbind(primesResData$recLmeanSpec,randomResData$recLmeanSpec))
chart.Correlation(cbind(primesResData$recLmeanDiv,randomResData$recLmeanDiv))
chart.Correlation(cbind(primesResData$recEntropySpec,randomResData$recEntropySpec))
chart.Correlation(cbind(primesResData$recEntropyDiv,randomResData$recEntropyDiv))
chart.Correlation(cbind(primesResData$recTrendSpec,randomResData$recTrendSpec))
chart.Correlation(cbind(primesResData$recTrendDiv,randomResData$recTrendDiv))
chart.Correlation(cbind(primesResData$recRateSpecMean,randomResData$recRateSpecMean))
chart.Correlation(cbind(primesResData$recRateDivMean,randomResData$recRateDivMean))



# compare logNormSigma
ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$logNormSigma), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "black") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#E69F00")) +
  theme_clean() +
  scale_y_continuous(trans = 'asinh') +
  labs(x = "Identifier set probability rank", y = "Sigma (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(scale(resData$logNormSigma))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(scale(resData$logNormSigma))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(logNormSigma), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(logNormSigma), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

# compare logNormMu
# compare pLawAlpha
ggplot(resData, 
aes(x=axisTicks,y=scale(resData$pLawAlpha), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "black") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#E69F00")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Alpha (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(scale(resData$pLawAlpha))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(scale(resData$pLawAlpha))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(pLawAlpha), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(pLawAlpha), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL



# compare conditionNumber
ggplot(resData, 
       aes(x=axisTicks,y=resData$conditionNumber, color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "black") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#E69F00")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Condition number", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(resData$conditionNumber)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(resData$conditionNumber)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(conditionNumber), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(conditionNumber), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL



# compare lapSpec (most promising because most variance)
ggplot(resData, 
       aes(x=axisTicks,y=resData$lapSpec, color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "black") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#E69F00")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Lapyunov spectrum", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(resData$lapSpec)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(resData$lapSpec)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="tailRemove"), 
       aes(x=runData,y=scale(lapSpec), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

ggplot(filter(resData,runType=="headRemove"), 
       aes(x=runData,y=scale(lapSpec), color=runData)) +
  geom_boxplot() +
  theme_clean() +
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(method = "t.test") +
  scale_colour_colorblind() +
  NULL

# compare recPercSpec + Div
ggplot(resData, 
       aes(x=runId,y=scale(resData$recPercSpec), color=runIdentifier)) +
  geom_line() +
  geom_point() +
  theme_clean() +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=runId,y=scale(resData$recPercDiv), color=runIdentifier)) +
  geom_line() +
  geom_point() +
  theme_clean() +
  scale_colour_colorblind() +
  NULL
# compare recDetSpec + Div
# compare recLamSpec + Div
# compare recRatioSpec + Div
# compare recLmaxSpec
# compare recEntropySpec + Div
# compare recTrendSpec + Div