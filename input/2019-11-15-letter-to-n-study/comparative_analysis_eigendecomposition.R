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
options(scipen = 10000)

set.seed(3011)
RNGkind("L'Ecuyer-CMRG")


# preprocessing the result dataset ------

setwd("~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/")
setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")


fils <- list.files(".",full.names = F, pattern = "-eigenvals.csv")
fils <- fils[-which(fils == "primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")]
fils <- fils[-which(fils == "randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27-eigenvals.csv")]
fils <- fils[-which(fils == "primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenvals.csv")]

primeseVals <- matrix(nrow = 200,ncol = 40)
primesRandeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrdereVals <- matrix(nrow = 200,ncol = 40)
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
randEvalVar <- list()

for(k in 1:40){
  primesEvalVar[k] <- var(primes.orig.evals,primeseVals[,k])
}

primes.rand.evals <- read_csv("primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenvals.csv")
primes.rand.meta <- read_csv("primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenMeta.csv")

for(k in 1:40){
  primesRandEvalVar[k] <- var(primes.rand.evals,primesRandEvalVar[,k])
}

primes.randorder.evals <- read_csv("primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00-eigenvals.csv")
primes.randorder.meta <- read_csv("primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00-eigenMeta.csv")

for(k in 1:40){
  primesRandOrderEvalVar[k] <- var(primes.randorder.evals,primesRandOrderEvalVar[,k])
}

randnums.evals <- read_csv("randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27-eigenvals.csv")
randnums.meta <- read_csv("randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27-eigenMeta.csv")

for(k in 1:40){
  randEvalVar[k] <- var(randnums.evals,randeVals[,k])
}


# resdata creation ------
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
                      hurstCoeffSpec=numeric(0),
                      lapDiv=numeric(0),
                      hurstCoeffDiv=numeric(0),
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
  
  hurstSpec <- fractal::hurstSpec(cumsum(coords$specificity-mean(coords$specificity)),method = "smoothed",sdf.method = "multitaper")
  hurstCoeffSpec <- as.numeric(hurstSpec)
  z <- fractal::lyapunov(cumsum(coords$specificity-mean(coords$specificity)))
  lapSpec <- max(c(unlist(z[[1]]),unlist(z[[2]]),unlist(z[[3]])))
  #corrDimSpec <- fractal::corrDim(cumsum(coords$specificity-mean(coords$specificity)))
  
  hurstSpec <- fractal::hurstSpec(cumsum(coords$diversity-mean(coords$diversity)),method = "smoothed",sdf.method = "multitaper")
  hurstCoeffDiv <- as.numeric(hurstSpec)
  z <- fractal::lyapunov(cumsum(coords$diversity-mean(coords$diversity)))
  lapDiv <- max(c(unlist(z[[1]]),unlist(z[[2]]),unlist(z[[3]])))
  
  eDimSpec <- estimateEmbeddingDim(cumsum(coords$specificity-mean(coords$specificity)))
  eDimDiv <- estimateEmbeddingDim(cumsum(coords$diversity-mean(coords$diversity)))
  
  rqa.analysis=rqa(time.series = cumsum(coords$specificity-mean(coords$specificity)),radius = 3,time.lag = 1)
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
  rqa.analysis=rqa(time.series = cumsum(coords$diversity-mean(coords$diversity)),radius = 3,time.lag = 1)
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
                              hurstCoeffSpec=hurstCoeffSpec,
                              lapDiv=lapDiv,
                              hurstCoeffDiv=hurstCoeffDiv,
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

setProbs <- setProbs[order(-setProbs$prob),]

setProbsPrimes <- setProbs
setProbsPrimes <- setProbsPrimes[order(-setProbsPrimes$prob),]

links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimes <- tokenProbs


nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00/createTIC/nodes.csv"),progress = F,col_types = cols())
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


setProbsPrimesRandOrder <- setProbs
setProbsPrimesRandOrder <- setProbsPrimes[order(-setProbsPrimes$prob),]

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


links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primes50k-random-discrete-tokenised-2019-11-21-20-42-39/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimesRand <- tokenProbs

nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primes50k-random-discrete-tokenised-2019-11-21-20-42-39/createTIC/nodes.csv"),progress = F,col_types = cols())
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

setProbsPrimesRand <- setProbs
setProbsPrimesRand <- setProbsPrimesRand[order(-setProbsPrimesRand$prob),]


# analysisng the result dataset -----

primesTailProbs <- rev(tail(setProbsPrimes$prob,20))
primesHeadProbs <- rev(head(setProbsPrimes$prob,20))
randomTailProbs <- rev(tail(setProbsRandoms$prob,20))
randomHeadProbs <- rev(head(setProbsRandoms$prob,20))
randomPrimesTailProbs <- rev(tail(setProbsPrimesRand$prob,20))
randomPrimesHeadProbs <- rev(head(setProbsPrimesRand$prob,20))
randomOrderPrimesTailProbs <- rev(tail(setProbsPrimesRandOrder$prob,20))
randomOrderPrimesHeadProbs <- rev(head(setProbsPrimesRandOrder$prob,20))


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

randomPrimesResData <- resData[which(resData$runData=="primesrandom50k"),]
randomPrimesResData[which(randomPrimesResData$runType=="headRemove"),]$runId <- 41-randomPrimesResData[which(randomPrimesResData$runType=="headRemove"),]$runId
randomPrimesResData <- randomPrimesResData[order(randomPrimesResData$runId),]
randomPrimesResData$axisTicks <- 0
randomPrimesResData$axisTicks[1:20] <- randomPrimesResData$runId[1:20] - 21
randomPrimesResData$axisTicks[21:40] <- randomPrimesResData$runId[21:40] - 20

randomOrderPrimesResData <- resData[which(resData$runData=="primesrandomorder50k"),]
randomOrderPrimesResData[which(randomOrderPrimesResData$runType=="headRemove"),]$runId <- 41-randomOrderPrimesResData[which(randomOrderPrimesResData$runType=="headRemove"),]$runId
randomOrderPrimesResData <- randomOrderPrimesResData[order(randomOrderPrimesResData$runId),]
randomOrderPrimesResData$axisTicks <- 0
randomOrderPrimesResData$axisTicks[1:20] <- randomOrderPrimesResData$runId[1:20] - 21
randomOrderPrimesResData$axisTicks[21:40] <- randomOrderPrimesResData$runId[21:40] - 20

primesResData$setProb <- c(primesTailProbs,primesHeadProbs)
randomResData$setProb <- c(randomTailProbs,randomHeadProbs)
randomPrimesResData$setProb <- c(randomPrimesTailProbs,randomPrimesHeadProbs)
randomOrderPrimesResData$setProb <- c(randomOrderPrimesTailProbs,randomOrderPrimesHeadProbs)

primesResData$lr <- primesResData$largestEV/primesResData$setProb
randomResData$lr <- randomResData$largestEV/randomResData$setProb
randomPrimesResData$lr <- randomPrimesResData$largestEV/randomPrimesResData$setProb
randomOrderPrimesResData$lr <- randomOrderPrimesResData$largestEV/randomOrderPrimesResData$setProb

chart.Correlation(primesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Primes")
chart.Correlation(randomResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Random numbers")
chart.Correlation(randomPrimesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Random Primes")
chart.Correlation(randomOrderPrimesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42)],histogram = T, pch=19, main="Random Order Primes")

chart.Correlation(primesResData[which(primesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(primesResData[which(primesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

chart.Correlation(randomResData[which(randomResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(randomResData[which(randomResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

chart.Correlation(randomPrimesResData[which(randomPrimesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(randomPrimesResData[which(randomPrimesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

chart.Correlation(randomOrderPrimesResData[which(randomOrderPrimesResData$runType=="headRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)
chart.Correlation(randomOrderPrimesResData[which(randomOrderPrimesResData$runType=="tailRemove"),c(4,6,7,9,10,11,12,16,17,19,24,25,31,32,39,40)],histogram = T, pch=19)

resData <- rbind(primesResData,randomResData,randomPrimesResData,randomOrderPrimesResData)

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

ggpairs(primesResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42,50)],showStrips = T, title = "Primes")
ggpairs(randomResData[,c(4,6,7,9,10,11,12,13,14,15,18,19,20,21,26,27,33,34,35,36,41,42,50)],showStrips = T, title = "Random numbers")


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

# colorblind_pal()(4)

# compare logNormSigma
ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$logNormSigma), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","top 10", "top 1")) +
  geom_point() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
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
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
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
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
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
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
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
       aes(x=axisTicks,y=scale(resData$recPercSpec), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=min(scale(resData$recPercSpec))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=min(scale(resData$recPercSpec))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$recPercDiv), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=max(scale(resData$recPercDiv))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=max(scale(resData$recPercDiv))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

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
# 
# compare hurst Spec + Div
ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$hurstCoeffSpec), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=min(scale(resData$hurstCoeffSpec))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=min(scale(resData$hurstCoeffSpec))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

ggplot(resData, 
       aes(x=axisTicks,y=scale(resData$hurstCoeffDiv), color=runData)) +
  #geom_line() +
  geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#56B4E9", "primesrandom50k" = "#E69F00", "primesrandomorder50k" = "#009E73")) +
  theme_clean() +
  labs(x = "Identifier set probability rank", y = "Recurrence percentage (scaled)", colour="Dataset") +
  annotate(geom="text", x=-5.5, y=min(scale(resData$hurstCoeffDiv))*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  annotate(geom="text", x=6, y=min(scale(resData$hurstCoeffDiv))*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() +
  NULL

write_csv(resData,"~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/fullResData.csv")
