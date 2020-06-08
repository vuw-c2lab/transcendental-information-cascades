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



# preprocessing for the result dataset ------

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

setwd("~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/")
#setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")


fils <- list.files(".",full.names = F, pattern = "-eigenvals.csv")
fils <- fils[-which(fils == "primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv")]
fils <- fils[-which(fils == "randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27-eigenvals.csv")]
fils <- fils[-which(fils == "primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenvals.csv")]
fils <- fils[-which(fils == "primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00-eigenvals.csv")]
fils <- fils[-which(fils == "primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenvals.csv")]

primeseVals <- matrix(nrow = 200,ncol = 40)
primesRandeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrdereVals <- matrix(nrow = 200,ncol = 40)
primesRandOrderBeVals <- matrix(nrow = 200,ncol = 40)
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
randEvalVar <- list()

for(k in 1:40){
  primesEvalVar[k] <- var(primes.orig.evals,primeseVals[,k])
}

primes.rand.evals <- read_csv("primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenvals.csv")
primes.rand.meta <- read_csv("primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenMeta.csv")

for(k in 1:40){
  primesRandEvalVar[k] <- var(primes.rand.evals,primesRandeVals[,k])
}

primes.randorder.evals <- read_csv("primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00-eigenvals.csv")
primes.randorder.meta <- read_csv("primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00-eigenMeta.csv")

for(k in 1:40){
  primesRandOrderEvalVar[k] <- var(primes.randorder.evals,primesRandOrdereVals[,k])
}

primes.randorderB.evals <- read_csv("primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenvals.csv")
primes.randorderB.meta <- read_csv("primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenMeta.csv")

for(k in 1:40){
  primesRandOrderBEvalVar[k] <- var(primes.randorderB.evals,primesRandOrderBeVals[,k])
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
                      corrDimSpec=numeric(0),
                      corrDimDiv=numeric(0),
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
  
  corrDimSpec <- nonlinearTseries::corrDim(specSeries,min.embedding.dim=3,max.embedding.dim = eDimSpec, min.radius = 1, max.radius = 50, do.plot = F)
  corrDimSpec <- estimate(corrDimSpec,regression.range=c(1,50000))
  corrDimDiv <- nonlinearTseries::corrDim(divSeries,min.embedding.dim=3,max.embedding.dim = eDimDiv, min.radius = 1, max.radius = 50, do.plot = F)
  corrDimDiv <- estimate(corrDimDiv,regression.range=c(1,50000))
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
                              corrDimSpec=corrDimSpec,
                              corrDimDiv=corrDimDiv,
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

resData <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/resData.csv")
# preprocessing for the original result dataset (i.e. non-disturbed data) ------

setwd("~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2019-12-letter-to-N-final-analysis/")
#setwd("/home/STAFF/luczakma/RProjects/2019-letter-to-nature-primes/figures/")


fils <- list.files(".",full.names = F, pattern = "-eigenvals.csv")
origs <- which(fils == "primes50k-discrete-tokenised-2019-11-21-17-54-58-eigenvals.csv" |
                 fils == "randomnumbers50k-discrete-tokenised-2019-11-21-21-16-27-eigenvals.csv" |
                 fils == "primes50k-random-discrete-tokenised-2019-11-21-20-42-39-eigenvals.csv" |
                 fils == "primesrandomorder50k-discrete-tokenised-2020-01-11-13-49-00-eigenvals.csv" |
                 fils == "primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34-eigenvals.csv")
fils <- fils[origs]

primeseVals <- matrix(nrow = 200,ncol = 40)
primesRandeVals <- matrix(nrow = 200,ncol = 40)
primesRandOrdereVals <- matrix(nrow = 200,ncol = 40)
primesRandOrderBeVals <- matrix(nrow = 200,ncol = 40)
randeVals <- matrix(nrow = 200,ncol = 40)

for(fil in 1:length(fils)){
  varName <- unlist(strsplit(fils[fil],"-"))[1]
  assign(paste0("ORIG",fil),
         read_csv(paste0(fils[fil])))
}


# original resdata creation ------

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
                      corrDimSpec=numeric(0),
                      corrDimDiv=numeric(0),
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
  if(unlist(strsplit(fils[i],"-"))[1] == "primes50k" & unlist(strsplit(fils[i],"-"))[2] == "random"){
    runData <- "primesrandom50k"
  } if(unlist(strsplit(fils[i],"-"))[1] == "randomnumbers50k"){
    runData <- "random50k"
  } else {
    runData <- unlist(strsplit(fils[i],"-"))[1]
  }
  
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
  
  corrDimSpec <- nonlinearTseries::corrDim(specSeries,min.embedding.dim=3,max.embedding.dim = eDimSpec, min.radius = 1, max.radius = 50, do.plot = F)
  corrDimSpec <- estimate(corrDimSpec,regression.range=c(1,50000))
  corrDimDiv <- nonlinearTseries::corrDim(divSeries,min.embedding.dim=3,max.embedding.dim = eDimDiv, min.radius = 1, max.radius = 50, do.plot = F)
  corrDimDiv <- estimate(corrDimDiv,regression.range=c(1,50000))
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
                              corrDimSpec=corrDimSpec,
                              corrDimDiv=corrDimDiv,
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

# get the original set probabiliy for primes and random ----
origResData <- read_csv("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/origResData.csv")
origResData$runIdentifier <- paste0(origResData$runData," orig")
origResData$axisTicks <- -21
origResData$setProb <- 0


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

links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimes <- tokenProbs


nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorder50k-discrete-tokenised-2020-01-28-11-21-43/createTIC/nodes.csv"),progress = F,col_types = cols())
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
setProbsPrimesRandOrder <- setProbsPrimesRandOrder[order(-setProbsPrimesRandOrder$prob),]



links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimes <- tokenProbs


nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorderB50k-discrete-tokenised-2020-01-22-20-37-34/createTIC/nodes.csv"),progress = F,col_types = cols())
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


setProbsPrimesRandOrderB <- setProbs
setProbsPrimesRandOrderB <- setProbsPrimesRandOrderB[order(-setProbsPrimesRandOrderB$prob),]



links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorderC50k-discrete-tokenised-2020-01-28-08-39-47/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimes <- tokenProbs


nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/primesrandomorderC50k-discrete-tokenised-2020-01-28-08-39-47/createTIC/nodes.csv"),progress = F,col_types = cols())
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


setProbsPrimesRandOrderC <- setProbs
setProbsPrimesRandOrderC <- setProbsPrimesRandOrderC[order(-setProbsPrimesRandOrderC$prob),]


links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/random50k-discrete-tokenised-2020-01-28-10-32-09/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsRandoms <- tokenProbs

nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/random50k-discrete-tokenised-2020-01-28-10-32-09/createTIC/nodes.csv"),progress = F,col_types = cols())
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


links <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/randomprimes50k-discrete-tokenised-2020-01-28-11-16-26/createTIC/links.csv"),progress = F,col_types = cols())
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)

tokenProbsPrimesRand <- tokenProbs

nodes <- readr::read_csv(paste0("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/randomprimes50k-discrete-tokenised-2020-01-28-11-16-26/createTIC/nodes.csv"),progress = F,col_types = cols())
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


# complete the res data (tick marks, probabilitiesetc.)------

# add the set probabilities
primesTailProbs <- rev(tail(setProbsPrimes$prob,20))
primesHeadProbs <- rev(head(setProbsPrimes$prob,20))
randomTailProbs <- rev(tail(setProbsRandoms$prob,20))
randomHeadProbs <- rev(head(setProbsRandoms$prob,20))
randomPrimesTailProbs <- rev(tail(setProbsPrimesRand$prob,20))
randomPrimesHeadProbs <- rev(head(setProbsPrimesRand$prob,20))
randomOrderPrimesTailProbs <- rev(tail(setProbsPrimesRandOrder$prob,20))
randomOrderPrimesHeadProbs <- rev(head(setProbsPrimesRandOrder$prob,20))
randomOrderBPrimesTailProbs <- rev(tail(setProbsPrimesRandOrderB$prob,20))
randomOrderBPrimesHeadProbs <- rev(head(setProbsPrimesRandOrderB$prob,20))
randomOrderCPrimesTailProbs <- rev(tail(setProbsPrimesRandOrderC$prob,20))
randomOrderCPrimesHeadProbs <- rev(head(setProbsPrimesRandOrderC$prob,20))

# create unique run id
resData$runIdentifier <- paste(resData$runData,resData$runType)

# subsets
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

randomPrimesResData <- resData[which(resData$runData=="randomprimes50k"),]
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

randomOrderBPrimesResData <- resData[which(resData$runData=="primesrandomorderB50k"),]
randomOrderBPrimesResData[which(randomOrderBPrimesResData$runType=="headRemove"),]$runId <- 41-randomOrderBPrimesResData[which(randomOrderBPrimesResData$runType=="headRemove"),]$runId
randomOrderBPrimesResData <- randomOrderBPrimesResData[order(randomOrderBPrimesResData$runId),]
randomOrderBPrimesResData$axisTicks <- 0
randomOrderBPrimesResData$axisTicks[1:20] <- randomOrderBPrimesResData$runId[1:20] - 21
randomOrderBPrimesResData$axisTicks[21:40] <- randomOrderBPrimesResData$runId[21:40] - 20

randomOrderCPrimesResData <- resData[which(resData$runData=="primesrandomorderC50k"),]
randomOrderCPrimesResData[which(randomOrderCPrimesResData$runType=="headRemove"),]$runId <- 41-randomOrderCPrimesResData[which(randomOrderCPrimesResData$runType=="headRemove"),]$runId
randomOrderCPrimesResData <- randomOrderCPrimesResData[order(randomOrderCPrimesResData$runId),]
randomOrderCPrimesResData$axisTicks <- 0
randomOrderCPrimesResData$axisTicks[1:20] <- randomOrderCPrimesResData$runId[1:20] - 21
randomOrderCPrimesResData$axisTicks[21:40] <- randomOrderCPrimesResData$runId[21:40] - 20

# fill probabilities
primesResData$setProb <- c(primesTailProbs,primesHeadProbs)
randomResData$setProb <- c(randomTailProbs,randomHeadProbs)
randomPrimesResData$setProb <- c(randomPrimesTailProbs,randomPrimesHeadProbs)
randomOrderPrimesResData$setProb <- c(randomOrderPrimesTailProbs,randomOrderPrimesHeadProbs)
randomOrderBPrimesResData$setProb <- c(randomOrderBPrimesTailProbs,randomOrderBPrimesHeadProbs)
randomOrderCPrimesResData$setProb <- c(randomOrderCPrimesTailProbs,randomOrderCPrimesHeadProbs)

# creating the LR metric (experimental) -----

primesResData$lr <- primesResData$largestEV/primesResData$setProb
randomResData$lr <- randomResData$largestEV/randomResData$setProb
randomPrimesResData$lr <- randomPrimesResData$largestEV/randomPrimesResData$setProb
randomOrderPrimesResData$lr <- randomOrderPrimesResData$largestEV/randomOrderPrimesResData$setProb
randomOrderBPrimesResData$lr <- randomOrderBPrimesResData$largestEV/randomOrderBPrimesResData$setProb
randomOrderCPrimesResData$lr <- randomOrderCPrimesResData$largestEV/randomOrderCPrimesResData$setProb

primesResData$lr2 <- primesResData$largestEV/primesResData$recTrendSpec
randomResData$lr2 <- randomResData$largestEV/randomResData$recTrendSpec
randomPrimesResData$lr2 <- randomPrimesResData$largestEV/randomPrimesResData$recTrendSpec
randomOrderPrimesResData$lr2 <- randomOrderPrimesResData$largestEV/randomOrderPrimesResData$recTrendSpec
randomOrderBPrimesResData$lr2 <- randomOrderBPrimesResData$largestEV/randomOrderBPrimesResData$recTrendSpec
randomOrderCPrimesResData$lr2 <- randomOrderCPrimesResData$largestEV/randomOrderCPrimesResData$recTrendSpec

origResData$lr <- Inf
origResData$lr2 <- origResData$largestEV/origResData$recTrendSpec

# bring it all back together into the result dataset -----
resData <- rbind(primesResData,randomResData,randomPrimesResData,randomOrderPrimesResData,randomOrderBPrimesResData,randomOrderCPrimesResData)

#resData <- rbind(resData,origResData)

# analysisng the result dataset -----



# for LR against prob
#GAM

# Build the model
model <- gam(lr2 ~ s(setProb), data = primesResData)
# Make predictions
predictions <- model %>% predict(primesResData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, primesResData$lr2),
  R2 = R2(predictions, primesResData$lr2)
)
dev.off()
#colorblind_pal()(5)
plo <- ggplot(resData, 
              aes(x=resData$setProb,y=resData$lr, color=runData)) + scale_x_continuous(trans='log10') +
  #geom_line() +
  #geom_vline(xintercept = 0, colour="grey", linetype="dashed") +
  #scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = c("bottom 1","bottom 10","","head 10", "head 1")) +
  geom_point() + 
  geom_label_repel(aes(label = resData$setProb, fill=resData$runData), color = 'white', size = 2.5, label.padding = unit(0.15, "lines"), point.padding = unit(0.35, "lines"),show.legend=F, segment.colour = "grey") +
  scale_fill_manual(values=c("primes50k" = "#000000", "random50k" = "#F0E442", "randomprimes50k" = "#0072B2", "primesrandomorder50k" = "#E69F00", "primesrandomorderB50k" = "#56B4E9", "primesrandomorderC50k" = "#009E73")) +
  #theme_clean() +
  labs(x = "Identifier set probability rank", y = "LR", colour="Dataset") +
  #annotate(geom="text", x=-5.5, y=min(resData$hurstCoeffDiv)*.8, label=TeX("$\\leftarrow$ tail removed"), color="grey") +
  #annotate(geom="text", x=6, y=min(resData$hurstCoeffDiv)*.8, label=TeX("head removed $\\rightarrow$"), color="grey") +
  scale_colour_colorblind() + stat_smooth(method = gam, formula = y ~ s(x)) +
  NULL
plo

# get model params (confidence interval, standard error)
ggplot_build(plo)$data[[2]]