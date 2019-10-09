# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores=1){
  #initialise google speech API
  library(googleLanguageR)
  #set auth json file
  gl_auth("~/Downloads/My First Project-44181eaedc00.json")
  
  # read source data and trasnform to text
  test_audio <- paste0(getwd(),"/data/",dataSource)
  res <- gl_speech(test_audio,sampleRateHertz = 44100)
  
  secs <- 2 # configure how long the distinct slices shall be (for multiplex analysis this must be identical to the signal slicing)
  
  sourceData <- res$timings
  
  slices <- floor(nrow(sourceData)/(sRate*secs))
  
  # power spectrum 
  #spec <- spectrum( cbind( sourceData$AF3[1:5000], sourceData$O1[1:5000] ),spans=c(5,7) )
  #plot( spec$freq, spec$coh )
  #mean(spec$coh)
  
  # cross correlation
  # ccf(sourceData$AF3[1:5000],sourceData$O1[1:5000])
  
  # loop over all slices to tokenise signal
  events<-list()
  specs<-list()
  for(i in 1:floor(nrow(sourceData)/(sRate*secs))){ #todo -> fix the slicing to make it generic based on sampling rate and signal length
    
    for(j in 1:ncol(sourceData)){
      ds1 <- sourceData[((i*sRate*secs)-((sRate*secs)-1)):(i*sRate*secs),j] # todo -> see above for generic slicing todo
      
      #for power spectra comparison use the power spectra of the slices as initialised below
      #wv<-Wave(ds1,samp.rate=sRate)
      #s1 <- wv@left
      #s1 <- s1 / 2^(wv@bit -1)
      #n <- length(s1)
      #p <- fft(s1)
      #nUniquePts <- ceiling((n+1)/2)
      #p <- p[1:nUniquePts] #select just the first half since the second half 
      #p <- abs(p)  #take the absolute value, or the magnitude 
      #p <- p / n #scale by the number of points so that
      #p <- p^2  # square it to get the power 
      #if (n %% 2 > 0){
      #  p[2:length(p)] <- p[2:length(p)]*2 # we've got odd number of points fft
      #} else {
      #  p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # we've got even number of points fft
      #}
      #freqArray <- (0:(nUniquePts-1)) * (wv@samp.rate / n) #  create the frequency array 
      #plot(freqArray/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)')
      #specs[[j]]<-10*log10(p)
      #old
      specs[[j]]<-round(stats::spectrum(ds1, plot=FALSE)$spec,2)
      
      #for raw signal comparison use the raw data as initialized below
      #specs[[j]]<-ds1
    }
    
    for(nChan in 1:ncol(sourceData)){
      if(nChan == 1){
        #for power spectra comparison use the power spectra of the slices as initialised below, old
        #events[[paste0(i)]] <- round(specs[[nChan]]$spec,1)
        
        #for raw signal comparison use the raw data as initialized below
        events[[paste0(i)]] <- specs[[nChan]]
      } else{
        #for power spectra comparison use the power spectra of the slices as initialised below, old
        #events[[paste0(i)]] <- cbind(events[[paste0(i)]],round(specs[[nChan]]$spec,1))
        
        #for raw signal comparison use the raw data as initialized below
        events[[paste0(i)]] <- cbind(events[[paste0(i)]],specs[[nChan]])
      }
    }
  }
  
  tokenised <- lapply(events,function(x){
    tok <- c()
    for(freq in 1:length(x)){
      tok <- c(tok,paste0(freq,"_",x[freq]))
    }
    paste(tok,collapse = ", ")
  })
  
  tokenised <- data.frame(x = c(1:length(tokenised)),
                          y = unlist(tokenised),
                          stringsAsFactors = FALSE)
  
  return(tokenised)
}

# SUPPORT FUNCTIONS -------------------------------------------------------


