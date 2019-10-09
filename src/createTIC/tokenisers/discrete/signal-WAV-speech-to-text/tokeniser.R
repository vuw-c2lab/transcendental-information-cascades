# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores=1){
  #initialise google speech API
  library(googleLanguageR)
  #set auth json file
  gl_auth("~/Downloads/My First Project-44181eaedc00.json")
  
  # read source data and trasnform to text
  test_audio <- paste0(getwd(),"/data/",dataSource)
  res <- gl_speech(test_audio,sampleRateHertz = 44100)
  
  secs <- 5 # configure how long the distinct slices shall be (for multiplex analysis this must be identical to the signal slicing)
  
  sourceData <- res$timings
  sourceData$startTime <- as.numeric(gsub("s","",sourceData$startTime))
  sourceData$endTime <- as.numeric(gsub("s","",sourceData$endTime))
  
  maxTime <- ceiling(max(sourceData$endTime))
  if((maxTime %% secs) != 0) maxTime <- maxTime + (secs - (maxTime %% secs))
  
  slices <- maxTime / secs
  
  # loop over all slices to tokenise signal
  tokenised<-list()
  for(i in 1:slices){ 
    tokenised[[length(tokenised)+1]] <- tolower(paste(sourceData[which(sourceData$startTime>((i-1)*secs) & sourceData$startTime<=(i*secs)),3],collapse = ", "))
  }
  
  tokenised <- data.frame(x = c(1:length(tokenised)),
                          y = unlist(tokenised),
                          stringsAsFactors = FALSE)
  
  return(tokenised)
}

# SUPPORT FUNCTIONS -------------------------------------------------------


