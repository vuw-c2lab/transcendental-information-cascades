if (!require(wordVectors)) devtools::install_github("bmschmidt/wordVectors")
if (!require(lsa)) install.packages("lsa")

# MAIN FUNCTION -----------------------------------------------------------

# vector init
wordVec = read.vectors("data/w2v-novel450.bin") #english

# POS model download
#model <- udpipe::udpipe_download_model(language = "english")
#model <- udpipe::udpipe_load_model(file = model$file_model)

# or load from file 
model <- udpipe::udpipe_load_model(file = "data/english-ewt-ud-2.4-190531.udpipe") #english
#model <- udpipe::udpipe_load_model(file = "/Users/MLR/indonesian-gsd-ud-2.4-190531.udpipe") #bahasa
#model <- udpipe::udpipe_load_model(file = "/Users/MLR/portuguese-bosque-ud-2.4-190531.udpipe") #portuguese


tokenise <- function(dataSource,no_cores=1){
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/data/",dataSource), col_names = F)
  sourceData$X1 <- tolower(sourceData$X1)
  
  #tokenise
  collect <- lapply(sourceData$X1, function(xx){
    
    #annotate
    x <- udpipe::udpipe_annotate(model, x = xx)
    x <- as.data.frame(x)
    #x <- x[which(x$upos!="PUNCT"),]
    x <- x[which(x$upos=="VERB" | x$upos=="NOUN" | x$upos=="ADJ"),]
    
    
    keywords <- lapply(x$token,function(x){
      if(!is.na(as.vector(wordVec[[x]]))){
        paste(as.vector(wordVec[[x]]),collapse = " # ")
      }
    })
    
    unique(unlist(keywords))
  })
  
  unique_nouns <- lapply(collect, function(tt){
    paste(sort(unique(unlist(tt))), collapse = ", ")
  })
  
  tokenised <- unique_nouns
  
  #tokenised <- data.frame(x = c(1:length(unique_nouns)),
  #                     y = unlist(unique_nouns),
  #                     stringsAsFactors = FALSE)
  
  return(tokenised)
}

# SIMILARITY MEASURE -------------------------------------------------------

similar <- function(xData,yData,lev=0.9){
  res <- list()
  
  #xData <- as.numeric(unlist(strsplit(xData," # ")))
  #yData <- as.numeric(unlist(strsplit(yData," # ")))
  
  res[[1]] <- FALSE
  res[[2]] <- 0
  
  #euclidian distance of the spectral densities or the raw signals
  #diff<-dist(rbind(xData,yData),method = "euclidian")
  sim<-cosine(xData,yData)[1,1]
  
  
  #threshold for cosine
  if(sim > lev){
    res[[1]] <- TRUE
    res[[2]] <- sim
  }
  
  return(res)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


