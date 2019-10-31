if (!require(wordVectors)) devtools::install_github("bmschmidt/wordVectors")

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
      sims <- wordVec %>% closest_to(x,n = 10)
      sims <- sims[which(sims[,2]>0.9),]
      sims$word
    })
    
    unique(unlist(keywords))
  })
  
  unique_nouns <- lapply(collect, function(tt){
    paste(sort(unique(unlist(tt))), collapse = ", ")
  }) 
  tokenised <- data.frame(x = c(1:length(unique_nouns)),
                       y = unlist(unique_nouns),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


