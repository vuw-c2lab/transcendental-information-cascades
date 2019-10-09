# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores=1){
  # NLP init
  
  # download
  #model <- udpipe::udpipe_download_model(language = "english")
  #model <- udpipe::udpipe_load_model(file = model$file_model)
  
  # or load from file 
  model <- udpipe::udpipe_load_model(file = "/Users/MLR/english-ewt-ud-2.4-190531.udpipe") #english
  #model <- udpipe::udpipe_load_model(file = "/Users/MLR/indonesian-gsd-ud-2.4-190531.udpipe") #bahasa
  #model <- udpipe::udpipe_load_model(file = "/Users/MLR/portuguese-bosque-ud-2.4-190531.udpipe") #portuguese
  
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/data/",dataSource), col_names = F)
  sourceData$X1 <- tolower(sourceData$X1)
  
  #tokenise
  #collect <- parLapply(cl,sourceData$X1, function(xx){
  collect <- lapply(sourceData$X1, function(xx){
    
    #annotate
    x <- udpipe::udpipe_annotate(model, x = xx)
    x <- as.data.frame(x)
    
    verbs <- subset(x, upos %in% c("VERB"))
    verbs <- verbs$lemma
    
    unique(verbs)
  })
  
  unique_verbs <- lapply(collect, function(tt){
    paste(sort(unique(unlist(tt))), collapse = ", ")
  }) 
  tokenised <- data.frame(x = c(1:length(unique_verbs)),
                       y = unlist(unique_verbs),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


