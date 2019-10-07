# MAIN FUNCTION -----------------------------------------------------------

# NLP init

# download
#model <- udpipe::udpipe_download_model(language = "english")
#model <- udpipe::udpipe_load_model(file = model$file_model)

# or load from file 
model <- udpipe::udpipe_load_model(file = "/Users/MLR/english-ewt-ud-2.4-190531.udpipe") #english
#model <- udpipe::udpipe_load_model(file = "/Users/MLR/indonesian-gsd-ud-2.4-190531.udpipe") #bahasa
#model <- udpipe::udpipe_load_model(file = "/Users/MLR/portuguese-bosque-ud-2.4-190531.udpipe") #portuguese

tokenise <- function(dataSource){
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/data/",dataSource), col_names = F)
  sourceData$X1 <- tolower(sourceData$X1)
  
  #tokenise
  collect <- lapply(sourceData$X1, function(xx){
    
    #annotate
    x <- udpipe::udpipe_annotate(model, x = xx)
    x <- as.data.frame(x)
    
    x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
    phrases <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                              pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                              is_regex = TRUE, detailed = FALSE)
    phrases <- subset(phrases, ngram > 1)
    
    unique(phrases)
  })
  
  unique_phrases <- lapply(collect, function(tt){
    paste(sort(unique(unlist(tt))), collapse = ", ")
  }) 
  tokenised <- data.frame(x = c(1:length(unique_phrases)),
                       y = unlist(unique_phrases),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


