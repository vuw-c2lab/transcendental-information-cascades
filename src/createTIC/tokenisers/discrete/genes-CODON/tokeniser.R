# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores){
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/data/",dataSource), col_names = F)
  sourceData$X1 <- tolower(sourceData$X1)
  
  slice <- 3
  
  tokenised <- list()
  
  for(i in seq(slice,nrow(sourceData),by = slice)){
    tokenised[[length(tokenised)+1]] <- paste(unique(sourceData$X1[c((i-(slice-1)):i)]), collapse = ", ")
  }
  
  tokenised <- data.frame(x = c(1:length(tokenised)),
                       y = unlist(tokenised),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


