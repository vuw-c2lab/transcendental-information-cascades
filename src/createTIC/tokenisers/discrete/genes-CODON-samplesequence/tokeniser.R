# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores){
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/data/",dataSource), col_names = F)
  sourceData$X1 <- tolower(sourceData$X1)

  tokenised <- list()
  
  for(i in 1:nrow(sourceData)){
    cdn <- c()
    for(j in 0:(floor(nchar(sourceData$X1[i])/3)-1)){
      for(k in 0:2){
        if(nchar(substr(c(sourceData$X1)[i],((j*3)+1+k),((j*3)+3+k))) == 3 & !grepl("n",substr(c(sourceData$X1)[i],((j*3)+1+k),((j*3)+3+k))) & !grepl("-",substr(c(sourceData$X1)[i],((j*3)+1+k),((j*3)+3+k))))
        cdn <- c(cdn,paste0("pos",j+1,"#+",k+1,"#",substr(c(sourceData$X1)[i],((j*3)+1+k),((j*3)+3+k))))
      }
    }
    
    tokenised[[length(tokenised)+1]] <- paste(unique(cdn), collapse = ", ")
  }
  
  tokenised <- data.frame(x = c(1:length(tokenised)),
                       y = unlist(tokenised),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


