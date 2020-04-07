# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource){
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/",dataSource), col_names = F)
  sourceData$X1 <- tolower(sourceData$X1)

  print(floor(nchar(sourceData$X1[1])/3)-1)
  
  tokenised <- list()
  
  for(i in 1:nrow(sourceData)){
    cdn <- c()
    for(j in 0:(floor(nchar(sourceData$X1[i])/3)-1)){
      for(k in 0:2){
        if(nchar(substr(sourceData$X1[i],((j*3)+1+k),((j*3)+3+k))) == 3 & !grepl("n",substr(sourceData$X1[i],((j*3)+1+k),((j*3)+3+k))) & !grepl("-",substr(sourceData$X1[i],((j*3)+1+k),((j*3)+3+k))))
        cdn <- c(cdn,paste0("pos",j+1,"#+",k+1,"#",substr(sourceData$X1[i],((j*3)+1+k),((j*3)+3+k))))
      }
    }
    
    tokenised[[length(tokenised)+1]] <- paste(unique(cdn), collapse = ", ")
  }
  
  tokenised <- data.frame(x = c(1:length(tokenised)),
                       y = unlist(tokenised),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}

setwd("/Users/thimic/Developer/TIC")
system.time(
tokens <- tokenise("ordered_short.txt")
)
# SUPPORT FUNCTIONS -------------------------------------------------------


