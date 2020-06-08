# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores){
  # read source data
  sourceData <- readr::read_csv(paste0(getwd(),"/data/",dataSource), col_names = F)
  
  tokenised <- data.frame(x = c(1:nrow(sourceData)),
                       y = sourceData[,1],
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


