# MAIN FUNCTION -----------------------------------------------------------

tokenise <- function(dataSource,no_cores){
  # read source data
  sourceData <- readr::read_delim(paste0(getwd(),"/data/",dataSource), col_names = F,delim = ";")
  sourceData$X1 <- tolower(sourceData$X1)
  
  tokenised <- data.frame(x = sourceData$X1,
                       y = gsub(",",", ",sourceData$X3),
                       stringsAsFactors = FALSE)
  
  return(tokenised)
}


# SUPPORT FUNCTIONS -------------------------------------------------------


