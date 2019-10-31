
# PREAMBLE ----------------------------------------------------------------

dir.create(file.path(paste0("../../output/",projectDir,"/"), "postProcessTICMultiplex"), showWarnings = FALSE)

# PREAMBLE ----------------------------------------------------------------

if (!require(rTensor)) install.packages('rTensor')

# MAIN FUNCTION -----------------------------------------------------------

postProcessTICMultiplex <- function(projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  # load all TIC adjacency matrices
  allTICs <- list.dirs(paste0("../../output/",projectDir,"/"),full.names = F,recursive = F)
  allTICs <- allTICs[-which(allTICs=="postProcessTICMultiplex")]
  # generate the mode of the TIC tensor
  z <- length(allTICs) # how many layers
  x <- y <- 0
  
  if(z > 1){ # only construct multiplex if more than one layer
    firstLayer <- as.matrix(readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[1],"/createTIC/TICMatrix.csv"), col_names = T))
    rownames(firstLayer) <- colnames(firstLayer)
    
    x <- y <- ncol(firstLayer)
    indices <- c(x,y,z)
    
    mats <- list()
    
    for(i in 2:z){
      mats[[i-1]] <- as.matrix(readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[i],"/createTIC/TICMatrix.csv"), col_names = T))
    }
    
    # initialise tensor randomly
    tnsr <- as.tensor(array(c(c(firstLayer),unlist(lapply(mats,function(x){c(x)}))),dim=indices))
  } else{
    print("not enough TICs created")
    tnsr <- NULL
  }
  return(tnsr)
}


# SUPPORT FUNCTIONS -------------------------------------------------------

