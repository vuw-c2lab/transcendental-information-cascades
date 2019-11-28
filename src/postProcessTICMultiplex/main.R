
# PREAMBLE ----------------------------------------------------------------

dir.create(file.path(paste0("../../output/",projectDir,"/"), "postProcessTICMultiplex"), showWarnings = FALSE)

# PREAMBLE ----------------------------------------------------------------

if (!require(rTensor)) install.packages('rTensor')
if (!require(mplex)) install.packages('mplex')

# MAIN FUNCTION -----------------------------------------------------------

postProcessTICMultiplex <- function(projectDir, outputDir, dataSource, allTICs, tokeniser,no_cores=1){
  # generate the mode of the TIC tensor
  z <- length(allTICs) # how many layers
  x <- y <- 0
  
  if(z > 1){ # only construct multiplex if more than one layer
    firstLayer <- as.matrix(readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[1],"/createTIC/TICmatrix.csv"), col_names = T))
    rownames(firstLayer) <- colnames(firstLayer)
    
    mplexLayers <- list()
    
    nodes <- readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[1],"/createTIC/nodes.csv"))
    links <- readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[1],"/createTIC/links.csv"))
    links$weight <- 1
    
    mplexLayers[["L1"]] <- links[,c(1,2,4)]
    
    x <- y <- ncol(firstLayer)
    indices <- c(x,y,z)
    
    mats <- list()
    
    for(i in 2:z){
      mats[[i-1]] <- as.matrix(readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[i],"/createTIC/TICmatrix.csv"), col_names = T))
      links <- readr::read_csv(paste0("../../output/",projectDir,"/",allTICs[i],"/createTIC/links.csv"))
      links$weight <- 1
      mplexLayers[[paste0("L",i)]] <- links[,c(1,2,4)]
    }
    
    # initialise tensor randomly
    tnsr <- as.tensor(array(c(c(firstLayer),unlist(lapply(mats,function(x){c(x)}))),dim=indices))
    
    mplexObj <- create.multiplex(nodes = as.data.frame(nodes[,c(1,3)]),
                                 #layersNames = c("1","2","3","4"),
                                 layer1 = as.data.frame(mplexLayers$L1),
                                 type1 = "undirected",
                                 as.data.frame(mplexLayers$L2),
                                 as.data.frame(mplexLayers$L3),
                                 as.data.frame(mplexLayers$L4),
                                 as.data.frame(mplexLayers$L5),
                                 as.data.frame(mplexLayers$L6)
    )
    
    input <- list(nodes = as.data.frame(nodes[,c(1,3)]),
               #layersNames = c("1","2","3","4"),
               layer1 = as.data.frame(mplexLayers$L1),
               type1 = "undirected")
    for(j in 2:z){
      input[[length(input)+1]] <- mplexLayers[[j]]
    }
    #mplexObj <- do.call(create.multiplex,  input)
    
  } else{
    print("not enough TICs created")
    tnsr <- NULL
  }
  return(list(tnsr,mplexObj))
}


# SUPPORT FUNCTIONS -------------------------------------------------------
