
# PREAMBLE ----------------------------------------------------------------

dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)

# MAIN FUNCTION -----------------------------------------------------------

postProcessTICNetwork <- function(tic, projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                      ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
  links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                      token = unlist(tic[[2]][,3]), stringsAsFactors = F)
  
  createTICMatrix(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
  createCooccurrenceMatrix(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
}


# SUPPORT FUNCTIONS -------------------------------------------------------

createTICMatrix <- function(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  # create TIC network matrix and persist matrix
  
  colnames(links) <- c("id1","id2","label")
  g <- igraph::graph.data.frame(links,directed = TRUE,vertices = nodes)
  readr::write_csv(as.data.frame(igraph::as_adjacency_matrix(g, sparse = F)), paste0("../../output/", projectDir,"/",
                                                                                     outputDir,"/createTIC/TICmatrix.csv"),col_names = T)
}

createCooccurrenceMatrix <- function(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  # create co-occurrence matrix and persist matrix
  
  colnames(nodes) <- c('id','title','label')
  
  socN1 <- c()
  counter_lin <- 0
  for(lin in 1:nrow(nodes)){
    nex <- unlist(strsplit(nodes$title[lin], ", "))
    if(length(nex) > 1){
      socN1 <- rbind(socN1, paste(nex, collapse = ', '))
      
      
      socEdges <- lapply(socN1, function(lin){
        templin <- unlist(strsplit(lin, ', '))
        gtools::combinations(length(templin),
                             2, templin)
      })
    }
  }
  socEdges <- do.call("rbind", socEdges)
  socEdges <- plyr::count(socEdges)
  colnames(socEdges) <- c("id1", "id2", "label")
  h <- igraph::graph.data.frame(socEdges, directed = F)
  igraph::E(h)$weight <- igraph::E(h)$label
  netm <- igraph::as_adjacency_matrix(h, attr = "weight", sparse = F)
  colnames(netm) <- igraph::V(h)$name
  rownames(netm) <- igraph::V(h)$name
  readr::write_csv(as.data.frame(netm), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/coocmatrix.csv"),col_names = T)
}