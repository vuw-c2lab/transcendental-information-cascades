
# MAIN FUNCTION -----------------------------------------------------------

createTIC <- function(projectDir, outputDir, dataSource, tokeniser){
  print("## createTIC ##")
  print(paste0("Project: ",projectDir))
  print(paste0("Data source: ",dataSource))
  print(paste0("Tokeniser: ",tokeniser))
  
  # create input sequence by passing data source to tokeniser
  source(paste0("../../src/createTIC/tokenisers/",tokeniser,"/tokeniser.R"))
  tokenised <- tokenise(dataSource)
  
  # create TIC using input sequence
  tic <- generateTIC(tokenised)
  
}


# SUPPORT FUNCTIONS -------------------------------------------------------

generateTIC <- function(inputsequence) {
  nodes <- c()
  links <- c()
  last_node <- list()
  
  for(pp in 1:nrow(inputsequence)){
    tokens <- unlist(strsplit(as.character(inputsequence[which(inputsequence[,1] == pp),2]), split=", "))
    nodes <- rbind(nodes, c(inputsequence[which(inputsequence[,1]== pp),1],
                            as.character(inputsequence[which(inputsequence[,1] == pp),2]),
                            inputsequence[which(inputsequence[,1] == pp),1]))
    
    for(jj in 1:length(tokens)){
      cur_token <- tokens[jj]
      if(!is.null(unlist(last_node[cur_token]))){ 
        source_node <- last_node[cur_token]
        target_node <- pp
        links <- rbind(links, c(source_node, target_node, cur_token))
      }
      last_node[cur_token] <- pp
    }
    
  }
  return(list(nodes, links))
}