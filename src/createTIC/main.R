
# MAIN FUNCTION -----------------------------------------------------------

createTIC <- function(projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  print("## createTIC ##")
  print(paste0("Project: ",projectDir))
  print(paste0("Data source: ",dataSource))
  print(paste0("Tokeniser: ",tokeniser))
  
  # create input sequence by passing data source to tokeniser
  source(paste0("../../src/createTIC/tokenisers/",tokeniser,"/tokeniser.R"))
  tokenised <- tokenise(dataSource,no_cores)
  
  # create TIC using input sequence
  if(grepl("continuous/",tokeniser)) tic <- generateContinuousTIC(tokenised)
  else tic <- generateDiscreteTIC(tokenised)
  
  return(tic)
}


# SUPPORT FUNCTIONS -------------------------------------------------------

generateDiscreteTIC <- function(inputsequence) {
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

generateContinuousTIC <- function(inputsequence,tokeniser) {
  
  nodes <- c()
  links <- c()
  
  matched<-list()
  matchedSource<-list()
  
  for(j in 1:length(inputsequence)){
    interact<-list()
    if(j>1){
      for(k in 1:(j-1)){
        print(inputsequence[[k]])
        for(l in 1:ncol(inputsequence[[k]])){
          if(is.null(matched[[paste(j,l,sep='_')]]) && is.null(matchedSource[[paste(k,l,sep='_')]])){
            
            #load similarity measure
            isSimilar <- similar(as.numeric(unlist(inputsequence[[j]][,l])),as.numeric(unlist(inputsequence[[k]][,l])),0.6)
            
            if(isSimilar[[1]]){
              
              interact[[paste0(l)]]<-l
              
              #if(length(tail(which(links[,3]==l),1)) > 0){
              #  if(links[tail(which(links[,3]==l),1),2]!=as.character(k)){
              #    links<-rbind(links,c(links[tail(which(links[,3]==l),1),2],k,paste0(l),0.2))
              #  }
              #}
              #links<-rbind(links,c(k,j,paste0(l),mean(isSimilar[[2]])))
              links<-rbind(links,c(k,j,paste0(l)))
              matched[[paste(j,l,sep='_')]]<-1
              matchedSource[[paste(k,l,sep='_')]]<-1
            }
          }
        }
      }
    }
    nodes <- rbind(nodes, c(as.numeric(j),paste(interact,collapse=', '),as.numeric(j)))
  }
  
  return(list(nodes, links))
}