
# PREAMBLE ----------------------------------------------------------------

dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)


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
  if(grepl("continuous/",tokeniser)) tic <- generateContinuousTIC(tokenised,tokeniser)
  else tic <- generateDiscreteTIC(tokenised)
  
  # persist TIC to files (optional but needed if post-processing doesn't happen immediately)
  nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                      ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
  links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                      token = unlist(tic[[2]][,3]), stringsAsFactors = F)
  readr::write_csv(nodes,paste0("../../output/", projectDir,"/",
                                outputDir,"/createTIC/nodes.csv"),col_names = T)
  readr::write_csv(links,paste0("../../output/", projectDir,"/",
                                outputDir,"/createTIC/links.csv"),col_names = T)
  
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
  
  if(grepl(pattern = "text",x = tokeniser)){
    for(j in 1:length(inputsequence)){
      interact<-list()
      allOldVectors <- unlist(strsplit(inputsequence[[j]],", "))
      words <- c()
      if(j>1 & length(allOldVectors)>0){
        for(k in 1:(j-1)){
          
          allVectors <- unlist(strsplit(inputsequence[[k]],", "))
          if(length(allVectors)>0){
            for(l in 1:length(allVectors)){
              #if(is.null(matched[[paste(j,allOldVectors[m],sep='_')]]) && is.null(matchedSource[[paste(k,allVectors[l],sep='_')]])){
                  #load similarity measure
                  for(m in 1:length(allOldVectors)){
                    isSimilar <- similar(as.numeric(unlist(strsplit(allOldVectors[m]," # "))),as.numeric(unlist(strsplit(allVectors[l]," # "))))
                    
                    if(isSimilar[[1]]){
                      
                      wordSource <- closest_to(wordVec,as.VectorSpaceModel(matrix(nrow = 1,ncol = length(as.numeric(unlist(strsplit(allVectors[l]," # ")))),data = as.numeric(unlist(strsplit(allVectors[l]," # "))))))[1,1]
                      wordTarget <- closest_to(wordVec,as.VectorSpaceModel(matrix(nrow = 1,ncol = length(as.numeric(unlist(strsplit(allOldVectors[m]," # ")))),data = as.numeric(unlist(strsplit(allOldVectors[m]," # "))))))[1,1]
                      
                      interact[[paste0(allVectors[l])]]<-paste0(wordSource,"-",wordTarget)
                      
                      if(is.null(matchedSource[[paste(k,allVectors[l],wordTarget,sep='_')]])){
                        links<-rbind(links,c(k,j,paste0(wordSource,"-",wordTarget)))
                        matched[[paste(j,allOldVectors[m],wordTarget,sep='_')]]<-1
                        matchedSource[[paste(k,allVectors[l],wordSource,sep='_')]]<-1
                      }
                    }
                  }
              }
          }
        }
        for(m in 1:length(allOldVectors)){
          words <- c(words,closest_to(wordVec,as.VectorSpaceModel(matrix(nrow = 1,ncol = length(as.numeric(unlist(strsplit(allOldVectors[m]," # ")))),data = as.numeric(unlist(strsplit(allOldVectors[m]," # "))))))[1,1])
        }
      }
      
      nodes <- rbind(nodes, c(as.numeric(j),paste(words,collapse=", "),as.numeric(j)))
    }
  } else{
    for(j in 1:length(inputsequence)){
      interact<-list()
      if(j>1){
        for(k in 1:(j-1)){
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
  }
  
  return(list(nodes, links))
}