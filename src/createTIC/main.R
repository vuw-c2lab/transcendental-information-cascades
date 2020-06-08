
# PREAMBLE ----------------------------------------------------------------
set.seed(3011)
RNGkind("L'Ecuyer-CMRG")
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
  nodes <- data.frame(id=numeric(nrow(inputsequence)),tokens=character(nrow(inputsequence)),dpub=character(nrow(inputsequence)),stringsAsFactors = F)
  links <- c()
  last_node <- list()
  cnt <- 0
  links <- apply(inputsequence,1,function(x){
    cnt <<- cnt + 1
    print(paste0("# process node ",cnt, " of ",nrow(inputsequence)))
    tokens <- unlist(strsplit(as.character(x[2]), split=", "))
    nodes[cnt,1] <<- as.numeric(x[1])
    nodes[cnt,2] <<- as.character(x[2])
    nodes[cnt,3] <<- x[1]

    tmp <- lapply(tokens,function(y){
      cur_token <- y
      new_link <- NULL
      #print(x)
      if(!is.null(unlist(last_node[cur_token]))){
        source_node <- as.numeric(last_node[cur_token])
        target_node <- as.numeric(x[1])
        new_link <- data.frame(source=as.numeric(source_node), target=as.numeric(target_node), token=cur_token)
        #new_link <- unlist(new_link)
      }
      last_node[cur_token] <<- as.numeric(x[1])
      if(!is.null(new_link)){
        new_link
      }
    })
    tmp <- do.call("rbind", tmp)
    tmp
  })
  links <- do.call("rbind", links)
  #print(links)
  return(list(nodes, as.data.frame(links)))
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
      }
      if(length(allOldVectors>0)){
        for(m in 1:length(allOldVectors)){
          words <- c(words,closest_to(wordVec,as.VectorSpaceModel(matrix(nrow = 1,ncol = length(as.numeric(unlist(strsplit(allOldVectors[m]," # ")))),data = as.numeric(unlist(strsplit(allOldVectors[m]," # "))))))[1,1])
        }
      }
      
      nodes <- rbind(nodes, c(as.numeric(j),paste(words,collapse=", "),as.numeric(j)))
    }
  } else if(grepl(pattern = "mean-standard-error",x = tokeniser)){
    for(j in 1:nrow(inputsequence)){
      print(paste0("# process node ",j, " of ",nrow(inputsequence)))
      
      if(j>1){
        for(k in 1:(j-1)){
          tokens <- unlist(strsplit(as.character(inputsequence[k,2]), split=", "))
          for(l in 1:length(tokens)){
            
            cur_token <- tokens[l]
            
            cur_val <- as.numeric(unlist(strsplit(cur_token,"#"))[3])
            cur_serr <- as.numeric(unlist(strsplit(cur_token,"#"))[4])
            cur_token_cut <- paste0(unlist(strsplit(cur_token,"#"))[1],"#",unlist(strsplit(cur_token,"#"))[2])
            
            if(is.null(matchedSource[[paste0(k,"#",cur_token_cut)]])){
              
              target_tokens <- unlist(strsplit(as.character(inputsequence[j,2]), split=", "))
              
              link_token <- target_tokens[which(grepl(cur_token_cut,target_tokens))]
              
              if(length(link_token) > 0){
                target_val <- as.numeric(unlist(strsplit(link_token[1],"#"))[3])
                target_serr <- as.numeric(unlist(strsplit(link_token[1],"#"))[4])
                target_token_cut <- paste0(unlist(strsplit(link_token[1],"#"))[1],"#",unlist(strsplit(link_token[1],"#"))[2])
                
                out <- tryCatch({
                  if((target_val+target_serr) >= (cur_val-cur_serr) & (target_val-target_serr) <= (cur_val+cur_serr)){
                    links<-rbind(links,c(k,j,paste0(cur_token_cut)))
                    matched[[paste0(j,"#",cur_token_cut)]]<-1
                    matchedSource[[paste0(k,"#",cur_token_cut)]]<-1
                    print(nrow(links))
                  }
                },
                error=function(cond) {
                  message(cond)
                  print("source")
                  print(cur_val)
                  print(cur_serr)
                  print("target")
                  print(target_val)
                  print(target_serr)
                })
              }
            }
          }
        }
      }
      nodes <- rbind(nodes, c(as.numeric(j),inputsequence[j,2],as.numeric(j)))
    }
    return(list(nodes, links))
  } else{
    for(j in 1:length(inputsequence)){
      interact<-list()
      if(j>1){
        for(k in 1:(j-1)){
          for(l in 1:ncol(inputsequence[[k]])){
            if(is.null(matched[[paste(j,l,sep='_')]]) & is.null(matchedSource[[paste(k,l,sep='_')]])){
              
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