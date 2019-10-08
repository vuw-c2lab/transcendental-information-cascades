
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
  if(grepl("signal/",tokeniser)) tic <- generateContinuousTIC(tokenised)
  else tic <- generateDiscreteTIC(tokenised)
  
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

generateContinuousTIC <- function(inputsequence) {
  cohlev <- 0.6 #this is for spectral coherence 
  
  nodes <- c()
  links <- c()
  
  matched<-list()
  matchedSource<-list()
  
  for(j in 1:length(events)){
    interact<-list()
    if(j>1){
      for(k in 1:(j-1)){
        for(l in 1:ncol(events[[k]])){
          if(is.null(matched[[paste(j,l,sep='_')]]) && is.null(matchedSource[[paste(k,l,sep='_')]])){
            #euclidian distance of the spectral densities or the raw signals
            #diff<-dist(rbind(unlist(events[[j]][,l]),unlist(events[[k]][,l])), method = "euclidean")
            
            #spectral coherence
            #diff<-stats::spectrum(cbind(unlist(events[[j]][,l]),unlist(events[[k]][,l])), plot=FALSE,spans=c(3,5))$coh
            diff<-max(ccf(unlist(events[[j]][,l]),unlist(events[[k]][,l]),plot = F)$acf)
            
            if(is.na(diff[1])){
              diff <- 1
            }
            #compare the power spectra with the granger test
            #diff <- round(grangertest(unlist(events[[j]][,l]),unlist(events[[k]][,l]))$F[2])
            
            #sum of the raw delta of the spectral densities
            #diff<-round(sum(data.frame(events[j])[,l]-data.frame(events[k])[,l]),0)
            
            #threshold for euclidian
            #if(abs(diff)<lev){
            
            #threshold for granger test -> we are interested in F score around 1 for similarity
            #if(round(diff)==1){
            # for sectral coherence
            if(mean(diff)>=cohlev){
              
              #print(mean(diff))
              interact[[paste0(l)]]<-l
              
              #if(length(tail(which(links[,3]==l),1)) > 0){
              #  if(links[tail(which(links[,3]==l),1),2]!=as.character(k)){
              #    links<-rbind(links,c(links[tail(which(links[,3]==l),1),2],k,paste0(l),0.2))
              #  }
              #}
              links<-rbind(links,c(k,j,paste0(l),mean(diff)))
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