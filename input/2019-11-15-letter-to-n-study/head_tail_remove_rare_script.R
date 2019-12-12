
links <- readr::read_csv("../../output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/links.csv")
tokenProbs <- plyr::count(as.character(links$token))
tokenProbs$prob <- as.numeric(tokenProbs$freq)/nrow(links)


nodes <- readr::read_csv("../../output/2019-11-15-letter-to-n-study/primes50k-discrete-tokenised-2019-11-21-17-54-58/createTIC/nodes.csv")
setProbs <- plyr::count(nodes$tokens)
setProbs$prob <- as.numeric(setProbs$freq)/nrow(nodes)
distr <- plyr::count(setProbs$freq)
plot(distr,type='l')

comProbs <- apply(setProbs,1,function(x){
  tokens <- unlist(strsplit(as.character(x[1]),", "))
  product <- c()
  lowToken <- list(NULL,NULL)
  for(i in tokens){
    if(is.null(lowToken[[2]])){
      lowToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
      lowToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
    } else if(lowToken[[2]] > tokenProbs[which(tokenProbs$x==i),3]){
      lowToken[[1]] <- tokenProbs[which(tokenProbs$x==i),1]
      lowToken[[2]] <- tokenProbs[which(tokenProbs$x==i),3]
    }
    product <- c(product,tokenProbs[which(tokenProbs$x==i),3])
  }
  res <- list(prod(product),lowToken[[1]])
  res
})

setProbs$tokenprobs <- sapply(comProbs,function(x){
  x[[1]]
})

setProbs$lowToken <- sapply(comProbs,function(x){
  x[[2]]
})

setProbs$overallprob <- setProbs$tokenprobs * setProbs$prob

setProbs <- setProbs[order(setProbs$prob),]

primes50k_headRemove1 <- primes50k
# 1 remove the item with lowest probabiliyty from the set with lowest set probability (do for bottom 5 sets)
# 
for(i in 1:20){
  primes50k_tailRemove1 <- primes50k
  nodesToRem <- which(nodes$tokens == setProbs$x[i])
  print(nodesToRem)
  for(j in 1:length(nodesToRem)){
    tokens <- unlist(strsplit(as.character(primes50k_tailRemove1[nodesToRem[j],3]),", "))
    tokens <- tokens[-which(tokens == setProbs$lowToken[i])]
    primes50k_tailRemove1[nodesToRem[j],3] <- paste0(tokens,collapse = ", ")
  }
  write_delim(as.data.frame(primes50k_tailRemove1),paste0("data/primes50k_tailRemove_",i,".txt"),delim = ";",col_names = F)
}



# 2 remove item with highest probability from set with highest set probability
# 
# 
# 3 compare eigenvalue distribution and largest eigenvaue for TIC after these steps
# 
# 4 remove item with lowest probability from set with lowest token probability
# 
setProbs <- setProbs[order(setProbs$tokenprobs),]
# 
# 5 remove item with highest probability from set with highest token probability
