
# PREAMBLE ----------------------------------------------------------------
# 
if (!require(digest)) install.packages('digest')
if (!require(entropy)) install.packages('entropy')
if (!require(vegan)) install.packages('vegan')

# MAIN FUNCTION -----------------------------------------------------------

postProcessTICNetwork <- function(tic=NULL, projectDir=NULL, outputDir=NULL, dataSource=NULL, tokeniser=NULL,no_cores=1,path=NULL){
  if(is.null(tic)){
    nodes <- read.csv(paste0(path,"nodes.csv"),stringsAsFactors = F)
    links <- read.csv(paste0(path,"links.csv"), stringsAsFactors = F)
    
  } else{
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                        ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        token = unlist(tic[[2]][,3]), stringsAsFactors = F)
    dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
    path = paste0("../../output/", projectDir,"/",outputDir,"/createTIC/")
  }
  
  createTICMatrix(nodes, links, path, no_cores=1)
  #createCooccurrenceMatrix(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
  
  createTICFeaturesFeature(nodes, links, path, no_cores=1)
}


# SUPPORT FUNCTIONS -------------------------------------------------------

createTICMatrix <- function(nodes, links, path, no_cores=1){
  # create TIC network matrix and persist matrix
  
  colnames(links) <- c("id1","id2","label")
  groupedLinks <- plyr::count(links, vars=c("id1","id2"))
  colnames(groupedLinks) <- c("id1","id2","weight")
  g <- igraph::graph.data.frame(groupedLinks,directed = F,vertices = nodes)
  readr::write_csv(as.data.frame(igraph::as_adjacency_matrix(g, sparse = F, type = "both", attr = "weight")), paste0(path,"TICmatrix.csv"),col_names = T)
  rm(g)
  rm(links)
  gc()
}

createCooccurrenceMatrix <- function(nodes, links, path, no_cores=1){
  # create co-occurrence matrix and persist matrix
  
  colnames(nodes) <- c('id','title','label')
  
  socN1 <- c()
  counter_lin <- 0
  socEdges <- NULL
  for(lin in 1:nrow(nodes)){
    nex <- unlist(strsplit(as.character(nodes$title[lin]), ", "))
    if(length(nex) > 1){
      socN1 <- rbind(socN1, paste(nex, collapse = ', '))
      
      
      socEdges <- lapply(socN1, function(lin){
        templin <- unlist(strsplit(lin, ', '))
        gtools::combinations(length(templin),
                             2, templin)
      })
    }
  }
  if(!is.null(socEdges)){
    socEdges <- do.call("rbind", socEdges)
    socEdges <- plyr::count(socEdges)
    colnames(socEdges) <- c("id1", "id2", "label")
    h <- igraph::graph.data.frame(socEdges, directed = F)
    igraph::E(h)$weight <- igraph::E(h)$label
    netm <- igraph::as_adjacency_matrix(h, attr = "weight", sparse = F)
    colnames(netm) <- igraph::V(h)$name
    rownames(netm) <- igraph::V(h)$name
    readr::write_csv(as.data.frame(netm), paste0(path,"coocmatrix.csv"),col_names = T)
  }
}

createTICFeaturesFeature <- function(nodes, links, path, no_cores=1){
  # now compute the structural and information theoretic features and persist these
  # 
  allTokens<-unique(unlist(strsplit(nodes$tokens,", ")))
  inter <- setNames(as.list(rep(0,length(allTokens))),allTokens)
  #ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
  wien <- data.frame(ShannonWiener=numeric(nrow(nodes)),Pielou=numeric(nrow(nodes)),Richness=numeric(nrow(nodes)))
  colnames(links) <- c('source', 'target', 'tag')
  
  coordinates <- data.frame(t=numeric(nrow(nodes)),specificity=numeric(nrow(nodes)),diversity=numeric(nrow(nodes)))
  spec <- list()
  div <- 1
  
  #g1 <- make_empty_graph(n = 0, directed = TRUE)
  #struct <- data.frame(diameter=numeric(nrow(nodes)),density=numeric(nrow(nodes)),modularity=numeric(nrow(nodes)))
  z <- 0
  
  res <- apply(nodes, 1, function(x){
    z <<- z + 1
    
    print(paste0("# compute seq features up to node ",z))
    if(nodes[z,]$tokens == "" | is.na(nodes[z,]$tokens)){
      if(z > 1){
        #ent <- rbind(ent, ent[nrow(ent),])
        wien[z,] <<- wien[z-1,]
        
      }else{
        #ent <- rbind(ent, c(0, 1, 0, 1))
        wien[z,] <<- c(0, 1, 1)
      }
      coordinates[z,] <<- c(as.numeric(nodes[z, 1]), 0, 0)
      
    }else{
      sortedInter <- paste(sort(unlist(strsplit(as.character(nodes[z,2]), ', '))), collapse = ', ')
      nextI <- digest(sortedInter, algo = "md5")
      if(length(spec) == 0){
        coordinates[z,] <<- c(as.numeric(nodes[z, 1]), 1, 1)
        spec[[nextI]] <<- c(1, 1)
      }
      else{
        if(is.null(spec[[nextI]])){
          div <<- div+1
          spec[[nextI]] <<- c(1,div)
          coordinates[z,] <<- c(as.numeric(nodes[z,1]),spec[[nextI]][1],div)
        }else{
          spec[[nextI]] <<- c(spec[[nextI]][1]+1,spec[[nextI]][2])
          coordinates[z,] <<- c(as.numeric(nodes[z,1]),spec[[nextI]][1],spec[[nextI]][2])
        }
      }
      
      interact <- list()
      cooccure <- list()
      #temp1 <- c()
      interactions <- unlist(strsplit(unlist(sortedInter),', '))
      for(v in 1:length(interactions)){
        inter[[interactions[v]]] <<- inter[[interactions[v]]] + 1
      }
      
      df <- data.frame(unlist(inter[inter!=0]))
      # tmp <- df[,1] / colSums(df)
      # df$loga <- log(tmp)
      # df$piloga <- tmp * log(tmp)
      # if(is.nan((-1 * (colSums(df)[3])) / log(nrow(df)))){
      #   ent <- rbind(ent,c(entropy.empirical(df[,1], unit = "log2"), 1, -1 * (colSums(df)[3]), 1))
      # } else{
      #   ent <- rbind(ent, c(entropy.empirical(df[,1], unit = "log2"), (-1 * (colSums(df)[3])) / log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
      # }
      
      if(nrow(df) == 1){
        wien[z,] <<- c(0, 1, 1)
      } else{
        S <- nrow(df)
        H <- vegan::diversity(df[,1])
        J <- H/log(S)
        wien[z,] <<- c(H, J, S)
      }}
    
    #}
    #colnames(ent)<-c('empEntropy', 'evenness_log2', 'entropy', 'evenness')
    
    #add node
    #g1 <<- add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
    
    #add all links to node
    #theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
    #for(srclnk in theLinks[,1]){
    #  g1 <<- add_edges(g1, c(which(V(g1)$id == srclnk), which(V(g1)$id == as.numeric(nodes[z,1]))))
    #}
    
    #degd <- degree.distribution(g1)
    #wtc <- cluster_walktrap(g1)
    #struct[z,] <<- c(diameter(g1), edge_density(g1), modularity(wtc))
  })
  
  #readr::write_csv(as.data.frame(ent), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICInfoTheory1.csv"),col_names = T)
  readr::write_csv(as.data.frame(wien), paste0(path,"TICInfoTheory2.csv"),col_names = T)
  
  #scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
  
  
  # ent_plot <- as.data.frame(ent) %>%
  #   mutate(rownumber = seq.int(nrow(.)))
  
  wien_plot <- as.data.frame(wien) %>%
    mutate(rownumber = seq.int(nrow(.)))
  
  #struct[which(is.na(struct))]<<-0
  #readr::write_csv(as.data.frame(struct), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICStructFeatures.csv"),col_names = T)
  
  readr::write_csv(as.data.frame(coordinates), paste0(path,"TICCoordinates.csv"),col_names = T)
  
  #struc_plot <- as.data.frame(struct) %>%
  #  mutate(rownumber = seq.int(nrow(.)))
  
  #ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource,"_gutenberg_entropy.jpg"),
  # plot = ent_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = empEntropy, group = 1) +
  #   geom_line() +
  #   labs(y = "Entropy") +
  #   theme_classic()
  # plot
  # 
  # #ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource,"_gutenberg_evenness.jpg"),
  # plot = ent_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = evenness_log2, group = 1) +
  #   geom_line() +
  #   labs(y = "Log Evenness") +
  #   theme_classic()
  # plot
  # 
  # #ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource, "_gutenberg_shannonwiener.jpg"),
  # plot = wien_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = ShannonWiener, group = 1) +
  #   geom_line() +
  #   labs(y = "Shannon Wiener") +
  #   theme_classic()
  # plot
  # 
  # #ggsave(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_pielou.jpg"),
  # plot = wien_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = Pielou, group = 1) +
  #   geom_line() +
  #   labs(y = "Pielou") +
  #   theme_classic()
  # plot
  # 
  # #ggsave(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_richness.jpg"),
  # plot = wien_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = Richness, group = 1) +
  #   geom_line() +
  #   labs(y = "Richness") +
  #   theme_classic()
  # plot
  # 
  # plot = struc_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = diameter, group = 1) +
  #   geom_line() +
  #   labs(y = "Diameter") +
  #   theme_classic()
  # plot
  # 
  # plot = struc_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = density, group = 1) +
  #   geom_line() +
  #   labs(y = "Density") +
  #   theme_classic()
  # plot
  # 
  # plot = struc_plot %>%
  #   ggplot() +
  #   aes(x = rownumber, y = modularity, group = 1) +
  #   geom_line() +
  #   labs(y = "Modularity") +
  #   theme_classic()
  # plot
  # 
  # ###powerlaw???
  # library(poweRlaw)
  # 
  # 
  # linksDelta <- as.integer(links$target)-as.integer(links$source)
  # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta.jpg"))
  # plot(linksDelta,type='l')
  # abline(h=mean(linksDelta),col="red")
  # abline(h=median(linksDelta),col="blue")
  # #dev.off()
  # linksDelta.count <- plyr::count(linksDelta)
  # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri.jpg"))
  # plot(linksDelta.count$x,linksDelta.count$freq,pch=20)
  # #dev.off()
  # 
  # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_loglog.jpg"))
  # plot(linksDelta.count$x,linksDelta.count$freq,pch=20,log="xy")
  # #dev.off()
  # 
  # #power law?
  # 
  # #powerlaw
  # m_bl = displ$new(linksDelta)
  # est = estimate_xmin(m_bl)
  # #m_bl$setXmin(est)
  # m_bl$setPars(estimate_pars(m_bl))
  # #lognormal
  # m_ln = dislnorm$new(linksDelta)
  # est = estimate_xmin(m_ln)
  # #m_ln$setXmin(est)
  # m_ln$setPars(estimate_pars(m_ln))
  # #poissoin
  # m_pois = dispois$new(linksDelta)
  # est = estimate_xmin(m_pois)
  # #m_pois$setXmin(est)
  # m_pois$setPars(estimate_pars(m_pois))
  # 
  # #jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
  # plot(m_bl, ylab="CDF",pch=20)
  # #text(100,0.15,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
  # lines(m_bl, col=2)
  # lines(m_ln, col=3)
  # lines(m_pois, col=4)
  # #dev.off()
  gc()
}