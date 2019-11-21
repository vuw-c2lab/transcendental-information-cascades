
# PREAMBLE ----------------------------------------------------------------
# 
if (!require(digest)) install.packages('digest')
if (!require(entropy)) install.packages('entropy')

dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)

# MAIN FUNCTION -----------------------------------------------------------

postProcessTICNetwork <- function(tic=NULL, projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  if(is.null(tic)){
    nodes <- read.csv(paste0("../../output/",projectDir,"/",outputDir,"/createTIC/nodes.csv"),stringsAsFactors = F)
    links <- read.csv(paste0("../../output/",projectDir,"/",outputDir,"/createTIC/links.csv"), stringsAsFactors = F)
    
  } else{
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                        ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        token = unlist(tic[[2]][,3]), stringsAsFactors = F)
  }
  
  createTICMatrix(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
  createCooccurrenceMatrix(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
  
  createTICFeaturesFeature(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1)
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
    readr::write_csv(as.data.frame(netm), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/coocmatrix.csv"),col_names = T)
  }
}

createTICFeaturesFeature <- function(nodes, links, projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  # create TIC network matrix and persist matrix
  
  colnames(links) <- c("id1","id2","label")
  g <- igraph::graph.data.frame(links,directed = TRUE,vertices = nodes)
  readr::write_csv(as.data.frame(igraph::as_adjacency_matrix(g, sparse = F)), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICmatrix.csv"),col_names = T)
  
  # now compute the structural and information theoretic features and persist these
  # 
  casc <- c()
  inter <- c()
  #ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
  wien <- c()
  colnames(links) <- c('source', 'target', 'tag')
  
  coordinates <- c()
  spec <- list()
  div = 1
  
  g1 <- make_empty_graph(n = 0, directed = TRUE)
  struct <- c()
  props <- c()
  for(z in 1:nrow(nodes)){
    if(nodes[z,]$tokens == "" | is.na(nodes[z,]$tokens)){
      if(nrow(ent) > 0){
        #ent <- rbind(ent, ent[nrow(ent),])
        wien <- rbind(wien, wien[nrow(wien),])
        
      }else{
        #ent <- rbind(ent, c(0, 1, 0, 1))
        wien <- rbind(wien, c(0, 1, 1))
      }
      coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 0, 0))
      
    }else{
      inter <- rbind(inter, paste(sort(unlist(strsplit(as.character(nodes[z,2]),', '))), collapse = ', '))
      nextI <- digest(paste(sort(unlist(strsplit(as.character(nodes[z,2]), ', '))), collapse = ', '), algo = "md5")
      if(length(spec) == 0){
        coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 1, 1))
        spec[[nextI]] <- c(1, 1)
      }
      else{
        if(is.null(spec[[nextI]])){
          spec[[nextI]] <- c(1,div)
          coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],div))
          div <- div+1
        }else{
          spec[[nextI]] <- c(spec[[nextI]][1]+1,spec[[nextI]][2])
          coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],spec[[nextI]][2]))
        }
      }
      
      interact <- list()
      cooccure <- list()
      #temp1 <- c()
      for(v in 1:nrow(inter)){
        interactions <- unlist(strsplit(unlist(inter[v,1]),', '))
        #temp1 <- rbind(temp1, gtools::combinations(length(interactions), 2, interactions))
        for( m in 1:length(interactions)){
          if(is.null(interact[[interactions[m]]])) interact[[interactions[m]]] <- 1
          else interact[[interactions[m]]] <- interact[[interactions[m]]] + 1
        }
      }
      df <- data.frame(unlist(interact))
      # tmp <- df[,1] / colSums(df)
      # df$loga <- log(tmp)
      # df$piloga <- tmp * log(tmp)
      # if(is.nan((-1 * (colSums(df)[3])) / log(nrow(df)))){
      #   ent <- rbind(ent,c(entropy.empirical(df[,1], unit = "log2"), 1, -1 * (colSums(df)[3]), 1))
      # } else{
      #   ent <- rbind(ent, c(entropy.empirical(df[,1], unit = "log2"), (-1 * (colSums(df)[3])) / log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
      # }
      
      if(nrow(df) == 1){
        wien <- rbind(wien, c(0, 1, 1))
      } else{
        H <- vegan::diversity(df[,1])
        S <- nrow(df)
        J <- H/log(S)
        wien <- rbind(wien,c(H, J, S))
      }}
    
    #}
    #colnames(ent)<-c('empEntropy', 'evenness_log2', 'entropy', 'evenness')
    colnames(wien)<-c('ShannonWiener', 'Pielou', 'Richness')
    
    #add node
    g1 <- add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
    
    #add all links to node
    theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
    for(srclnk in theLinks[,1]){
      g1 <- add_edges(g1, c(which(V(g1)$id == srclnk), which(V(g1)$id == as.numeric(nodes[z,1]))))
    }
    
    #degd <- degree.distribution(g1)
    wtc <- cluster_walktrap(g1)
    struct <- rbind(struct, c(diameter(g1), edge_density(g1), modularity(wtc)))
  }
  
  #readr::write_csv(as.data.frame(ent), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICInfoTheory1.csv"),col_names = T)
  readr::write_csv(as.data.frame(wien), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICInfoTheory2.csv"),col_names = T)
  
  #scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
  
  
  # ent_plot <- as.data.frame(ent) %>%
  #   mutate(rownumber = seq.int(nrow(.)))
  
  wien_plot <- as.data.frame(wien) %>%
    mutate(rownumber = seq.int(nrow(.)))
  
  colnames(struct) <- c("diameter","density","modularity")
  struct[which(is.nan(struct))]<-0
  readr::write_csv(as.data.frame(struct), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICStructFeatures.csv"),col_names = T)
  
  colnames(coordinates) <- c("t","specificity","diversity")
  readr::write_csv(as.data.frame(coordinates), paste0("../../output/", projectDir,"/",outputDir,"/createTIC/TICCoordinates.csv"),col_names = T)
  
  struc_plot <- as.data.frame(struct) %>%
    mutate(rownumber = seq.int(nrow(.)))
  
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
}