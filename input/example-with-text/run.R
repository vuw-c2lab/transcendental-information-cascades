

# PREAMBLE ----------------------------------------------------------------

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(igraph)) install.packages('igraph')
if (!require(gtools)) install.packages('gtools')
if (!require(tools)) install.packages('tools')

# set available CPU cores
no_cores <- detectCores() - 1

# flag to use cuda GPU processing if available
cuda <- F

# CONFIG ------------------------------------------------------------------

# set the working directory to the desired project directory in input
setwd("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/input/example-with-text/")

# PIPELINE 1 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example.txt"

# select tokeniser
tokeniser <- "text/POS-nouns"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# persist TIC to files (optional but needed if post-processing doesn't happen immediately)
nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                    ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                    token = unlist(tic[[2]][,3]), stringsAsFactors = F)
readr::write_csv(nodes,paste0("../../output/", projectDir,"/",
                              outputDir,"/createTIC/nodes.csv"),col_names = T)
readr::write_csv(links,paste0("../../output/", projectDir,"/",
                                   outputDir,"/createTIC/links.csv"),col_names = T)

# create TIC network matrix and persist matrix

colnames(links) <- c("id1","id2","label")
g <- igraph::graph.data.frame(links,directed = TRUE,vertices = nodes)
readr::write_csv(as.data.frame(igraph::as_adjacency_matrix(g, sparse = F)), paste0("../../output/", projectDir,"/",
                                                                    outputDir,"/createTIC/TICmatrix.csv"),col_names = T)

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

## post process TIC network

## visualise process TIC network

# PIPELINE 2 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example.txt"

# select tokeniser
tokeniser <- "text/POS-verbs"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser,no_cores)

# persist TIC to files (optional but needed if post-processing doesn't happen immediately)
nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                    ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                    token = unlist(tic[[2]][,3]), stringsAsFactors = F)
readr::write_csv(nodes,paste0("../../output/", projectDir,"/",
                              outputDir,"/createTIC/nodes.csv"),col_names = T)
readr::write_csv(links,paste0("../../output/", projectDir,"/",
                              outputDir,"/createTIC/links.csv"),col_names = T)

# create TIC network matrix and persist matrix

colnames(links) <- c("id1","id2","label")
g <- igraph::graph.data.frame(links,directed = TRUE,vertices = nodes)
readr::write_csv(as.data.frame(igraph::as_adjacency_matrix(g, sparse = F)), paste0("../../output/", projectDir,"/",
                                                                                   outputDir,"/createTIC/TICmatrix.csv"),col_names = T)

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

# PIPELINE 3 --------------------------------------------------------------

## create TIC
# get project name from current directory and create output directory for project
projectDir <- basename(getwd())
dir.create(file.path(paste0("../../output/"), projectDir), showWarnings = FALSE)

# data source
dataSource <- "example.txt"

# select tokeniser
tokeniser <- "text/POS-adjectives"

# create output directories for the different modules in this pipeline
outputDir <- paste0(tools::file_path_sans_ext(dataSource),"-",gsub("/","-",tokeniser),"-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
dir.create(file.path(paste0("../../output/",projectDir), outputDir), showWarnings = T)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "createTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICNetwork"), showWarnings = FALSE)
dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "postProcessTICMultiplex"), showWarnings = FALSE)

# run
source("../../src/createTIC/main.R")
tic <- createTIC(projectDir, outputDir, dataSource, tokeniser)

# persist TIC to files (optional but needed if post-processing doesn't happen immediately)
nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tokens = unlist(tic[[1]][,2]),
                    ord = unlist(tic[[1]][,3]), stringsAsFactors = F)
links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                    token = unlist(tic[[2]][,3]), stringsAsFactors = F)
readr::write_csv(nodes,paste0("../../output/", projectDir,"/",
                              outputDir,"/createTIC/nodes.csv"),col_names = T)
readr::write_csv(links,paste0("../../output/", projectDir,"/",
                              outputDir,"/createTIC/links.csv"),col_names = T)

# create TIC network matrix and persist matrix

colnames(links) <- c("id1","id2","label")
g <- igraph::graph.data.frame(links,directed = TRUE,vertices = nodes)
readr::write_csv(as.data.frame(igraph::as_adjacency_matrix(g, sparse = F)), paste0("../../output/", projectDir,"/",
                                                                                   outputDir,"/createTIC/TICmatrix.csv"),col_names = T)

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


# PIPELINE 4 --------------------------------------------------------------

## post process TIC multiplex from pipelines 1 - 3

#which TICs to comnbine?

