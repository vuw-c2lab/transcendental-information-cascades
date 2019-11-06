if (!require(dplyr)) install.packages('dplyr')
if (!require(networkD3)) install.packages('networkD3')
if (!require(tidyr)) install.packages('tidyr')
if (!require(igraph)) install.packages('igraph')
if (!require(arcdiagram)) devtools::install_github('gastonstat/arcdiagram')

# PREAMBLE ----------------------------------------------------------------

dir.create(file.path(paste0("../../output/",projectDir,"/",outputDir), "visualiseTIC"), showWarnings = FALSE)


# MAIN FUNCTION -----------------------------------------------------------

visualiseTIC <- function(projectDir, outputDir, dataSource, tokeniser,no_cores=1){
  print("## vis TIC ##")
  
  # persist TIC to files (optional but needed if post-processing doesn't happen immediately)
  nodes <- readr::read_csv(paste0("../../output/", projectDir,"/",
                                         outputDir,"/createTIC/nodes.csv"),col_names = T)
  links <- readr::read_csv(paste0("../../output/", projectDir,"/",
                                         outputDir,"/createTIC/links.csv"),col_names = T)
  # links$value<-1
  # networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
  #                          Source = 'source', 
  #                          Target = 'target', 
  #                          Value = 'value', 
  #                          NodeID = 'node_id',
  #                          units = 'token',
  #                          fontSize = 10,
  #                          nodeWidth = 10) %>% saveNetwork(file = "Net1.html")
  #                          
  
  g <- igraph::graph_from_edgelist(as.matrix(data.frame(source=links$source,target=links$target)))
  g.edges <- igraph::get.edgelist(g)
  
  #arcdiagram::arcplot(head(g.edges,500))
  
  V(g)$size <- 8
  V(g)$frame.color <- "white"
  V(g)$color <- "orange"
  V(g)$label <- nodes$node_id 
  E(g)$arrow.mode <- 0
  l <- layout_with_graphopt(g, charge=0.00000001)
  plot(g, layout=l)
}


# SUPPORT FUNCTIONS -------------------------------------------------------
