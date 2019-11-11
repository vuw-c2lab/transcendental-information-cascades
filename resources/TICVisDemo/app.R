#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(networkD3)
library(htmlwidgets)
library(threejs)
library(igraph)
library(jsonlite)

links <- readr::read_csv("www/links.csv",col_names = T)
#links$value <- links$token
links$value <- 1
#links$source <- links$source-1
#links$target <- links$target-1
nodes <- readr::read_csv("www/nodes.csv",col_names = T)
nodes$names <- as.character(nodes$node_id)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$script(src="https://unpkg.com/three"),
        tags$script(src="https://unpkg.com/three-spritetext"),
        tags$script(src="https://unpkg.com/3d-force-graph")
    ),
        #tags$script(src="https://unpkg.com/3d-force-graph-vr")),
    
    tags$script(src="script.js"),
    
    titlePanel("TIC Vis Prototype"),
    
    sidebarLayout(
        sidebarPanel(
            actionButton("bwd", "<<"),
            actionButton("fwd", ">>")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("TIC Network", sankeyNetworkOutput("sankey")),
                tabPanel("3D TIC", tags$div(id = "force"), plotOutput("tic3d"))#,
                #tabPanel("VR TIC", tags$div(id = "ticVR"), plotOutput("ticVR"))
            )
        )
    ),
    
    tags$script(src="tic3d.js")#,
    #tags$script(src="ticVR.js")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    step <- 1
    slice <- 100
    bwd <- 0
    fwd <- 0
    
    subNodes <- NULL
    subLinks <- NULL
    
    subNodes3D <- NULL
    subLinks3D <- NULL
    
    subNodes3DOld <- NULL
    subLinks3DOld <- NULL
    
    D3obj <- NULL
    newNodes <- NULL
    remNodes <- NULL
    
    refresh <- F
    
    fixNodeIndex <- function(nodes,links){
        
        for(i in 1:nrow(links)){
            oldIndexSource <- links$source[i]
            oldIndexTarget <- links$target[i]
            
            newIndexSource <- unlist(nodes[which(nodes$node_id==oldIndexSource),5])[1]
            newIndexTarget <- unlist(nodes[which(nodes$node_id==oldIndexTarget),5])[1]
            
            links$source[i] <- newIndexSource
            links$target[i] <- newIndexTarget
        }
        
        return(links)
    }
    
    refreshSlice <- function(){
        subNodes3DOld <<- subNodes3D
        subLinks3DOld <<- subLinks3D
        
        if(step >=2 & input$bwd[1] > bwd){
            step <<- step - 1
            bwd <<- input$bwd[1]
            fwd <<- input$fwd[1]
            refresh <<- T
            print("go back")
        }  else if(input$fwd[1] > fwd){
            step <<- step + 1
            bwd <<- input$bwd[1]
            fwd <<- input$fwd[1]
            refresh <<- T
            print("go fwd")
        } else if(step < 2){
            bwd <<- input$bwd[1]
            fwd <<- input$fwd[1]
            step <<- 1
            refresh <<- F
            print("reset")
        } else {
            bwd <<- input$bwd[1]
            fwd <<- input$fwd[1]
            #step <<- 1
            refresh <<- F
            print("otherwise")
        }
    }
    
    sankey_reactive <- eventReactive( c(input$fwd,input$bwd), {
        
        refreshSlice()
        
        subLinks <<- links[which(links$source>=step & links$target<(step+slice)),]
        subNodes <<- nodes[which(nodes$node_id>=min(subLinks$source) & nodes$node_id<=max(subLinks$target)),]
        subNodes$fixedIndex <<- c(0:(nrow(subNodes)-1))
        
        subLinks <<- fixNodeIndex(subNodes,subLinks)
        
        subLinks3D <<- links[which(links$source>=step & links$target<(step+slice)),]
        subLinks3D <<- aggregate(token ~ .,subLinks3D,paste, collapse = ", ")
        
        subNodes3D <<- nodes[which(nodes$node_id>=min(subLinks3D$source) & nodes$node_id<=max(subLinks3D$target)),]
        subNodes3D$fixedIndex <<- c(0:(nrow(subNodes3D)-1))
        
        keepIds <- subNodes3D$node_id
        
        subNodes3D$realIds <<- keepIds
        
        subLinks3D <<- fixNodeIndex(subNodes3D,subLinks3D)
        subNodes3D$node_id <<- subNodes3D$fixedIndex
        
        #what was in old nodes and is now gone?
        if(!is.null(subNodes3DOld)){
            from <- unlist(subNodes3DOld$realIds)
            to <- unlist(subNodes3D$realIds)
            remNodes <<- unlist(subNodes3DOld[!(from %in% to),1])
        }

        g <- graph_from_data_frame(data.frame(source=subLinks3D$source,target=subLinks3D$target),vertices = subNodes3D)
        E(g)$value <- subLinks3D$token
        V(g)$name <- as.numeric(V(g)$name)
        V(g)$label <- keepIds
        v <- V(g)$label
        
        wc <- cluster_walktrap(g)
        members <- membership(wc)
        members <- keepIds
        
        d3graph <- igraph_to_networkD3(g, group = members)
        #print(d3graph)
        
        D3obj <<- toJSON(d3graph)
        D3obj <<- gsub(':([0-9]+)',':\\"\\1\\"',D3obj)
        D3obj <<- gsub("name","id",D3obj)
        #cat(x)
        fileConn<-file("www/graph.json")
        writeLines(D3obj, fileConn)
        close(fileConn)
        
        sankeyNetwork(Links = subLinks, Nodes = subNodes, Source = "source",
                      Target = "target", Value = "value", NodeID = "names",
                      units = "", fontSize = 10, nodeWidth = 2, nodePadding = 5, height = 1050, width = 1050)
    })
    
    tic3d_reactive <- eventReactive( c(input$fwd,input$bwd), {
        
        refreshSlice()
        
        subLinks3D <<- links[which(links$source>=step & links$target<(step+slice)),]
        subLinks3D <<- aggregate(token ~ .,subLinks3D,paste, collapse = ", ")
        
        subNodes3D <<- nodes[which(nodes$node_id>=min(subLinks3D$source) & nodes$node_id<=max(subLinks3D$target)),]
        subNodes3D$fixedIndex <<- c(0:(nrow(subNodes3D)-1))
        
        keepIds <- subNodes3D$node_id
        
        subNodes3D$realIds <<- keepIds
        
        subLinks3D <<- fixNodeIndex(subNodes3D,subLinks3D)
        subNodes3D$node_id <<- subNodes3D$fixedIndex
        
        #what was in old nodes and is now gone?
        if(!is.null(subNodes3DOld)){
            from <- unlist(subNodes3DOld$realIds)
            to <- unlist(subNodes3D$realIds)
            remNodes <<- unlist(subNodes3DOld[!(from %in% to),1])
        }
        
        g <- graph_from_data_frame(data.frame(source=subLinks3D$source,target=subLinks3D$target),vertices = subNodes3D)
        E(g)$value <- subLinks3D$token
        V(g)$name <- as.numeric(V(g)$name)
        V(g)$label <- keepIds
        v <- V(g)$label
        
        wc <- cluster_walktrap(g)
        members <- membership(wc)
        members <- keepIds
        
        d3graph <- igraph_to_networkD3(g, group = members)
        #print(d3graph)
        
        D3obj <<- toJSON(d3graph)
        D3obj <<- gsub(':([0-9]+)',':\\"\\1\\"',D3obj)
        D3obj <<- gsub("name","id",D3obj)
        #cat(x)
        fileConn<-file("www/graph.json")
        writeLines(D3obj, fileConn)
        close(fileConn)
        #(graphjs(g, vertex.shape=V(g)$label,vertex.size = 1,edge.width = 2,edge.color = "gray87", bg = "black", vertex.color = "gray87"))
        #(graphjs(g, vertex.shape="sphere",vertex.size = 1,edge.width = 1,edge.color = "gray75", vertex.color = "gray50"))
        #
        #
        observe({
            session$sendCustomMessage("update-graph", list(remNodes,newNodes))
            remNodes <<- NULL
            #invalidateLater(3000)
        })
    })
    
    output$sankey <- renderSankeyNetwork({
        sankey_reactive()
    })
    
    output$tic3d <- renderPlot({
        tic3d_reactive()
    })
    
    # output$ticVR <- renderPlot({
    #     tic3d_reactive()
    # })
    
    observe({
        # Send the next color to the browser
        session$sendCustomMessage("replace-labels", list(subNodes,subLinks))
        # Update the color every 100 milliseconds
        invalidateLater(1000)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
