#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
        tags$script(src="https://unpkg.com/3d-force-graph"),
        tags$script(src="https://unpkg.com/3d-force-graph-vr")),
    
    tags$script(src="script.js"),
    
    titlePanel(""),
    
    sidebarLayout(
        sidebarPanel(
            actionButton("bwd", "<<"),
            actionButton("fwd", ">>"),
            textInput("slice", "Slice", "50")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("TIC Network", sankeyNetworkOutput("sankey")),
                tabPanel("3D TIC", tags$div(id = "force"), plotOutput("tic3d")),
                tabPanel("VR TIC", tags$div(id = "ticVR"), plotOutput("ticVR"))
            )
        )
    ),
    
    tags$script(src="tic3d.js"),
    tags$script(src="ticVR.js")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    step <- 1
    bwd <- 0
    fwd <- 0
    
    subNodes <- NULL
    subLinks <- NULL
    
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
    
    output$value <- renderText({ input$slice })
    
    sankey_reactive <- eventReactive( c(input$fwd,input$bwd,input$caption), {
        
        if(step >=2 & input$bwd[1] > bwd){
            step <<- step - 1
            bwd <<- bwd + 1
            fwd <<- input$fwd[1]
        }  else if(input$fwd[1] > fwd){
            step <<- step + 1
            fwd <<- fwd +1
            bwd <<- input$bwd[1]
        } else {
            bwd <<- input$bwd[1]
            fwd <<- input$fwd[1]
            step <<- 1
        }
        
        if(as.numeric(input$slice)<5){
            slice <- 5
        } else if(as.numeric(input$slice)>50){
            slice <- 50
        } else{
            slice <- as.numeric(input$slice)
        }
        subLinks <<- links[which(links$source>=step & links$target<(step+slice)),]
        subNodes <<- nodes[which(nodes$node_id>=min(subLinks$source) & nodes$node_id<=max(subLinks$target)),]
        subNodes$fixedIndex <<- c(0:(nrow(subNodes)-1))
        
        subLinks <<- fixNodeIndex(subNodes,subLinks)
        
        sankeyNetwork(Links = subLinks, Nodes = subNodes, Source = "source",
                      Target = "target", Value = "value", NodeID = "names",
                      units = "", fontSize = 10, nodeWidth = 5, nodePadding = 10, height = 500, width = 900)
    })
    
    tic3d_reactive <- eventReactive( c(input$fwd,input$bwd,input$caption), {
        fewLinks <- head(links,30)

        if(step >=2 & input$bwd[1] > bwd){
            step <<- step - 1
            bwd <<- bwd + 1
            fwd <<- input$fwd[1]
        }  else if(input$fwd[1] > fwd){
            step <<- step + 1
            fwd <<- fwd +1
            bwd <<- input$bwd[1]
        } else {
            bwd <<- input$bwd[1]
            fwd <<- input$fwd[1]
            step <<- 1
        }

        if(as.numeric(input$slice)<5){
            slice <- 5
        } else if(as.numeric(input$slice)>50){
            slice <- 50
        } else{
            slice <- as.numeric(input$slice)
        }
        subLinks3D <- links[which(links$source>=step & links$target<(step+slice)),]
        subNodes3D <- nodes[which(nodes$node_id>=min(subLinks$source) & nodes$node_id<=max(subLinks$target)),]

        g <- graph_from_data_frame(data.frame(source=subLinks3D$source,target=subLinks3D$target))
        E(g)$value <- links$token
        V(g)$name <- as.numeric(V(g)$name)-1
        V(g)$label <- V(g)$name
        v <- V(g)$label
        
        wc <- cluster_walktrap(g)
        members <- membership(wc)
        
        d3graph <- igraph_to_networkD3(g, group = members)
        #print(d3graph)
        
        x <- toJSON(d3graph)
        x <- gsub(':([0-9]+)',':\\"\\1\\"',x)
        x <- gsub("name","id",x)
        #cat(x)
        fileConn<-file("www/graph.json")
        writeLines(x, fileConn)
        close(fileConn)
        #(graphjs(g, vertex.shape=V(g)$label,vertex.size = 1,edge.width = 2,edge.color = "gray87", bg = "black", vertex.color = "gray87"))
        #(graphjs(g, vertex.shape="sphere",vertex.size = 1,edge.width = 1,edge.color = "gray75", vertex.color = "gray50"))
    })
    
    output$sankey <- renderSankeyNetwork({
        sankey_reactive()
    })
    
    output$tic3d <- renderPlot({
        tic3d_reactive()
    })
    
    output$ticVR <- renderPlot({
        tic3d_reactive()
    })
    
    observe({
        # Send the next color to the browser
        session$sendCustomMessage("replace-labels", list(subNodes,subLinks))
        
        # Update the color every 100 milliseconds
        invalidateLater(1000)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
