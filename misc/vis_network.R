
library("visNetwork") 
library(igraph)
library(shiny)
library(dplyr)
# https://github.com/datastorm-open/visNetwork/issues/131
# visGetSelectedNodes

# https://github.com/datastorm-open/visNetwork/issues/50 # GREAT EXAMPLE

# https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton
# http://datastorm-open.github.io/visNetwork/shiny.html
server <- function(input, output, session) {

  n_nodes = 30
  ba <-  sample_pa(n=n_nodes, power=1, m=1,  directed=F)
  #plot(ba, vertex.size=6, vertex.label=NA)
  
  # get.edgelist(ba)
  links = igraph::as_data_frame(ba, what="edges")  %>% mutate(group=1)
  nodes = data.frame(id=1:n_nodes) # as_data_frame(ba, what="vertices")%>% mutate(group=1)
  #net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
  values <- reactiveValues()
  values$links = links
  values$nodes = nodes
  values$selected_nodes = NULL
  
  vis.nodes <- nodes
  vis.links <- links
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$id # Text on click
  vis.nodes$label  <- vis.nodes$id # Node label
  vis.nodes$size   <- vis.nodes$audience.size # Node size
  vis.nodes$borderWidth <- 2 # Node border width
  
  vis.nodes$color.background <- rep(c("slategrey", "tomato", "gold"), times=20)[1:nrow(nodes)]  # [nodes$media.type]
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  output$network <- renderVisNetwork({
    
    # withProgress(message = 'drawing the network...', value = 0.1, {
    #   Sys.sleep(0.25)
      
      VisNetwork_graph <- visNetwork::visNetwork(vis.nodes, vis.links, width="100%", height="800px", #background="#eeefff",
               main="Network", submain="And what a great network it is!",
               footer= "Hyperlinks and mentions among media sources") %>% 
      visEdges(arrows = "to", 
               color = list(color = "black",highlight ="red",hover = "blue")
      ) %>%    
      visHierarchicalLayout() %>%  # direction = "LR"

      # visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
      # visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
      # addFontAwesome() %>%
      # visLegend(addNodes = data.frame(label = c("A", "B"), shape = "icon", 
      #                                 icon.code = c("f0c0", "f007"), 
      #                                 icon.size = c(25,50), 
      #                                 icon.color = c(NA, "red")),
      #         addEdges = data.frame(label = "link"), useGroups = FALSE)
      
    
    
    #visEdges(color = list(color = "white",highlight ="red",hover = "blue")) %>%
    #visInteraction(hover = T) %>% 
    # visSetSelection((edgesId = edges_selection,nodesId=nodes_selection))
    visInteraction(multiselect=T,selectable=T)  %>% 
      
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                ;}"
      )
    
    visNetwork::visOptions(graph= VisNetwork_graph, manipulation = TRUE,
                           selectedBy = "group",
                           highlightNearest = TRUE )
    
  #}) #end of draw
})
  
  
  
  observe({
    g <- visNetworkProxy("network")  
    validate(need(g, message=FALSE), 
             need(input$click, message=FALSE)
    )
    
    # ShortPth <- get.shortest.paths(g, 
    #                                values$selected_nodes[1], 
    #                                values$selected_nodes[2]
    #                                )    # List of path 8->2
    # 
    # E(g)$color <- "SkyBlue2"
    # E(g)$width <- 1
    #E(g, path=ShortPth)$color <- "red" 
    
    #E(g, path=ShortPth$vpath[[1]])$color <- "red"
    
    visNetworkProxy("network") %>% 
      visUpdateNodes(nodes = data.frame(id=input$click, color.background =  "red"))
    #E(g, path=unlist(ShortPth$vpath))$color <- "red"
    #E(g, path=unlist(ShortPth[[1]]))$color <- "red"
    #plot(g)
    
    #visUpdateNodes and visUpdateEdges. Related to #50
    
    
    
  })
  
  observe({
    graph <- visNetwork::visNetworkProxy("network")
    if(input$enableHierarchiId){
      #conditionalPanel("input.enableHierarchiId==true",
      visHierarchicalLayout(graph, enabled= input$enableHierarchiId,
                            direction = input$Hierarchi_AttId,
                            sortMethod= input$MethodHierarchiId
      )
      #)
    }else if(input$changeGroupId){
      nodes <- data.frame(id = 1:12,
                          label = paste("Node", 1:12),
                          value = c(10,5,3,7,2,3,2.6,2,9,2,11,12),
                          shape ="circle",
                          group= c("Gr1","Gr2","Gr3"),
                          color= c("#DF0101","orange" ,"blue"))
      visNetwork::visUpdateNodes(graph, nodes)
      
    }else{
      #conditionalPanel("input.enableHierarchiId==false",
      visNetwork::visPhysics(graph, solver = input$PhysicLayoutId )
      
      #)
    }
  })
    
    
  observe({ 
    #visNearestNodes(target = input$click, maxpoints =10)
    #visGetSelectedNodes() #input = paste0(network$id, "_selectedNodes"))
    #visNetworkProxy("network") %>% visGetSelectedNodes(input="network_selectedNodes")
    # https://github.com/datastorm-open/visNetwork/issues/87
    values$selected_nodes = unique(c(values$selected_nodes, input$click))
    visNetworkProxy("network") %>%
      visSelectNodes(id = values$selected_nodes)
    #visSelectEdges(id = edges_selection)
  })
  
  
  output$shiny_return <- renderPrint({
    values$links %>% filter(from %in% input$click | to  %in% input$click)
  })
  }



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Edges Attributes:"),
      
      enableHierarchi <- c("FALSE","TRUE"),
      PhysicLayout <- c('barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based'),
      MethodHierarchi <- c("hubsize", "directed"),
      Hierarchi_Att <- c("LR","RL","UD","DU"),
      
      wellPanel(
        
        checkboxInput("enableHierarchiId",label="Hierarchi", value = FALSE),
        
        
        selectizeInput("MethodHierarchiId",label="Layout Method", choices = MethodHierarchi,
                       selected= "hubsize", multiple = FALSE),
        
        selectizeInput("Hierarchi_AttId", label = "Direction", choices = Hierarchi_Att, multiple = FALSE),
        
        selectizeInput("PhysicLayoutId",label="Physics Options", choices = PhysicLayout,
                       selected= 'hierarchicalRepulsion', multiple = FALSE)
      ),
      
      checkboxInput("changeGroupId",label="add Gr2", value = FALSE),
      
      h6("or (not working)"),
      selectizeInput("changeGroupIdbis",label="Select node for Gr2", choices = nodes <- (id = 1:12), selected= NULL, multiple = TRUE),
      
      checkboxInput("RunId",label="Plot", value = FALSE)
    ),
    
    mainPanel(
      # conditionalPanel(condition ="input.RunId==true",
      #                  visNetworkOutput("network")
      # ),
      visNetworkOutput("network"), 
      verbatimTextOutput("shiny_return")
      
       # if(input$RunId){
       #  visNetworkOutput("network")
       # }else{}
      
    )
  )   
)

shiny::shinyApp(ui = ui, server = server)


