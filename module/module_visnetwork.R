

library("visNetwork") 
library(igraph)
library(shiny)
library(dplyr)


create_network <- function(n_nodes=20) { 
  ba <-  sample_pa(n=n_nodes, power=1, m=1,  directed=F)
  #plot(ba, vertex.size=6, vertex.label=NA)
  
  # get.edgelist(ba)
  edges = igraph::as_data_frame(ba, what="edges")  %>% mutate(group=1)
  nodes = data.frame(id=1:n_nodes) # as_data_frame(ba, what="vertices")%>% mutate(group=1)
  #net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T) 

  
  nodes$shape  <- "dot"  
  nodes$shadow <- TRUE # Nodes will drop shadow
  nodes$title  <- nodes$id # Text on click
  nodes$label  <- nodes$id # Node label
  #nodes$size   <- nodes$audience.size # Node size
  nodes$borderWidth <- 2 # Node border width
  nodes$group   <-sample(1:3, size=nrow(nodes), replace=TRUE) # Node size
  
  nodes$color.background <- rep(c("slategrey", "tomato", "gold"), times=20)[1:nrow(nodes)]  # [nodes$media.type]
  nodes$color.border <- "black"
  nodes$color.highlight.background <- "orange"
  nodes$color.highlight.border <- "darkred"
 
  return(list(nodes=nodes, edges=edges)) 
}


create_model_library <- function(n_model = 20) { 
  
  nodes0 = data.frame(
  rowid = 1:n_model, 
  model = paste0("model_",add_prefix(1:n_model, digits=3)), 
  company = paste0("company_",add_prefix(1:1, digits=3)), 
  author = paste0("modeler_", sample(toupper(letters[1:6]), n_model, replace=TRUE)), 
  area = paste0("area_", sample(toupper(letters[1:5]), n_model, replace=TRUE)), 
  feature =  sample(c("linear", "MM-elimation", "TMDD", "PKPD", "dose-response"), n_model, replace=TRUE),
  compartment =  paste0("comp_",sample(1:6, n_model, replace=TRUE)),
  version = paste0("version_", sample(add_prefix(1:10, digits=2), n_model, replace=TRUE)), 
  year =  sample(2000:2019, n_model, replace=TRUE) 
) %>% 
  mutate(
    program = paste0(area, "_", 
                     paste0("P", sample(add_prefix(1:30, digits=3), n_model, replace=TRUE))
    ), 
  
    study = paste0(program, "_", 
                   paste0("S", sample(add_prefix(1:100, digits=3), n_model, replace=TRUE))
    )
  )
  
  nodes0 <- nodes0 %>% arrange(area, author, program, study, feature,  compartment,    version, year )
  return(nodes0)
  
}


create_nodes_edges <- function(nodes0, level_lst=c("program", "author", "model")) { 
 
  #ba <-  sample_pa(n=n_nodes, power=1, m=1,  directed=F)
  #plot(ba, vertex.size=6, vertex.label=NA)
  
  # get.edgelist(ba)
  #edges = igraph::as_data_frame(ba, what="edges")  %>% mutate(group=1)
  #nodes = data.frame(id=1:n_model) # as_data_frame(ba, what="vertices")%>% mutate(group=1)
  #net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T) 

  #level_lst <- c("program", "author") #,  "study", "feature", "compartment", "version", "year")
  #level_lst <- c(level_lst, "model")
    
  nodes2 <- nodes0 %>% tidyr::gather(key, value, one_of( level_lst), -rowid)
  nodes2 <- nodes2 %>% mutate(
    id = as.integer(as.factor(paste0(key,"_", value))),
    key = ordered(key, levels=level_lst), 
    level = as.integer(key)#, 
    #level = ifelse(is.na(level), 3, level)
  ) %>% arrange( key, value )  #%>% head(n=20)
  
  # nodes 
  nodes <- nodes2 %>% distinct(id, .keep_all=TRUE) #[which(!duplicated(nodes2$id)),]
  
  # edges
  nodes2 <- nodes2 %>% group_by(rowid) %>% 
    mutate(from_to =ifelse(duplicated(rowid) , paste0(id, collapse="_"), NA)) %>% 
    as.data.frame() %>% ungroup()
    
  library(stringr)
  tt = stringr::str_split_fixed(nodes2$from_to[!is.na(nodes2$from_to)], "_", length(level_lst))
  edges = NULL
  for (i in 1:(ncol(tt)-1)) {
    edges = rbind(edges, tt[, i:(i+1)])  
  }
  colnames(edges) = c("from", "to")
  edges = edges %>% as.data.frame()
  edges = edges %>% mutate(from_to=paste0(from,"_", to)) %>% distinct(from_to, .keep_all = TRUE)
  
    
  nodes$shape  <- "dot"  
  nodes$shadow <- TRUE # Nodes will drop shadow
  nodes$title  <- nodes$value # Text on click
  nodes$label  <- nodes$value # Node label
  #nodes$size   <- nodes$audience.size # Node size
  nodes$borderWidth <- 2 # Node border width
  #nodes$group   <-sample(1:3, size=nrow(nodes), replace=TRUE) # Node size
  
  nodes$color.background <- rep(c("slategrey", "tomato", "gold"), times=20)[1:nrow(nodes)]  # [nodes$media.type]
  nodes$color.border <- "black"
  nodes$color.highlight.background <- "orange"
  nodes$color.highlight.border <- "darkred"
  
  nodes <- nodes %>% select(-value, -key)
  
    
  return(list(nodes=nodes, edges=edges)) 
}


# #tt=create_network(n_nodes=20)
# tt = create_nodes_edges(create_model_library(n_model=20), 
#                     level_lst=c("year", "model")
#                     )  # "program", "modeler
                         
# visNetwork::visNetwork(
#   tt$nodes, 
#   tt$edges, width = "100%", height="1800px"
#   #main="Network", submain="And what a great network it is!",
#   #footer= "Hyperedges and mentions among media sources"
# ) %>% 
#   visEdges(arrows = "to", 
#            color = list(color = "black",highlight ="red",hover = "blue")
#   )    %>%  
#   visHierarchicalLayout(enabled= TRUE,
#                       direction = "UD",   
#                        sortMethod=  "directed"
#   )
# 


#######################################################################
# module_build_adsl_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------
module_visnetwork_model_library_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  fluidPage(
    #titlePanel("VisNetwork + shiny"),
    fluidRow(
      column(4, 
          checkboxInput(ns("enableHierarchiId"),label="Hierarchi", value = FALSE),
          uiOutput(ns("params_for_hierarchi_container")), 
          uiOutput(ns("params_for_nonhierarchi_container"))  
      ),  
      column(8, 
         visNetworkOutput(ns("network"))
      ) 
    )
  )
   
}

########################################################################
# module_runsim_adsl
########################################################################

module_visnetwork_model_library <- function(input, output, session)  {
  
  ns <- session$ns 
   
  values <- reactiveValues()
  values$selected_nodes = NULL
  
  #tt=create_network(n_nodes=20)
  tt = create_nodes_edges(create_model_library(n_model=100), 
                         level_lst=c("company", "area", "program", "model")
  )  # "program", "author
  
  values$nodes = tt$nodes    #tt$nodes
  values$edges = tt$edges    # tt$edges
  
  
  #--------------------------------------  
  # params_for_hierarchi_container
  #--------------------------------------
  output$params_for_hierarchi_container <- renderUI({ 
   validate(need(input$enableHierarchiId, message=FALSE))
    
    enableHierarchi <- c("FALSE","TRUE") 
    Hierarchi_Att <- c("LR","RL","UD","DU") 
    MethodHierarchi <- c("hubsize", "directed") 
    
    tagList(  
      selectizeInput(ns("MethodHierarchiId"),
                     label="Layout Method", 
                     choices = MethodHierarchi,
                    selected= "directed", 
                    multiple = FALSE
                    ),
      
      selectizeInput(ns("Hierarchi_AttId"), 
                     label = "Direction", 
                     choices = Hierarchi_Att, 
                     selected = "UD",
                     multiple = FALSE
                     )
    )
    
  })
  
  output$params_for_nonhierarchi_container <- renderUI({ 
    validate(need(!input$enableHierarchiId, message=FALSE))
    
    PhysicLayout <- c('barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based') 
    
    selectizeInput(ns("PhysicLayoutId"),label="Physics Options", choices = PhysicLayout,
                 selected= 'barnesHut', multiple = FALSE)
  
  })
  
  
  output$network <- renderVisNetwork({
    #  nodes <- data.frame(id = 1:10, label = paste("Label", 1:10),
    #                     group = sample(c("A", "B"), 10, replace = TRUE))
    #edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
    nodes = values$nodes    #tt$nodes
    edges = values$edges    # tt$edges
      
    validate(need(nodes, message = FALSE), 
             need(edges, message =FALSE)
    )
     
    
    withProgress(message = 'drawing the network...', value = 0.1, {
      Sys.sleep(0.25)
      
      # nodes <- data.frame(id = 1:10,
      #                     label = paste("Node", 1:10),
      #                     value = c(10,5,3,7,2,3,2.6,2,9,2),
      #                     shape ="circle",
      #                     group= c("Gr1","Gr3"),
      #                     color= c("#DF0101", "blue")
      # )
      # 
      # edges <- data.frame(from = c(1,5,3,7,2,9,10,8,3,3,6),
      #                     to = c(5,3,5,1,5,1,2,3,7,4,9) )
      #tt = create_network(n_nodes=20)
      
      
      #tt=create_network(n_nodes=20)
      #tt = create_nodes_edges(create_model_library(n_model=20), 
       #                       level_lst=c("year", "model")
      #)  # "program", "author
      
      

      #Shiny.onInputChange('click', nodes.nodes[0]);
      #"Shiny.onInputChange(\"click\",  this.id, nodes.nodes[0]);",
      #sprintf('Shiny.onInputChange("%s",  this.id',  session$ns("click")), ", nodes.nodes[0]);", 
      #"Shiny.onInputChange(session$ns('click'), nodes.nodes[0]);",
      graphe <- visNetwork::visNetwork(
        nodes, edges, width = "100%", height="1800px"
        #main="Network", submain="And what a great network it is!",
        #footer= "Hyperedges and mentions among media sources"
        ) %>% 
        visEdges(arrows = "to", 
                 color = list(color = "black",highlight ="red",hover = "blue")
        ) %>% 
        
        visInteraction(multiselect=T,selectable=T)  %>% 
        
        #https://github.com/datastorm-open/visNetwork/issues/241
        visEvents(click = paste0("function(nodes){ 
                  Shiny.onInputChange('", ns("click"), "', nodes.nodes[0]);
                  ;}"), 
                  
                  doubleClick  = paste0("function(nodes){ 
                  Shiny.onInputChange('", ns("double_click"), "', nodes.nodes[0]);
                                        ;}")  
        )
      
       visNetwork::visOptions(graph= graphe, manipulation = TRUE,
                             selectedBy = "author"
                             #highlightNearest = TRUE
                             )
      
  })
  })
  
  
  output$shiny_return <- renderPrint({
    values$edges %>% filter(from %in% input$click | to  %in% input$click)
  })
  
  
  
  observeEvent({input$click}, { 
    #visNearestNodes(target = input$click, maxpoints =10)
    #visGetSelectedNodes() #input = paste0(network$id, "_selectedNodes"))
    #visNetworkProxy("network") %>% visGetSelectedNodes(input="network_selectedNodes")
    # https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html
    values$selected_nodes = unique(c(values$selected_nodes, input$click))
    
    # https://stackoverflow.com/questions/19827139/highlight-shortest-path-on-plot-of-graph/19996189
    
    #graph <- visNetwork::visNetworkProxy("network")
    graph <-  sample_pa(n=20, power=1, m=1,  directed=F)
    
    #  ShortPth <- get.shortest.paths(graph, 
    #                                 values$selected_nodes[1], 
    #                                 values$selected_nodes[2]
    #                                 )    # List of path 8->2
    #  
    #  E(graph)$color <- "SkyBlue2"
    #  E(graph)$width <- 4 
    #  print("OK here")
    #  #E(graph, path=ShortPth$vpath[[1]])$color <- "red"
    #  E(graph, path=unlist(ShortPth$vpath))$color <- "red"
    #  
    #  edges_selection = E(graph, path=unlist(ShortPth$vpath))
    #  
    visNetworkProxy(ns("network")) %>%
      visSelectNodes(id =  (values$selected_nodes))   # %>% 
    #   visSelectEdges(id = edges_selection)
  })
  
  observeEvent({input$double_click}, { 
    values$selected_nodes = setdiff(values$selected_nodes, input$double_click)
    
    visNetworkProxy(ns("network")) %>%
      visSelectNodes(id =  (values$selected_nodes))   # %>% 
    
    
  })
  
  
  
  observe({
    validate(need(input$enableHierarchiId, message=FALSE)
    )
    graph <- visNetwork::visNetworkProxy(ns("network"))
     
    visHierarchicalLayout(graph, 
                          enabled= input$enableHierarchiId,
                          direction = input$Hierarchi_AttId,
                          sortMethod= input$MethodHierarchiId
    )
      
  })
  
  
  observe({
    validate(need(!input$enableHierarchiId, message=FALSE)
    )
    graph <- visNetwork::visNetworkProxy(ns("network"))
    visNetwork::visPhysics(graph, solver = input$PhysicLayoutId )
    
  })
  
}



if (1==1 ) {

server <- shinyServer(function(input, output) {
  callModule(module_visnetwork_model_library, "call_visnetwork")
})

ui <- shinyUI(
  fluidRow(
    column(12, 
           module_visnetwork_model_library_UI("call_visnetwork", label = NULL)#,
           #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
           )
)
)
       
shiny::shinyApp(ui = ui, server = server)
    

}



