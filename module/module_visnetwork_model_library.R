
# https://datastorm-open.github.io/visNetwork/shiny.html
# http://www.cse.unsw.edu.au/~mike/myrlibrary/visNetwork/NEWS
#visNearestNodes(target = input$click, maxpoints =10)
#visGetSelectedNodes() #input = paste0(network$id, "_selectedNodes"))
#visNetworkProxy("network") %>% visGetSelectedNodes(input="network_selectedNodes")
# https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html
# https://stackoverflow.com/questions/19827139/highlight-shortest-path-on-plot-of-graph/19996189

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
  model = paste0("model",add_prefix(1:n_model, digits=3)), 
  company = paste0("company",add_prefix(1:1, digits=3)), 
  author = paste0("modeler", sample(toupper(letters[1:6]), n_model, replace=TRUE)), 
  area = paste0("area", sample(toupper(letters[1:5]), n_model, replace=TRUE)), 
  feature =  sample(c("linear", "MM-elimation", "TMDD", "PKPD", "dose-response"), n_model, replace=TRUE),
  compartment =  paste0("comp",sample(1:6, n_model, replace=TRUE)),
  version = paste0("version", sample(add_prefix(1:10, digits=2), n_model, replace=TRUE)), 
  year =  sample(2000:2019, n_model, replace=TRUE) 
) %>% 
  mutate(
    program = paste0(area, "", 
                     paste0("P", sample(add_prefix(1:30, digits=3), n_model, replace=TRUE))
    ), 
  
    study = paste0(program, "", 
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
    
  nodes2 <- nodes0 %>% tidyr::gather(mykey, myvalue, one_of( level_lst), -rowid)
  nodes2 <- nodes2 %>% mutate(
    id = as.integer(as.factor(paste0(mykey,"_", myvalue))),
    mykey = ordered(mykey, levels=level_lst), 
    level = as.integer(mykey)#, 
    #level = ifelse(is.na(level), 3, level)
  ) %>% arrange( mykey, myvalue )  #%>% head(n=20)
  
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
  nodes$title  <- nodes$myvalue # Text on click
  nodes$label  <- nodes$myvalue # Node label
  #nodes$size   <- nodes$audience.size # Node size
  nodes$borderWidth <- 2 # Node border width
  #nodes$group   <-sample(1:3, size=nrow(nodes), replace=TRUE) # Node size
  
  
  nodes$color.border <- "black"
  nodes$color.highlight.background <- "orange"
  nodes$color.highlight.border <- "darkred"
   
  nodes <- nodes %>% mutate(
    color.background = ifelse(mykey=="model", "red", 
                              ifelse(mykey=="company", "gold", 
                                     ifelse(mykey=="area", "tomato", "slategrey")
                                     )
                              )# rep(c("slategrey", "tomato", "gold"), times=20)[1:nrow(nodes)]  # [nodes$media.type]
  )
   
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
   
  default_level_lst = c("area", "program")  # 
  choices <- c(default_level_lst, 
               setdiff(colnames(create_model_library(n_model=1)), 
                       c(default_level_lst, "company",   "model"))
  )
  
  fluidPage(
    #titlePanel("VisNetwork + shiny"),
    fluidRow(
      column(4,  
             
             selectizeInput(ns("level_lst"),
                            label="clustered by", 
                            choices = choices,
                            selected= default_level_lst, 
                            multiple = TRUE
                            ),
             
             uiOutput(ns("params_for_nonhierarchi_container")), 
             
             selectizeInput(ns("highlight_lst"),
                            label="highlighted by", 
                            choices = choices,
                            selected= "author", 
                            multiple = TRUE
             )
             
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
   
  
 
  
  output$params_for_nonhierarchi_container <- renderUI({ 
    
    PhysicLayout <- c('barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based') 
    
    selectizeInput(ns("PhysicLayoutId"),label="Physics Options", choices = PhysicLayout,
                 selected= 'forceAtlas2Based', multiple = FALSE)
  
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
        nodes, edges, width = "100%", height="100vh"
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
      
      # https://github.com/datastorm-open/visNetwork/issues/174
      # https://github.com/datastorm-open/visNetwork/issues/53
       visNetwork::visOptions(graph= graphe, manipulation = TRUE,
                             selectedBy = list(variable = "highlight", multiple = TRUE), # c("company", "program", "author"), # "highlight",
                             highlightNearest = TRUE
                             )
      
  })
  })
  
  
  output$shiny_return <- renderPrint({
    values$edges %>% filter(from %in% input$click | to  %in% input$click)
  })
  
  
  
  
  observeEvent({input$level_lst}, { 
    tt = create_nodes_edges(create_model_library(n_model=50), 
                            level_lst=c("company", input$level_lst, "model")  #c("company", "area", "program", "model")
    )  # "program", "author
    
    values$nodes = tt$nodes     
    values$edges = tt$edges  
  
  })
  
  observeEvent({input$highlight_lst}, { 
    validate(need(input$highlight_lst, message=FALSE))
    
    values$nodes = values$nodes %>% 
      unite_("highlight", input$highlight_lst, sep=",", remove=FALSE) 
    
  })
  
  
  # observeEvent({input$network_selectedBy3}, { 
  # validate(need(input$network_selectedBy, message=FALSE))
  #   
  #   print("click hightlight event:")
  #   
  #   graph = visNetworkProxy(ns("network")) 
  #   #selected_nodes_lst = graph %>% visGetSelectedNodes()
  #   selected_nodes_lst = values$nodes %>% 
  #     filter(highlight== input$network_selectedBy, mykey=="model") %>% pull(id)
  #   
  #   graph %>%
  #     visSelectNodes(id = selected_nodes_lst)   # %>% 
  # 
  # })
  
  
  observeEvent({input$double_click}, { 
      
    values$selected_nodes = unique(c(values$selected_nodes, input$click))
    

    visNetworkProxy(ns("network")) %>%
      visSelectNodes(id =  values$selected_nodes)   # %>% 
    #   visSelectEdges(id = edges_selection)
  })
  # 
  # observeEvent({input$double_click}, { 
  #   values$selected_nodes = setdiff(values$selected_nodes, input$double_click)
  #   
  #   visNetworkProxy(ns("network")) %>%
  #     visSelectNodes(id =  (values$selected_nodes))   # %>% 
  # })
  
  
   
  
  observe({ 
    graph <- visNetwork::visNetworkProxy(ns("network"))
    visNetwork::visPhysics(graph, solver = input$PhysicLayoutId )
    
  })
  
}



if (1==1) {

server <- shinyServer(function(input, output) {
  callModule(module_visnetwork_model_library, "call_visnetwork")
})

ui <- shinyUI(
  fluidRow(
    
    #level_lst=c("company", "area", "program", "model")
    column(12, 
           module_visnetwork_model_library_UI("call_visnetwork", label = NULL)#,
           #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
           )
)
)
  
shiny::shinyApp(ui = ui, server = server)  # , options = list(display.mode="showcase"))
    
}  




