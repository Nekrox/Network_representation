library(shiny)
library(igraph)
library(networkD3)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
source("global.R")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Menu", titleWidth = 230),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("General information", tabName = "general", icon = icon("info-circle")),
      menuItem("Nodal Degree", tabName = "net_viz")
    )
  ),
  # ========================================================================
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "general", style = "padding: 25px",
              fluidRow(
                titlePanel("Network visualization"),
                tags$style(HTML("
                  .intro_text {
                    color: blue;
                    font-family: sans-serif;
                    font-size: 20px;
                  }
                ")),
                wellPanel(
                h4(class = ".intro_text", "place actual text here")
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "net_viz", style = "padding: 25px",
              
              fluidRow(
                
                titlePanel("Nodal degree"),
                plotOutput("plot1"),
                titlePanel("Network visualization"),
                
                sliderInput("percent_changed", "Select percentage", 15, 
                            step = 5, min = 5, max = 25, width = 500),
                actionButton("choose_percentage", "Simulate"),
                uiOutput("network_ui_elements")
              ),
              
      ) # End of second Tab item
      
    )
  )
  # ========================================================================
)

# ======== SERVER ======
server <- shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    histogram_plot
  })
  
  output$network_ui_elements <- renderUI({
    if (input$choose_percentage == 0)
      return(NULL)
    
    # now when the user selected the % of data, we need to load that particular data
    decomposed_g <<- load_requested_data_based_on_percent(input$percent_changed)
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          # helpText(1, " sub-graphs.")),
          help_str <- paste0(promt_sub_g, length(decomposed_g), "subgraphs"),
          helpText(HTML("<div style='font-size: 16px; 
                        font-family: Arial, sans-serif;
                        '>help_str</div>")),
          
          sliderInput("sub_number_changed", "Subgraph number", 1, 
                      step = 1, min = 1, max = length(decomposed_g), width = 500),
          actionButton("execute_net", "Run selected subgraph"),
          
          h2(style = "color: #8675A9", "Node information:"),
          
          htmlOutput("selected_node_info"),
          tags$head(tags$style("#selected_node_info{color: black;
                                 font-size: 24px;
                                 font-style: normal;
                                 }"
                              )
                    ),
          
          uiOutput("ui_open_tab_button")
          
        ), # End of the sidebarPanel 
        
        
        mainPanel(
          div(style = "width: 800px; height: 800px; border: 2px solid purple;",
            forceNetworkOutput("force", width = "800px", height = "800px")
          ),
        
        )
 
      )
    )
    
  })
  
  
  # Obserbe changes in user input and change the label accordingly
  observeEvent(input$sub_number_changed, {
    label <- paste0("Visualize sub-graph number ", input$sub_number_changed)
    updateActionButton(inputId = "execute_net", label = label)
  })
  
  observeEvent(input$percent_changed, {
    label <- paste0("Select ", input$percent_changed, "% of data")
    updateActionButton(inputId = "choose_percentage", label = label)
  })
  
  # Observe click on a node and present the full information on it if clicked
  observeEvent(input$node_clicked, {
    node_information = selected_nodes_df[selected_nodes_df$node.ID == input$node_clicked,]
    url_1 <- paste(link_str,node_information$node.ID[1], sep = "")
    
    output$selected_node_info <- renderText({ 
      str_id <- paste("Node ID: ", node_information$node.ID[1])
      str_name <- paste("Name: ", node_information$node.name[1])
      str_label <- paste("Visited: ", node_information$event.label[1])
      str_type <- paste("Participated in: ", node_information$event.Type[1])
      HTML(paste(str_id, str_name,str_label, str_type, sep = '<br/>'))
    })
    
    output$ui_open_tab_button <- renderUI({
      shiny::a(
        h4(icon("link"),
           paste0("Personal web-page"),
           class = "btn btn-default action-button",
           style = "fontweight:600"),
        target = "_blank",
        href = url_1
      )
    })
    
  })
  
  # Network prep and plotting
  output$force <- renderForceNetwork({

    script <- 'Shiny.onInputChange("node_clicked", d.name)'
    sj_list <- create_sj_object(input$sub_number_changed)
    
    forceNetwork(Links = sj_list$links, Nodes = sj_list$nodes, Source = "source",
                 Target = "target", NodeID = "name", Group = "group", # linkColour = "#4ECCA3"
                 opacity = 1, zoom = T, fontSize = 0, linkDistance = 350, clickAction = script
    )
    
  })
  
})

shinyApp(ui, server)