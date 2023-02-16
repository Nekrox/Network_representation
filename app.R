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
    
    tags$head(
      tags$style(HTML('
      p {font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; font-size: 20px;}
    ')),
      # custom actionButton
      tags$style(HTML("
      .custom_button {
        width: 200px;
        background-color: #8675A9;
        color: #FFFFFF;
        border-color: #000000;
        border-radius: 5px;
        padding: 5px 10px;
        font-size: 16px;
        }
       ")),
      
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                      .js-irs-0 .irs-bar {background: #8675A9; border: #8675A9}")),
     
    ),
    
    tabItems(
      # First tab content
      tabItem(tabName = "general", style = "padding: 25px",
              fluidRow(
                
                titlePanel("How to use this application"),
                
                wellPanel(
                p(general_info_p)
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "net_viz", style = "padding: 25px",
              
              fluidRow(
                titlePanel("Nodal degree"),
                plotOutput("plot1"),
                titlePanel("Network visualization"),

                sliderInput("percent_changed", "Select percentage", value = 10, 
                            step = 5, min = 5, max = 25, width = 700),
                actionButton("choose_percentage", "Simulate", class = "custom_button"),
                uiOutput("network_ui_elements")
              ),
              
              # Optional fluid row which will render UI elements if [show network] button is pressed. 
              fluidRow(
                mainPanel(
                  conditionalPanel(
                    condition = "input.indv_net_pressed > 0",
                    
                    sidebarLayout(
                      sidebarPanel(
                       
                      ),
                      mainPanel(
                        div(style = "width: 600px; height: 600px; border: 2px solid purple;",
                            forceNetworkOutput("individual_network_viz", width = "600px", height = "600px")
                        )
                      )
                    )
                    
                    
                    
                    
                    
                    
                  )
                )
              )
              
              
      ) # End of second Tab item
      
    )
  )
  # ========================================================================
)

# ======== SERVER ======
server <- shinyServer(function(input, output, session) {
  
  output$plot1 <- renderPlot({
    histogram_plot
  })
  
  output$network_ui_elements <- renderUI({
    if (input$choose_percentage == 0)
      return(NULL)
    
    # now when the user selected the % of data, we need to load that particular data
    decomposed_g <- load_requested_data_based_on_percent(input$percent_changed)
    updateSelectInput(session, "selectInput_changed", choices = 1:length(decomposed_g))
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
      
          helpText(div(paste0(promt_sub_g, length(decomposed_g), " subgraphs."), 
                       style = "color: #888FF; font-size: 18px;")),
          
          selectInput("selectInput_changed", "Select an option:", choices = c(1,2,3)),
          
          h2(style = "color: #8675A9", "Node information:"),
          helpText("Note: to see the node information click on any node on the right."),
          
          htmlOutput("selected_node_info"),
          tags$head(tags$style("#selected_node_info{color: black;
                                 font-size: 24px;
                                 font-style: normal;
                                 }"
                              )
                    ),
          
          uiOutput("personal_link_button"),
          uiOutput("indiv_network_button")
          
        ), # End of the sidebarPanel 
        
        
        mainPanel(
          div(style = "width: 600px; height: 600px; border: 2px solid purple;",
            forceNetworkOutput("subgraph_network_viz", width = "600px", height = "600px")
          ),
        
        )
 
      )
    )
    
  })
  
  # Obserbe changes in user input and change the label accordingly
  
  observeEvent(input$percent_changed, {
    updateSelectInput(session, "selectInput_changed", choices = 1:length(decomposed_g))
    label <- paste0("Select ", input$percent_changed, "% of data")
    updateActionButton(inputId = "choose_percentage", label = label)
  })
  
  # Observe click on a node and present the full information on it if clicked
  observeEvent(input$node_clicked, {
    node_information = selected_nodes_df[selected_nodes_df$node.ID == input$node_clicked,]
    personal_url <- paste(link_str, node_information$node.ID[1], sep = "")
    selected_node <<- node_information$node.ID[1]
    
    output$selected_node_info <- renderText({ 
      str_id <- paste("Node ID: ", selected_node)
      str_name <- paste("Name: ", node_information$node.name[1])
      str_label <- paste("Visited: ", node_information$event.label[1])
      str_type <- paste("Participated in: ", node_information$event.Type[1])
      HTML(paste(str_id, str_name,str_label, str_type, sep = '<br/>'))
    })
    
    output$personal_link_button <- renderUI({
      shiny::a(
        h4(icon("link"),
           paste0("Personal web-page"),
           class = "custom_button",
           style = "fontweight:600"),
        target = "_blank",
        href = personal_url
      )
    })
    
    # show individual network
    output$indiv_network_button <- renderUI({
      actionButton("indv_net_pressed", "Show egocentric network", class = "custom_button")
      # config the button to appear and have some text
    })
    
  }) # end of observe event when the node is clicked
  
  # Subgraph Network plotting
  output$subgraph_network_viz <- renderForceNetwork({

    script <- 'Shiny.onInputChange("node_clicked", d.name)'
    sj_list <- create_sj_object(as.integer(input$selectInput_changed))
    
    forceNetwork(Links = sj_list$links, Nodes = sj_list$nodes, Source = "source",
                 Target = "target", NodeID = "name", Group = "group", # linkColour = "#4ECCA3"
                 opacity = 1, zoom = T, fontSize = 0, linkDistance = 350, clickAction = script
    )
    
  })
  
  # Individual node Network plotting
  output$individual_network_viz <- renderForceNetwork({

    sj_indiv <- create_sj_indiv(selected_node)

    forceNetwork(Links = sj_indiv$links, Nodes = sj_indiv$nodes, Source = "source",
                 Target = "target", NodeID = "name", Group = "group",
                 opacity = 1, zoom = T, fontSize = 0, linkDistance = 150
    )
  })
  
})

shinyApp(ui, server)