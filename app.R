library(rsconnect)
library(shinyjs)
library(shiny)
library(igraph)
library(networkD3)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
source("global.R")

ui <- fluidPage(
  titlePanel("Clericus network representation"),
  # extend shiny function browseURL so it can work in shinyapp.io
  shinyjs::useShinyjs(),
  extendShinyjs(text = js_code, functions = 'browseURL'),
  
  tags$head(
    tags$style(HTML("
      .navbar .navbar-nav { 
                           color: #FFFFFF; 
                           font-size: 22px; 
                           background-color: #8675A9 ; } 
      .navbar.navbar-default.navbar-static-top{ color: #8675A9; 
                                      font-size: 38px; 
                                      background-color: #8675A9;}
      .navbar-default .navbar-brand { color: #000000; 
                                      font-size: 30px; 
                                      background-color: #8675A9 ;}
      
      /* Change text color of panels when hovered */
      .navbar .navbar-nav > li > a:hover {
        color: #FFFFFF; 
      }
        
      /* Change text color of panels when not hovered */
      .navbar-default .navbar-nav > li > a {
        color: black;
      }
      

    ")),
   
  ),
  navbarPage(
    
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
      
      tags$style(HTML(".small_margin_class {margin-bottom: 5px;}")),
      
      tags$style(
        HTML(".text_custom_class {
           color: black;
           font-size: 24px;
           font-style: normal;
         }")
      ),
      
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                      .js-irs-0 .irs-bar {background: #8675A9; border: #8675A9}")),
      
    ),
    
    tabPanel("General Info",
             
             fluidRow(
               titlePanel("How to use this application"),
               wellPanel(p(general_info_p))
             )
             
    ),
    # second panel
    tabPanel("Nodal Degree",
             
             fluidRow(
               
               titlePanel("Nodal degree"),
               # plotOutput("plot1"),
               img(src='nodal_histogram.png', height = "700px", width = "1000px"),
              
               titlePanel("Network visualization"),
               
               sliderInput("percent_changed", "Select percentage", value = 10, 
                           step = 5, min = 5, max = 25, width = 700),
               actionButton("choose_percentage", "Simulate", class = "custom_button"),
               uiOutput("network_ui_elements")
             ),
             
             fluidRow(
               uiOutput("ego_centric_ui_elements")
             ) # end of fluid row
       
    ), # end of second panel
    
    # THIRD PANEL
    tabPanel("Time frame",
             
             fluidRow(
               titlePanel("Network representation based on time frames"),
               wellPanel(p(time_info_p)),
               selectInput("start_y_input", "Please select a start year:", 
                           choices = 1593:1993),
               selectInput("end_y_input", "Please select an end year:", 
                           choices = NULL),
               actionButton("time_frame_pressed", "Show my time frame", class = "custom_button"),
               uiOutput("time_frame_elements")
               
             )
    ),
  )
)

server <- shinyServer(function(input, output, session) {
  
  # output$plot1 <- renderPlot({
  #   histogram_plot
  # })
  
  network_ui_elements <- eventReactive(input$choose_percentage, {
    # Create UI elements here
    decomposed_g <<- load_requested_data_based_on_percent(input$percent_changed)
    updateSelectInput(session, "selectInput_changed", choices = 1:length(decomposed_g))
    
    fluidRow(
      style = "margin: 20px; padding: 0px;",
      sidebarLayout(
        sidebarPanel(
          
          helpText(div(paste0(PROMT_SUB_G, length(decomposed_g), " subgraphs."), 
                       style = "color: #888FF; font-size: 18px;")),
          
          selectInput("selectInput_changed", "Select an option:", choices = NULL),

          h2(style = "color: #8675A9", "Node information:"),
          helpText(HELP_STR),
          htmlOutput("selected_node_info", class = "text_custom_class"),
          uiOutput("personal_link_button", class = "small_margin_class"),
          uiOutput("indv_net_pressed", class = "small_margin_class"),
          uiOutput("node_info_ui_elements")
        ), # End of the sidebarPanel 
        
        mainPanel(
          div(style = "width: 600px; height: 600px; border: 2px solid purple;",
              forceNetworkOutput("subgraph_network_viz", width = "600px", height = "600px")
          ),
          
        )
        
      )
    )
  })
  
  ego_centric_ui_elements <- eventReactive(input$indv_net_pressed, {
    # When the "show ego centric" button is pressed, it activates the following code:

    fluidRow(
      style = "margin: 0px; padding: 10px;",
      sidebarLayout(
        sidebarPanel(
          h2(style = "color: #8675A9", "Egocentric node information:"),
          htmlOutput("selected_ind_info", class = "text_custom_class"),
          uiOutput("ind_personal_link_button")
          
        ),
        mainPanel(
          div(style = "width: 600px; height: 600px; border: 2px solid purple;",
              forceNetworkOutput("individual_network_viz", width = "600px", height = "600px")
          )
        )
      )
      
    )
  })
  
  time_frame_elements <- eventReactive(input$time_frame_pressed, {
    # Create UI elements here
    time_frame_decomposed_g <<- time_frame_decomposed_creation(start_year, end_year)
    updateSelectInput(session, "tf_g_number_selectI_changed", choices = 1:length(time_frame_decomposed_g))
    
    fluidRow(
      # style = "margin: 20px; padding: 0px;",
      sidebarLayout(
        sidebarPanel(
          
          helpText(div(paste0(PROMT_SUB_G, length(time_frame_decomposed_g), " subgraphs."), 
                       style = "color: #888FF; font-size: 18px;")),
          
          selectInput("tf_g_number_selectI_changed", "Select an option:", choices = NULL),
          
          h2(style = "color: #8675A9", "Node information:"),
          helpText(HELP_STR),
          htmlOutput("selected_time_frame_info", class = "text_custom_class"),
          uiOutput("time_personal_link_button", class = "small_margin_class"),
          # uiOutput("indv_net_pressed", class = "small_margin_class"),
        ), # End of the sidebarPanel 
        
        mainPanel(
          div(style = "width: 600px; height: 600px; border: 2px solid purple;",
              forceNetworkOutput("time_frame_network_viz", width = "600px", height = "600px")
          ),
          
        )
        
      )
    )
  })
  
  output$network_ui_elements <- renderUI({
    network_ui_elements()
  })
  
  output$ego_centric_ui_elements <- renderUI({
    ego_centric_ui_elements()
  })
  
  output$time_frame_elements <- renderUI({
    time_frame_elements()
  })
  
  # Time frame input handling
  observeEvent(input$start_y_input, {
    start_year <<- as.integer(input$start_y_input)
    end_year <<- as.integer(input$start_y_input) + FIXED_INTERVAL
    
    updateSelectInput(session, "end_y_input", 
                      choices = start_year:end_year)
  })
  
  # Obserbe changes in user input and change the label accordingly
  observeEvent(input$percent_changed, {
    
    updateSelectInput(session, "selectInput_changed", choices = 1:length(decomposed_g))
    label <- paste0("Select ", input$percent_changed, "% of data")
    updateActionButton(inputId = "choose_percentage", label = label)
  })  
  
  
  # Observe click on a node and present the full information on it if clicked
  observeEvent(input$node_clicked, {
    node_information <- selected_nodes_df[selected_nodes_df$node.ID == input$node_clicked,]
    personal_url <<- paste(LINK_STR, node_information$node.ID[1], sep = "")

    
    output$selected_node_info <- renderText({ 
      str_id <- paste("Node ID: ", node_information$node.ID[1])
      str_name <- paste("Name: ", node_information$node.name[1])
      str_type <- paste("Participated in: ", nrow(node_information), " event(s).")
      str_message <- "Visit the link below for additional information."
      HTML(paste(str_id, str_name, str_type, str_message, sep = '<br/>'))
    })
    
    output$personal_link_button <- renderUI({
      actionButton(
        inputId = "personal_link_button",
        label = "Personal web-page",
        icon = icon("link"),
        class = "custom_button"
      )
    })
    
    output$indv_net_pressed <- renderUI({
      actionButton(
        inputId = "indv_net_pressed",
        label = "Show egocentric network",
        class = "custom_button"
      )
    })
    
    
  }) # end of observe event when the node is clicked
  
  observeEvent(input$personal_link_button, {
    js$browseURL(personal_url)
  })
  
  # Observe click on selected egocentric network and provide information on the node
  observeEvent(input$ind_node_clicked, {
    ind_n_info = selected_indv_n_df[selected_indv_n_df$node.ID == input$ind_node_clicked,]
    egoc_ind_url <- paste(LINK_STR, ind_n_info$node.ID[1], sep = "")
    ind_s_node <<- ind_n_info$node.ID[1]
    
    output$selected_ind_info <- renderText({ 
      str_id <- paste("Node ID: ", ind_s_node)
      str_name <- paste("Name: ", ind_n_info$node.name[1])
      str_type <- paste("Participated in: ", nrow(ind_n_info), " event(s).")
      HTML(paste(str_id, str_name, str_type, sep = '<br/>'))
    })
    
    output$ind_personal_link_button <- renderUI({
      actionButton(
        inputId = "ind_personal_link_button",
        label = "Personal web-page",
        icon = icon("link"),
        class = "custom_button"
      )
    })
    
    # open a link if it is pressed
    observeEvent(input$ind_personal_link_button, {
      js$browseURL(egoc_ind_url)
    })
    
  }) # end of Observe click
  
  
  # Observe click on selected TIMEFRAME network and provide information on the node
  observeEvent(input$tf_node_clicked, {
    time_n_info = selected_tf_df[selected_tf_df$node.ID == input$tf_node_clicked,]
    time_url <- paste(LINK_STR, time_n_info$node.ID[1], sep = "")
    time_s_node <<- time_n_info$node.ID[1]
    
    output$selected_time_frame_info <- renderText({ 
      str_id <- paste("Node ID: ", time_s_node)
      str_name <- paste("Name: ", time_n_info$node.name[1])
      str_type <- paste("Participated in: ", nrow(time_n_info), " event(s).")
      HTML(paste(str_id, str_name, str_type, sep = '<br/>'))
    })
    
    output$time_personal_link_button <- renderUI({
      actionButton(
        inputId = "time_personal_link_button",
        label = "Personal web-page",
        icon = icon("link"),
        class = "custom_button"
      )
    })
    
    # open a link if it is pressed
    observeEvent(input$time_personal_link_button, {
      js$browseURL(time_url)
    })
    
  }) # end of Observe click 
  
  
  
  
  
  
  
  

  
  
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
    
    # print(selected_node)
    sj_indiv <- create_sj_indiv(as.integer(input$node_clicked))
    ind_script <- 'Shiny.onInputChange("ind_node_clicked", d.name)'
    
    forceNetwork(Links = sj_indiv$links, Nodes = sj_indiv$nodes, Source = "source",
                 Target = "target", NodeID = "name", Group = "group",
                 opacity = 1, zoom = T, fontSize = 0, linkDistance = 150, clickAction = ind_script
    )
  })
  



  # TIMEFRAME node Network plotting
  output$time_frame_network_viz <- renderForceNetwork({
    
    sj_tf <- create_tf_sj_object(as.integer(input$tf_g_number_selectI_changed))
    
    
    tf_script <- 'Shiny.onInputChange("tf_node_clicked", d.name)'
    
    forceNetwork(Links = sj_tf$links, Nodes = sj_tf$nodes, Source = "source",
                 Target = "target", NodeID = "name", Group = "group",
                 opacity = 1, zoom = T, fontSize = 0, linkDistance = 150, clickAction = tf_script
    )
  })






})
shinyApp(ui, server)
