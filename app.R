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
  # Apply all tags from the tag_list in global
  tags_list,

  navbarPage(
    title = "",
    id = "navbar",
    tabPanel("General Info",
             
             fluidRow(
                 wellPanel(div(
                   HTML(instructions_html),
                   style = "width: 100%; height: 100%; overflow-y: auto;"
                 )),
             )
             
    ),
    # second panel
    tabPanel("Nodal Degree",
             
             fluidRow(
               style = "margin: 20px; padding: 0px;",
               titlePanel("Nodal degree"),
               p(HISTOGRAM_STR),
               
               img(src = "nodal_histogram.png", width = "1200px", height = "600px"),
               titlePanel("Network visualization"),
               
               sliderInput("percent_changed", "Select percentage", value = 10, 
                           step = 5, min = 5, max = 25, width = 700),
               actionButton("choose_percentage", "Simulate", class = "custom_button"),
               uiOutput("network_ui_elements", class = "big_margin_class"),
               uiOutput("ego_centric_ui_elements")
               
             ),
             
    ), # end of second panel
    
    # THIRD PANEL
    tabPanel("Time frame",
             # CLEAN THE ENV
             fluidRow(
               style = "margin: 20px; padding: 0px;",
               titlePanel("Network representation based on time frames"),
               wellPanel(p(time_info_p)),
             fluidRow(
                 column(width = 2,
                   selectInput("start_y_input", "Please select a start year:", 
                               choices = 1600:1983)
                 ),
                 column(width = 2,
                   selectInput("end_y_input", "Please select an end year:", 
                               choices = NULL)
                 ),
                 column(width = 2,
                    div(
                      actionButton("time_frame_pressed", "Show my time frame", class = "custom_button"),
                      style = "margin-top: 14px;"
                    )
                 ),
                 column(width = 6,
                        
                 )
              ),
               uiOutput("time_frame_elements", class = "med_margin_class")
               
             )
    ),
    
  )
)

server <- shinyServer(function(input, output, session) {
  
  ind_selected_node <- reactiveVal(NULL)
  show_nodal_network <- reactiveVal(TRUE)
  show_tf_network <- reactiveVal(TRUE)
  
  update_tf_network_trigger <- reactiveVal(FALSE)

  network_ui_elements <- eventReactive(input$choose_percentage, {
    # Create UI elements here
    show_nodal_network(TRUE)
    decomposed_g <<- load_requested_data_based_on_percent(input$percent_changed)
    updateSelectInput(session, "selectInput_changed", choices = 1:length(decomposed_g))

      fluidRow(
      style = "margin: 1px; padding: 0px;",
      sidebarLayout(
        sidebarPanel(
          
          p(div(paste0(PROMT_SUB_G_P1, length(decomposed_g), PROMT_SUB_G_P2), 
                       style = "color: #888FF; font-size: 18px;")),
          
          selectInput("selectInput_changed", "Select a subgraph:", selected = 2, choices = 1:length(decomposed_g)),
          
          # present statistics for the current sub graph
          h2(style = "color: #8675A9", "Network summary statistics:"),
          htmlOutput("current_sub_g_statistics", class = "text_custom_class"),
          p("Explanation of every statistics can be found in the general tab."),
          
          
          # present node information and two buttons: link and egocentric network
          h2(style = "color: #8675A9", "Node information:"),
          p(HELP_STR),
          htmlOutput("selected_node_info", class = "text_custom_class"),
          uiOutput("personal_link_button", class = "med_margin_class"),
          uiOutput("indv_net_pressed", class = "med_margin_class"),
          uiOutput("node_info_ui_elements"),
          
        ), # End of the sidebarPanel 
        
        mainPanel(
          div(style = "width: 100%; height: 100%; border: 2px solid purple;",
              forceNetworkOutput("subgraph_network_viz", width = "100%", height = "800px")
          ),
          
        )
        
      )
    )
    
  })
  
  ego_centric_ui_elements <- eventReactive(input$indv_net_pressed, {
    # When the "show ego centric" button is pressed, it activates the following code:

    fluidRow(
      style = "margin: 1px; padding: 10px;",
      sidebarLayout(
        sidebarPanel(
          h2(style = "color: #8675A9", "Egocentric network information:"),
          htmlOutput("selected_ind_info", class = "text_custom_class"),
          uiOutput("ind_personal_link_button"),
          
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
    show_tf_network(TRUE)
    # Create UI elements here
    time_frame_decomposed_g <<- time_frame_decomposed_creation(input$start_y_input, input$end_y_input)
    
    if (length(time_frame_decomposed_g) == 0) {
      fluidRow(p(NO_DATA_STR, style = "color: #F99417; font-size: 22px;"))
    } else {
      
      fluidRow(
        style = "margin: 1px; padding: 0px;",
        sidebarLayout(
          sidebarPanel(
            
            p(div(paste0(PROMT_SUB_G_P1, length(time_frame_decomposed_g), PROMT_SUB_G_P2), 
                         style = "color: #888FF; font-size: 18px;")),
        
            selectInput("tf_g_number_selectI_changed", "Select a subgraph:", choices = 1:length(time_frame_decomposed_g)),
            
            update_tf_network_trigger(TRUE),
            
            # present statistics for the current sub graph
            h2(style = "color: #8675A9", "Network summary statistics:"),
            htmlOutput("current_sub_g_tf_statistics", class = "text_custom_class"),
            p("Explanation of every statistics can be found in general tab"),
            # present Node information
            h2(style = "color: #8675A9", "Node information:"),
            p(HELP_STR),
            htmlOutput("selected_tf_node_info", class = "text_custom_class"),
            uiOutput("time_personal_link_button", class = "med_margin_class"),
            
            
          ), # End of the sidebarPanel 
          
          mainPanel(
            div(style = "width: 750px; height: 750px; border: 2px solid purple;",
                forceNetworkOutput("time_frame_network_viz", width = "750px", height = "750px")
            ),
            
          )
          
        )
      )
      
    }# end of else
    
  })# end of time frame event reactive
  
  
  # EGOCENTRIC TIMEFRAME
  
  
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
    updateSelectInput(session, "end_y_input", choices = start_year:end_year)
  })
  
  # Obserbe changes in user input and change the label accordingly
  observeEvent(input$percent_changed, {
    # updateSelectInput(session, "selectInput_changed", choices = 1:length(decomposed_g))
    label <- paste0("Select ", input$percent_changed, "% of data")
    updateActionButton(inputId = "choose_percentage", label = label)
  })  
  
  # Observe click on a node and present the full information on it if clicked
  observeEvent(input$node_clicked, {
    node_information <- selected_nodes_df[selected_nodes_df$node.ID == input$node_clicked,]
    personal_url <<- paste(LINK_STR, node_information$node.ID[1], sep = "")
    ind_selected_node(NULL) # Remove previously selected egocentric node
    
    output$selected_node_info <- renderText({ 
      nodeOnClickInfo_func(node_information)
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
    
    # replace old text from the egocentric node information column with a instruction. 
    output$selected_ind_info <- renderText({
      HTML("Please click on any node to see more information.")
    })
    
  }) # end of observe event when the node is clicked
  
  # Observe click on selected egocentric network and provide information on the node
  observeEvent(input$ind_node_clicked, {
    
    ind_n_info = selected_indv_n_df[selected_indv_n_df$node.ID == input$ind_node_clicked,]
    egoc_ind_url <<- paste(LINK_STR, ind_n_info$node.ID[1], sep = "")
    ind_s_node <<- ind_n_info$node.ID[1]
    ind_selected_node(ind_s_node)  # update my egocentric selected node.
    
    output$selected_ind_info <- renderText({
      nodeOnClickInfo_func(ind_n_info)
    })
    
    output$ind_personal_link_button <- renderUI({
  
      if (!is.null(ind_selected_node())) {
        fluidRow(
          style = "margin: 1px; padding: 0px;",
          actionButton(
            inputId = "ind_personal_link_button",
            label = "Personal web-page",
            icon = icon("link"),
            class = "custom_button"
          ),
          helpText("Please make sure your browser does not block pop-up windows.")
        )
      }
    })
    
  }) # end of Observe click
  
  
  # Observe click on selected TIMEFRAME network and provide information on the node
  observeEvent(input$tf_node_clicked, {
    time_n_info = selected_tf_df[selected_tf_df$node.ID == input$tf_node_clicked,]
    time_url <<- paste(LINK_STR, time_n_info$node.ID[1], sep = "")
    time_s_node <<- time_n_info$node.ID[1]
    
    output$selected_tf_node_info <- renderText({ 
      nodeOnClickInfo_func(time_n_info)
    })
    
    output$time_personal_link_button <- renderUI({
      actionButton(
        inputId = "time_personal_link_button",
        label = "Personal web-page",
        icon = icon("link"),
        class = "custom_button"
      )
    })
    
  }) # end of Observe click 
  
  
  # Subgraph Network plotting
  output$subgraph_network_viz <- renderForceNetwork({
    
    script <- 'Shiny.onInputChange("node_clicked", d.name)'
    sj_list <- create_sj_object(as.integer(input$selectInput_changed))
    
    
    if (show_nodal_network()) {
      forceNetwork(Links = sj_list$links, Nodes = sj_list$nodes, Source = "source",
                   Target = "target", NodeID = "name", Group = "group", # linkColour = "#4ECCA3"
                   opacity = 1, zoom = T, fontSize = 0, linkDistance = 350, clickAction = script
      )
    }
    
  })
  
  # Individual node Network plotting
  output$individual_network_viz <- renderForceNetwork({

    ind_script <- 'Shiny.onInputChange("ind_node_clicked", d.name)'
    if (show_nodal_network()) {
      egocentricNetwork_func(as.integer(input$node_clicked),ind_script)
    }
    
  })
  
  # TIMEFRAME node Network plotting
  output$time_frame_network_viz <- renderForceNetwork({
 
    sj_tf <- create_tf_sj_object(as.integer(input$tf_g_number_selectI_changed))
    tf_script <- 'Shiny.onInputChange("tf_node_clicked", d.name)'
    
    show_tf_network
    if (show_tf_network()) {
      forceNetwork(Links = sj_tf$links, Nodes = sj_tf$nodes, Source = "source",
                   Target = "target", NodeID = "name", Group = "group",
                   opacity = 1, zoom = T, fontSize = 0, linkDistance = 150, clickAction = tf_script
      )
    }
  })
  
  # Individual TIME FRAME Network plotting
  # output$individual_time_frame_viz <- renderForceNetwork({
  #   
  #   ind_tf_script <- 'Shiny.onInputChange("XXXXXXXX", d.name)'
  #   if (show_tf_network()) {
  #     egocentricNetwork_func(as.integer(input$tf_node_clicked),ind_tf_script)
  #   }
  #   
  # })
  
  # =======================OBSERVE ELEMNTS RESPONSIBLE FOR LINKS OPENNING=====================================   
  
  # open a link NODAL_DEGREE [MAIN]
  observeEvent(input$personal_link_button, {
    js$browseURL(personal_url)
  })
  
  # open a link NODAL_DEGREE [EGOCENTRIC]
  observeEvent(input$ind_personal_link_button, {
    js$browseURL(egoc_ind_url)

  })
  
  # open a link TIMEFRAME [MAIN]
  observeEvent(input$time_personal_link_button, {
    js$browseURL(time_url)
  })
  
  output$current_sub_g_statistics <- renderText({ 
    present_network_stat_func(as.integer(input$selectInput_changed), decomposed_g)
  })
  
  output$current_sub_g_tf_statistics <- renderText({ 
    present_network_stat_func(as.integer(input$tf_g_number_selectI_changed), time_frame_decomposed_g)
  })
  # =======================OTHER ELEMNTS=======================================
  output$my_text <- renderUI({
    display_text_func()
  })
  
  observe({
    if (update_tf_network_trigger()) {
      updateSelectInput(session, "tf_g_number_selectI_changed", selected = 2)
      update_tf_network_trigger(FALSE)
    }
  })
  
 
  # Track which tav is selected. 
  observe({
    if (req(input$navbar) == "General Info") {
      gc()
    }

    if (req(input$navbar) == "Nodal Degree") {
      gc()
      show_tf_network(FALSE)
    }

    if (req(input$navbar) == "Time frame") {
      gc()
      show_nodal_network(FALSE)
    }


  })
  
})
shinyApp(ui, server)



