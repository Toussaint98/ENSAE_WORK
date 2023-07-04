#Groupe7 : AMEWOUAME; SOSSOU; NGATCHA NOUTCHA; LOUGUE
library(shiny)
library(shinydashboard)
library(DT)
library(gt)
library(gtsummary)
library(tools)
library(readxl)
library(ggplot2)


ui <- dashboardPage(
  dashboardHeader(
    title = "Tableau de Bord"
  ),
  dashboardSidebar(
    
    # Déroulement du menu
    
    sidebarMenu(
      
      # menu accueil
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      
      # menu statistiques univariées
      menuItem("Statistiques univariées", tabName = "univar", icon = icon("chart-bar"),
               menuSubItem("Variables quantitatives", tabName = "quant"),
               menuSubItem("Variables qualitatives", tabName = "qual")),
      
      # menu statistiques bivariées
      menuItem("Statistiques bivariées", tabName = "bivar", icon = icon("chart-bar"))
      
    )
  ),
  
  # Présentation du body de l'application
  dashboardBody(
    tabItems(
      
      # Présentation de l'accueil
      tabItem(
        tabName = "accueil",
        h2("Page d'accueil"),
        p("tableau de bord !"),
        style = "overflow-x: scroll;",
        fileInput("database_file", "Sélectionnez votre base de données :"),
        actionButton("open_button", "Ouvrir"),
        dataTableOutput("table")
      ),
      
      tabItem(
        tabName = "quant",
        tabBox(width = 12, height = "500px",
               title = p("Statistiques Univariées",
                         style="font-size:20px;
                          font-family: Fira Sans, Arial, sans-serif;
                          monospace;
                          color:white; 
                          background-color:red;
                          padding:12px"),
               
               # Affichage du tableau
               tabPanel("Tableau", icon = icon("table"),
                        # Permettre à l'utilisateur de sélectionner une variable
                        fluidRow(
                          column(4, selectInput("vars_quant_tab", "Choisissez une variable: ", choices = NULL)),
                          # Afficher un tableau
                          column(8, dataTableOutput("tab_quant")))
               ), 
               
               # Affiches de quelques statistiques
               tabPanel("Summary", icon = icon("square-poll-horizontal"),
                        selectInput("vars_quant_sum", "Choisissez une variable: ", choices = NULL),
                        verbatimTextOutput("summary")
               ),
               
               # Afficihage d'un histogramme
               tabPanel("Histogramme", icon = icon("chart-column"),
                        fluidRow(
                          column(4, selectInput("vars_quant_hist", "Choisissez une variable: ", choices = NULL),
                                 br(),
                                 br(),
                                 sliderInput("bins", "Nombre de barres: ", min = 5, max = 50, value = 5, step = 5)),
                          column(8, plotOutput("hist")))
               ),
               
               #Affichage de a boite à moustaches
               tabPanel("Boites à moustaches", icon = icon("chart-column"),
                        fluidRow(
                          column(4, selectInput("vars_quant_box", "Choisissez une variable: ", choices = NULL)),
                          column(8, plotOutput("box")))
               )
        )
      ),
      
      # Statistiques des variables qualitatives
      tabItem(
        tabName = "qual",
        tabBox(width = 12, height = "500px",
               title = p("Statistiques Univariées",
                         style="font-size:20px;
                          font-family: Fira Sans, Arial, sans-serif;
                          monospace;
                          color:white; 
                          background-color:red;
                          padding:12px"),
               
               # Affichage du tableau
               tabPanel("Tableau", icon = icon("table"),
                        fluidRow(
                          column(4, selectInput("vars_qual_tab", "Choisissez une variable: ", choices = NULL)),
                          # Afficher un tableau
                          column(8, dataTableOutput("tab_qual")))
               ),
               
               # Affichage du diagramme des barres
               tabPanel("Graphique", icon = icon("chart-simple"),
                        fluidRow(
                          column(4, selectInput("vars_qual_bar", "Choisissez une variable: ", choices = NULL)),
                          # Afficher un diagramme des barres
                          column(8, plotOutput("bar")))
               )
        )
      ),
      
      tabItem(
        tabName = "bivar",
        tabBox(width = 12, height = "500px",
               title = p("Statistiques Bivariées",
                         style="font-size:20px;
                          font-family: Fira Sans, Arial, sans-serif;
                          monospace;
                          color:white; 
                          background-color:red;
                          padding:12px"),
               
               # Affichage du tableau
               tabPanel("Tableaux", icon = icon("table"),
                        fluidRow(
                          column(4, selectInput("vars_tab_1", "Choisissez la 1ère variable: ", choices = NULL),
                                 br(),
                                 br(),
                                 selectInput("vars_tab_2", "Choisissez la 2è variable: ", choices = NULL)),
                          # Afficher un tableau
                          column(8, gt_output("tab_croix")))
               ),
               
               # Affichage du nuage de points
               tabPanel("Nuage de points", icon = icon("chart-simple"),
                        fluidRow(
                          column(4, selectInput("vars_quant_point_1", "Choisissez la 1ère variable: ", choices = NULL),
                                 br(),
                                 br(),
                                 selectInput("vars_quant_point_2", "Choisissez la 2è variable: ", choices = NULL),
                                 br(),
                                 br(),
                                 selectInput("vars_color", "Choisissez la variable de croisement: ", choices = NULL)),
                          # Afficher un diagramme des barres
                          column(8, plotOutput("points")))
               )
        )
        
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  # Importer la base de données
  data <- reactive({
    if (!is.null(input$open_button) && input$open_button > 0 && tools::file_ext(input$database_file$datapath) == "csv") {
      
      read.csv(input$database_file$datapath)
      
    } else if (!is.null(input$open_button) && input$open_button > 0 && tools::file_ext(input$database_file$datapath) == "xlsx") {
      
      read_excel(input$database_file$datapath)
    } else{
      NULL
    }
  })
  
  # Afficher la base importée
  output$table <- renderDataTable({
    data()
  })
  
  
  # mettre à jour les UI selectInput
  observeEvent(data(), {
    updateSelectInput(session, "vars_quant_tab", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_quant_sum", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_quant_hist", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_quant_box", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_qual_tab", choices = names(data())[!sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_qual_bar", choices = names(data())[!sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_tab_1", choices = names(data())[!sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_tab_2", choices = names(data()))
    updateSelectInput(session, "vars_quant_point_1", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_quant_point_2", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "vars_color", choices = names(data())[!sapply(data(), is.numeric)])
  })
  
  # Afficher le tableau de la variable
  output$tab_quant<-renderDataTable({
    tableau<-table(data()[,input$vars_quant_tab])
    tableau<-as.data.frame(tableau)
    datatable(tableau)
  })
  
  output$tab_qual<-renderDataTable({
    tableau<-table(data()[,input$vars_qual_tab])
    tableau<-as.data.frame(tableau)
    datatable(tableau)
  })
  
  # Afficher quelques statistiques descriptives
  
  output$summary <- renderPrint({
    summary(data()[, input$vars_quant_sum])
  })
  
  # Tracer l'histogramme
  
  
  output$hist <- renderPlot({
    
    if (!is.null(data())) {
      ggplot(data(), aes(x = .data[[input$vars_quant_hist]])) + 
        geom_histogram(fill = "darkblue", color = "black", bins = input$bins)+
        labs(x = input$vars_quant_select, y = "Fréquence", title = paste0("Histogramme de la variable ", input$vars_quant_hist))
    } else {
      NULL
    }
  })
  
  # Tracer les boites à moustaches
  
  output$box <- renderPlot({
    
    if (!is.null(data())) {
      ggplot(data(), aes(x = .data[[input$vars_quant_box]])) + 
        geom_boxplot()+
        labs(x = input$vars_quant_box, y = "Fréquence", title = paste0("Boites à moustache de la variable ", input$vars_quant_box))
    } else {
      NULL
    }
  })
  
  # Tracer le diagramme des barres
  
  output$bar <- renderPlot({
    if (!is.null(data())) {
      ggplot(data(), aes(x = .data[[input$vars_qual_bar]])) + 
        geom_bar(fill = "darkblue", color = "black")+
        labs(x = input$vars_qual_bar, y = "Fréquence", title = paste0("Diagramme en bandes de la variable ", input$vars_qual_bar))
    } else {
      NULL
    }
  })
  
  # Tableau croisé
  
  output$tab_croix <- render_gt({
    
    if (!is.null(data())) {
      base <-data()[,c(input$vars_tab_1, input$vars_tab_2)]
      tbl_summary(base, by = all_of(input$vars_tab_1), missing = "no")%>%
        bold_labels() %>% italicize_levels() %>% as_gt()
    }
    
    # data() %>%
    #   select(input$vars_tab_1, input$vars_tab_2) %>%
    #   tbl_summary(by = all_of(input$vars_tab_1), missing = "no") %>%
    #   as_gt()
  })
  
  
  # Tracer le nuage de points
  
  output$points <- renderPlot({
    if (!is.null(data())) {
      ggplot(data(), aes(x = .data[[input$vars_quant_point_1]], y = .data[[input$vars_quant_point_2]], 
                         color = .data[[input$vars_color]])) +
        geom_point()
    } else {
      NULL
    }
  })
  
  
  
  output$text <- renderPrint({
    vars_quant()
  })
}
shinyApp(ui, server)