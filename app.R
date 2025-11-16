# --- 0. Chargement des Bibliothèques ---
library(shiny)
library(shinythemes) 
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)
library(shinymanager) # Pour le login (Expert)
library(DT)           # Pour les tableaux interactifs

# --- 1. Chargement des Données et Sécurité ---
# L'app lit le fichier qui est DANS LE MEME DOSSIER
logements_data <- read_csv("dpe_clean.csv") %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(dpe_classe))

# Variables pour la régression (Intermédiaire)
quantitative_vars <- c(
  "Surface (m²)" = "surface_m2",
  "Consommation (kWh/m²)" = "consommation_kwh_m2",
  "Année Construction" = "annee_construction"
)

# Sécurité (Expert)
credentials <- data.frame(
  user = c("admin", "enedis"), # Utilisateurs
  password = c("admin123", "enedis"), # Mots de passe
  stringsAsFactors = FALSE
)

# --- 2. Interface Utilisateur (UI) ---
ui <- fluidPage(
  # Charte CSS (Expert)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom_style.css") # <-- Il doit chercher dans 'www/'
  ),
  
  # UI de gestion du login
  uiOutput("auth_ui"),
  
  # UI principale (cachée jusqu'à la connexion)
  uiOutput("main_app_ui")
)


# --- 3. Serveur ---
server <- function(input, output, session) {
  
  # --- A. Authentification (Expert) ---
  auth_results <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_ui <- renderUI({
    auth_ui(
      id = "auth",
      tag_line = "Connexion au Dashboard GreenTech"
    )
  })
  
  # --- B. UI Principale (Post-Connexion) ---
  output$main_app_ui <- renderUI({
    req(auth_results$user_auth()) # Exige la connexion
    
    navbarPage(
      title = "GreenTech DPE Lyon",
      theme = shinytheme("flatly"), # Thème (Intermédiaire)
      
      # --- Onglet 1: Tableau de bord (Standard) ---
      tabPanel("Tableau de bord", icon = icon("dashboard"), # Onglet 1
               sidebarLayout(
                 sidebarPanel(
                   h3("Filtres"),
                   
                   # Widgets (Standard)
                   radioButtons("source_type", "Type de logement :",
                                choices = c("Tous", "Ancien", "Neuf"), selected = "Tous"),
                   
                   sliderInput("surface_range", "Surface (m²) :",
                               min = min(logements_data$surface_m2, na.rm = TRUE),
                               max = max(logements_data$surface_m2, na.rm = TRUE),
                               value = c(min(logements_data$surface_m2, na.rm = TRUE), 
                                         max(logements_data$surface_m2, na.rm = TRUE))),
                   
                   selectInput("dpe_classe", "Classe DPE :",
                               choices = c("Toutes", levels(logements_data$dpe_classe)),
                               selected = "Toutes", multiple = TRUE),
                   
                   hr(),
                   # Bouton Export CSV (Intermédiaire)
                   downloadButton("download_data_csv", "Exporter Données (.csv)")
                 ),
                 mainPanel(
                   # KPIs (Standard)
                   fluidRow(
                     column(4, div(class = "kpi-box",
                                   div(class = "kpi-value", textOutput("kpi_logements_count")),
                                   div(class = "kpi-label", "Logements trouvés"))),
                     column(4, div(class = "kpi-box",
                                   div(class = "kpi-value", textOutput("kpi_conso_m2_moy")),
                                   div(class = "kpi-label", "Consommation Moy. (kWh/m²)"))),
                     column(4, div(class = "kpi-box",
                                   div(class = "kpi-value", textOutput("kpi_surface_moy")),
                                   div(class = "kpi-label", "Surface Moyenne (m²)")))
                   ),
                   hr(),
                   h4("Aperçu des données filtrées"),
                   DT::dataTableOutput("main_table") # Tableau (Standard)
                 )
               )
      ), # Fin tabPanel 1
      
      # --- Onglet 2: Visualisations (Standard) ---
      tabPanel("Graphiques", icon = icon("chart-bar"), # Onglet 2
               tabsetPanel(
                 # 4 types de graphiques (Standard)
                 tabPanel("Histogramme (Consommation)", 
                          plotOutput("plot_histo_conso_m2"),
                          downloadButton("download_histo_png", "Exporter (.png)")), # Export PNG (Intermédiaire)
                 tabPanel("Boîte à Moustaches (Surface)", 
                          plotOutput("plot_boxplot_surface")),
                 tabPanel("Diagramme (Répartition DPE)", 
                          plotOutput("plot_bar_dpe"))
               )
      ), # Fin tabPanel 2
      
      # --- Onglet 3: Régression (Intermédiaire) ---
      tabPanel("Analyse & Régression", icon = icon("calculator"), # Onglet 3
               sidebarLayout(
                 sidebarPanel(
                   h4("Analyse de Régression Linéaire"), #
                   selectInput("reg_x_var", "Variable X :", choices = quantitative_vars),
                   selectInput("reg_y_var", "Variable Y :", 
                               choices = quantitative_vars, 
                               selected = quantitative_vars[2]),
                   hr(),
                   h5("Corrélation de Pearson :"), #
                   verbatimTextOutput("correlation_text")
                 ),
                 mainPanel(
                   # Nuage de points (4e type de graphique)
                   plotOutput("plot_regression")
                 )
               )
      ), # Fin tabPanel 3
      
      # --- Onglet 4: Cartographie (Standard) ---
      tabPanel("Cartographie", icon = icon("map-marked-alt"),
               h3("Carte interactive des logements (Lyon)"),
               p("Affiche un échantillon (max 1000) pour la performance."),
               leafletOutput("map_lyon", height = "600px") #
      ) # Fin tabPanel 4
      
    ) # Fin navbarPage
  }) # Fin renderUI main_app
  
  
  # --- C. Données Réactives (Filtrage) ---
  filtered_data <- reactive({
    # Filtres (Standard)
    data_to_filter <- logements_data
    
    if (input$source_type != "Tous") {
      data_to_filter <- data_to_filter %>% filter(source_type == input$source_type)
    }
    if (!is.null(input$dpe_classe) && !"Toutes" %in% input$dpe_classe) {
      data_to_filter <- data_to_filter %>% filter(dpe_classe %in% input$dpe_classe)
    }
    data_to_filter <- data_to_filter %>%
      filter(surface_m2 >= input$surface_range[1] &
               surface_m2 <= input$surface_range[2])
    
    return(data_to_filter)
  })
  
  # --- D. Sorties (KPIs, Graphiques, Tables) ---
  
  # KPIs
  output$kpi_logements_count <- renderText({ nrow(filtered_data()) })
  output$kpi_conso_m2_moy <- renderText({
    conso <- mean(filtered_data$consommation_kwh_m2, na.rm = TRUE)
    round(conso, 1)
  })
  output$kpi_surface_moy <- renderText({
    surf <- mean(filtered_data$surface_m2, na.rm = TRUE)
    round(surf, 1)
  })
  
  # Table
  output$main_table <- DT::renderDataTable({
    DT::datatable(filtered_data() %>% 
                    select(code_postal, source_type, dpe_classe, consommation_kwh_m2, surface_m2, annee_construction),
                  options = list(pageLength = 5))
  })
  
  # Graphiques
  output$plot_histo_conso_m2 <- renderPlot({
    ggplot(filtered_data(), aes(x = consommation_kwh_m2)) +
      geom_histogram(fill = "#21908CFF", color = "white", bins = 30) +
      labs(title = "Distribution de la Consommation (kWh/m²)", x = "Consommation (kWh/m²)", y = "Nb. Logements") +
      theme_minimal(base_size = 14)
  })
  
  output$plot_boxplot_surface <- renderPlot({
    ggplot(filtered_data(), aes(x = dpe_classe, y = surface_m2, fill = dpe_classe)) +
      geom_boxplot(show.legend = FALSE) +
      scale_fill_viridis_d(option = "viridis") +
      labs(title = "Distribution de la Surface (m²) par Classe DPE", x = "Classe DPE", y = "Surface (m²)") +
      theme_minimal(base_size = 14)
  })
  
  output$plot_bar_dpe <- renderPlot({
    ggplot(filtered_data(), aes(x = dpe_classe, fill = dpe_classe)) +
      geom_bar(show.legend = FALSE) +
      scale_fill_viridis_d(option = "viridis") +
      labs(title = "Répartition des Logements par Classe DPE", x = "Classe DPE", y = "Nb. Logements") +
      theme_minimal(base_size = 14)
  })
  
  # Régression
  output$plot_regression <- renderPlot({
    req(input$reg_x_var, input$reg_y_var)
    ggplot(filtered_data(), aes_string(x = input$reg_x_var, y = input$reg_y_var)) +
      geom_point(alpha = 0.3, color = "#21908CFF") +
      geom_smooth(method = "lm", color = "#FDE725FF") +
      labs(title = paste("Régression:", input$reg_x_var, "vs", input$reg_y_var),
           x = names(quantitative_vars)[quantitative_vars == input$reg_x_var],
           y = names(quantitative_vars)[quantitative_vars == input$reg_y_var]) +
      theme_minimal(base_size = 14)
  })
  
  output$correlation_text <- renderPrint({
    cor_val <- cor(
      filtered_data()[[input$reg_x_var]], 
      filtered_data()[[input$reg_y_var]], 
      use = "complete.obs"
    )
    cat("Coefficient (r) :", round(cor_val, 3))
  })
  
  # Carte
  output$map_lyon <- renderLeaflet({
    data_map <- filtered_data()
    
    if (nrow(data_map) > 1000) {
      data_map <- sample_n(data_map, 1000)
    }
    
    leaflet(data_map) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude, lat = ~latitude,
        popup = ~paste("DPE:", dpe_classe, "<br>", 
                       "Type:", source_type, "<br>",
                       "Consommation:", consommation_kwh_m2, "kWh/m²"),
        clusterOptions = markerClusterOptions() # Regroupement des points
      )
  })
  
  # --- E. Exports (Intermédiaire) ---
  
  # Export CSV
  output$download_data_csv <- downloadHandler(
    filename = function() { paste("dpe_lyon_filtre_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write_csv(filtered_data(), file) }
  )
  
  # Export PNG (Histogramme)
  output$download_histo_png <- downloadHandler(
    filename = "export_histogramme.png",
    content = function(file) {
      plot_to_save <- ggplot(filtered_data(), aes(x = consommation_kwh_m2)) +
        geom_histogram(fill = "#21908CFF", color = "white", bins = 30) +
        labs(title = "Distribution de la Consommation (kWh/m²)", x = "Consommation (kWh/m²)", y = "Nb. Logements") +
        theme_minimal(base_size = 14)
      ggsave(file, plot = plot_to_save, device = "png", width = 8, height = 6)
    }
  )
  
} # Fin Serveur

# --- 4. Lancement de l'Application ---
shinyApp(ui = ui, server = server)