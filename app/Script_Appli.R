# --- 0. Chargement des BibliothÃ¨ques ---
library(shiny)
library(shinythemes)
library(shinymanager) 
library(ggplot2)
library(dplyr)
library(readr)
library(DT) 
library(viridis) 
library(plotly) 

# --- 1. Chargement des DonnÃ©es et Variables ---

# Credentials (Identifiant/Mot de passe PRÃDÃFINIS)
credentials <- data.frame(
  user = c("eleve"),
  password = c("projet"),
  stringsAsFactors = FALSE
)

# L'app lit le fichier qui est DANS LE MEME DOSIER
logements_data <- read_csv("dpe_clean.csv") %>%
  # S'assurer que dpe_classe est un facteur ordonnÃ© pour les graphiques
  mutate(dpe_classe = factor(dpe_classe, levels = c("A", "B", "C", "D", "E", "F", "G"))) %>%
  # Nettoyer les NA pour les variables clÃ©s
  filter(!is.na(dpe_classe), !is.na(surface_m2), !is.na(consommation_kwh_m2), !is.na(annee_construction))

# DÃ©finir les limites de l'annÃ©e de construction pour le slider
min_annee <- min(logements_data$annee_construction, na.rm = TRUE)
max_annee <- max(logements_data$annee_construction, na.rm = TRUE)


# Variables pour la rÃ©gression et les nouvelles visualisations
quantitative_vars <- c(
  "Surface (mÂ²)" = "surface_m2",
  "Consommation (kWh/mÂ²)" = "consommation_kwh_m2",
  "AnnÃ©e Construction" = "annee_construction"
)

# --- 2. Interface Utilisateur (UI) ---
app_ui <- fluidPage(
  
  # Charte CSS (Assurez-vous d'avoir le dossier www/custom_style.css si vous le gardez)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom_style.css")
  ),
  
  navbarPage(
    title = "GreenTech DPE Lyon - Analyse AvancÃ©e",
    theme = shinytheme("flatly"), 
    
    # --- Onglet 1: Tableau de bord & SynthÃ¨se AvancÃ©e (Remplacement de la Carte) ---
    tabPanel("Tableau de bord & SynthÃ¨se", icon = icon("chart-pie"), 
             sidebarLayout(
               sidebarPanel(
                 h3("Filtres d'Analyse"),
                 
                 # Filtre Code Postal
                 selectInput("code_postal", "Code Postal :",
                             choices = c("Tous", sort(unique(logements_data$code_postal))),
                             selected = "Tous", multiple = TRUE),
                 
                 radioButtons("source_type", "Type de logement :",
                              choices = c("Tous", "Ancien", "Neuf"), selected = "Tous"),
                 
                 sliderInput("surface_range", "Surface (mÂ²) :",
                             min = min(logements_data$surface_m2, na.rm = TRUE),
                             max = max(logements_data$surface_m2, na.rm = TRUE),
                             value = c(min(logements_data$surface_m2, na.rm = TRUE), 
                                       max(logements_data$surface_m2, na.rm = TRUE))),
                 
                 # Filtre par AnnÃ©e de Construction
                 sliderInput("annee_range", "AnnÃ©e de Construction :",
                             min = min_annee,
                             max = max_annee,
                             value = c(min_annee, max_annee),
                             step = 1,
                             sep = ""), # Supprime le sÃ©parateur de milliers
                 
                 selectInput("dpe_classe", "Classe DPE :",
                             choices = c("Toutes", levels(logements_data$dpe_classe)),
                             selected = "Toutes", multiple = TRUE),
                 
                 hr(),
                 downloadButton("download_data_csv", "Exporter DonnÃ©es FiltrÃ©es (.csv)")
               ),
               mainPanel(
                 h3("Indicateurs ClÃ©s (KPIs)"),
                 fluidRow(
                   column(4, div(class = "kpi-box",
                                 div(class = "kpi-value", textOutput("kpi_logements_count")),
                                 div(class = "kpi-label", "Logements trouvÃ©s"))),
                   column(4, div(class = "kpi-box",
                                 div(class = "kpi-value", textOutput("kpi_conso_m2_moy")),
                                 div(class = "kpi-label", "Consommation Moy. (kWh/mÂ²)"))),
                   column(4, div(class = "kpi-box",
                                 div(class = "kpi-value", textOutput("kpi_surface_moy")),
                                 div(class = "kpi-label", "Surface Moyenne (mÂ²)")))
                 ),
                 hr(),
                 
                 # --- NOUVEAUX ÃLÃMENTS ANALYTIQUES ---
                 h3("Relation entre Consommation, Surface et AnnÃ©e de Construction (Interactif)"),
                 p("Consommation et Surface sont reprÃ©sentÃ©es sur les axes. L'AnnÃ©e de Construction est codÃ©e par la couleur du point."),
                 plotlyOutput("plot_3d_scatter", height = 400),
                 
                 hr(),
                 h3("RÃ©partition de la Classe DPE par Type de Logement (Ancien vs Neuf)"),
                 # Interactive Plotly
                 plotlyOutput("plot_stacked_dpe_type", height = 300)
                 # --- FIN NOUVEAUX ÃLÃMENTS ANALYTIQUES ---
               )
             )
    ), # Fin tabPanel 1
    
    # --- Onglet 2: Visualisations Standard ---
    tabPanel("Graphiques", icon = icon("chart-bar"), 
             tabsetPanel(
               # Histogramme (Consommation) - Rendu interactif
               tabPanel("Histogramme (Consommation)", 
                        plotlyOutput("plot_histo_conso_m2_plotly", height = 400),
                        downloadButton("download_histo_png", "Exporter (.png)")), 
               # BoÃ®te Ã  Moustaches (Surface) - Rendu interactif
               tabPanel("BoÃ®te Ã  Moustaches (Surface)", 
                        plotlyOutput("plot_boxplot_surface_plotly", height = 400)), 
               # Diagramme (RÃ©partition DPE) - Rendu interactif
               tabPanel("Diagramme (RÃ©partition DPE)", 
                        plotlyOutput("plot_bar_dpe_plotly", height = 400)) 
             )
    ), # Fin tabPanel 2
    
    # --- Onglet 3: Tableau de DonnÃ©es ---
    tabPanel("DonnÃ©es FiltrÃ©es", icon = icon("table"), 
             h4("AperÃ§u des donnÃ©es filtrÃ©es"),
             DT::dataTableOutput("main_table") 
    ), # Fin tabPanel 3
    
    # --- Onglet 4: Analyse & RÃ©gression ---
    tabPanel("Analyse & RÃ©gression", icon = icon("calculator"), 
             sidebarLayout(
               sidebarPanel(
                 h4("Analyse de RÃ©gression LinÃ©aire"),
                 # Correction : Consommation sÃ©lectionnÃ©e par dÃ©faut pour Y, Surface pour X
                 selectInput("reg_x_var", "Variable X :", 
                             choices = quantitative_vars,
                             selected = quantitative_vars[1]),
                 selectInput("reg_y_var", "Variable Y :", 
                             choices = quantitative_vars, 
                             selected = quantitative_vars[2]),
                 hr(),
                 h5("CorrÃ©lation de Pearson :"),
                 verbatimTextOutput("correlation_text")
               ),
               mainPanel(
                 # Rendu en Plotly pour l'interactivitÃ©
                 plotlyOutput("plot_regression_plotly") 
               )
             )
    ), # Fin tabPanel 4
    
    # --- Onglet 5: Statistiques Locales ---
    tabPanel("Statistiques Locales", icon = icon("chart-pie"),
             fluidRow(
               column(12, 
                      h3("SynthÃ¨se par Code Postal"),
                      p("Analyse des indicateurs clÃ©s par Code Postal, basÃ©s sur les filtres appliquÃ©s."),
                      DT::dataTableOutput("summary_table_postal")
               )
             )
    ), # Fin tabPanel 5
    
    # --- Onglet 6: Aide & Explication DPE ---
    tabPanel("Aide & DPE", icon = icon("question-circle"),
             fluidRow(
               column(8, offset = 2,
                      h2("Comprendre le Diagnostic de Performance ÃnergÃ©tique (DPE)"),
                      p("Le DPE Ã©value la performance Ã©nergÃ©tique d'un logement sur une Ã©chelle de A Ã  G."),
                      
                      tags$ul(
                        tags$li(tags$b("A - C :"), " Bonne performance, faible consommation."),
                        tags$li(tags$b("D - E :"), " Performance moyenne Ã  Ã©levÃ©e."),
                        tags$li(tags$b("F - G :", style="color:red;"), " Logements trÃ¨s Ã©nergivores (Passoires thermiques), nÃ©cessitant des rÃ©novations.")
                      ),
                      
                      hr(),
                      h3("Comment utiliser l'outil"),
                      p("Utilisez les filtres Ã  gauche pour segmenter les donnÃ©es. Les diffÃ©rents onglets offrent des vues complÃ©mentaires : SynthÃ¨se (relations croisÃ©es), Graphiques (distributions), et RÃ©gression (corrÃ©lation linÃ©aire).")
               )
             )
    ) # Fin tabPanel 6
  ) # Fin navbarPage
) # Fin fluidPage (UI App)

# Ãcran de connexion sÃ©curisÃ©
ui <- secure_app(app_ui,
                 tags_top = tags$div(
                   style = "text-align: center;",
                   h2("Connexion GreenTech DPE Lyon"),
                   p("Identifiant: eleve / Mot de passe: projet")
                 )
)

# --- 3. Serveur ---
server <- function(input, output, session) {
  
  # Appel pour l'authentification
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # --- A. DonnÃ©es RÃ©actives (Filtrage) ---
  filtered_data <- reactive({
    data_to_filter <- logements_data
    
    # Filtre Code Postal
    if (!is.null(input$code_postal) && !"Tous" %in% input$code_postal) {
      data_to_filter <- data_to_filter %>% filter(code_postal %in% input$code_postal)
    }
    
    # Filtre Type de Logement
    if (input$source_type != "Tous") {
      data_to_filter <- data_to_filter %>% filter(source_type == input$source_type)
    }
    
    # Filtre Classe DPE
    if (!is.null(input$dpe_classe) && !"Toutes" %in% input$dpe_classe) {
      data_to_filter <- data_to_filter %>% filter(dpe_classe %in% input$dpe_classe)
    }
    
    # Filtre Surface
    if (!is.null(input$surface_range)) {
      data_to_filter <- data_to_filter %>%
        filter(surface_m2 >= input$surface_range[1] &
                 surface_m2 <= input$surface_range[2])
    }
    
    # Application du Filtre AnnÃ©e
    if (!is.null(input$annee_range)) {
      data_to_filter <- data_to_filter %>%
        filter(annee_construction >= input$annee_range[1] &
                 annee_construction <= input$annee_range[2])
    }
    
    # Nettoyer les lignes avec NA sur les variables clÃ©s pour les graphiques avancÃ©s
    data_to_filter <- data_to_filter %>% 
      filter(!is.na(surface_m2), !is.na(consommation_kwh_m2), !is.na(annee_construction))
    
    return(data_to_filter)
  })
  
  # --- B. Sorties (KPIs, Graphiques, Tables) ---
  
  # KPIs
  output$kpi_logements_count <- renderText({ 
    nrow(filtered_data()) 
  })
  output$kpi_conso_m2_moy <- renderText({
    conso <- mean(filtered_data()$consommation_kwh_m2, na.rm = TRUE)
    round(conso, 1)
  })
  output$kpi_surface_moy <- renderText({
    surf <- mean(filtered_data()$surface_m2, na.rm = TRUE)
    round(surf, 1)
  })
  
  # Table (Onglet 3: DonnÃ©es FiltrÃ©es)
  output$main_table <- DT::renderDataTable({
    DT::datatable(filtered_data() %>% 
                    select(code_postal, source_type, dpe_classe, consommation_kwh_m2, surface_m2, annee_construction),
                  options = list(pageLength = 10))
  })
  
  # --- VISUALISATIONS INTERACTIVES (Onglet 1: Tableau de bord) ---
  
  # Nuage de Points (Plotly)
  output$plot_3d_scatter <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # Correction : PrÃ©-calculer le texte du tooltip pour la robustesse avec ggplotly
    plot_data <- filtered_data() %>%
      mutate(tooltip_text = paste0("DPE: ", dpe_classe, 
                                   "<br>Surface: ", round(surface_m2, 0), " mÂ²", 
                                   "<br>Conso: ", round(consommation_kwh_m2, 0), " kWh/mÂ²",
                                   "<br>AnnÃ©e: ", annee_construction))
    
    p <- ggplot(plot_data, 
                aes(x = surface_m2, y = consommation_kwh_m2, color = annee_construction)) +
      aes(text = tooltip_text) + # Utiliser la colonne prÃ©-calculÃ©e
      geom_point(alpha = 0.6) +
      scale_color_viridis(option = "magma", name = "AnnÃ©e\nConstruction") +
      labs(title = "", x = "Surface (mÂ²)", y = "Consommation (kWh/mÂ²)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(title = "Surface (mÂ²)"),
        yaxis = list(title = "Consommation (kWh/mÂ²)")
      )
  })
  
  # Diagramme de Barres EmpilÃ©es (Plotly)
  output$plot_stacked_dpe_type <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # Calculer le nombre de logements par DPE et Type
    data_summary <- filtered_data() %>%
      group_by(source_type, dpe_classe) %>%
      summarise(count = n(), .groups = 'drop') %>%
      # Calculer le pourcentage dans chaque Type (Ancien/Neuf)
      group_by(source_type) %>%
      mutate(percentage = count / sum(count)) %>%
      ungroup() 
    
    p <- ggplot(data_summary, 
                aes(x = source_type, y = percentage, fill = dpe_classe,
                    # Info-bulle dÃ©taillÃ©e
                    text = paste0("Type: ", source_type,
                                  "<br>Classe DPE: ", dpe_classe,
                                  "<br>Proportion: ", round(percentage * 100, 1), "%",
                                  "<br>Nombre: ", count))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d(option = "turbo", name = "Classe DPE") +
      labs(
        title = "Distribution relative des Classes DPE par Type de Logement",
        x = "Type de Logement", 
        y = "Proportion (%)"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Graphiques Standards Rendu en Plotly pour l'interactivitÃ© ---
  
  # 1. Histogramme (Consommation) - Plotly (RESTRICTION D'AXE X Ã  0-600)
  output$plot_histo_conso_m2_plotly <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(filtered_data(), aes(x = consommation_kwh_m2)) +
      # Modification de coord_cartesian pour le zoom sur l'axe X (0-600)
      geom_histogram(fill = "#21908CFF", color = "white", bins = 50) + 
      coord_cartesian(xlim = c(0, 600)) + 
      labs(title = "Distribution de la Consommation (kWh/mÂ²) [Zoom 0-600]", x = "Consommation (kWh/mÂ²)", y = "Nb. Logements") +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  # 2. BoÃ®te Ã  Moustaches (Surface) - Plotly
  output$plot_boxplot_surface_plotly <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(filtered_data(), 
                # Ajout de 'text' pour l'info-bulle sur les points individuels (outliers)
                aes(x = dpe_classe, y = surface_m2, fill = dpe_classe, 
                    text = paste0("DPE: ", dpe_classe, "<br>Surface: ", round(surface_m2, 1), " mÂ²"))) +
      geom_boxplot(show.legend = FALSE) +
      scale_fill_viridis_d(option = "plasma") + 
      labs(title = "Distribution de la Surface (mÂ²) par Classe DPE", x = "Classe DPE", y = "Surface (mÂ²)") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  # 3. Diagramme (RÃ©partition DPE) - Plotly
  output$plot_bar_dpe_plotly <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # PrÃ©paration des donnÃ©es pour l'affichage prÃ©cis dans l'info-bulle
    data_count <- filtered_data() %>%
      group_by(dpe_classe) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(data_count, 
                aes(x = dpe_classe, y = count, fill = dpe_classe,
                    # Info-bulle dÃ©taillÃ©e
                    text = paste0("Classe DPE: ", dpe_classe, "<br>Nb. Logements: ", count))) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_viridis_d(option = "cividis") + 
      labs(title = "RÃ©partition des Logements par Classe DPE", x = "Classe DPE", y = "Nb. Logements") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  # RÃ©gression (Onglet 4) - Rendu en Plotly
  output$plot_regression_plotly <- renderPlotly({
    req(input$reg_x_var, input$reg_y_var, nrow(filtered_data()) > 0)
    
    # Correction : PrÃ©-calculer le texte du tooltip pour la robustesse avec ggplotly
    plot_data <- filtered_data() %>%
      mutate(tooltip_text = paste0("DPE: ", dpe_classe,
                                   "<br>Conso: ", round(consommation_kwh_m2, 0), " kWh/mÂ²", 
                                   "<br>Surface: ", round(surface_m2, 0), " mÂ²",
                                   "<br>AnnÃ©e: ", annee_construction))
    
    p <- ggplot(plot_data, 
                aes_string(x = input$reg_x_var, y = input$reg_y_var)) +
      aes(text = tooltip_text) + # Utiliser la colonne prÃ©-calculÃ©e
      geom_point(alpha = 0.3, color = "#21908CFF") +
      geom_smooth(method = "lm", color = "#FDE725FF") +
      labs(title = paste("RÃ©gression:", input$reg_x_var, "vs", input$reg_y_var),
           x = names(quantitative_vars)[quantitative_vars == input$reg_x_var],
           y = names(quantitative_vars)[quantitative_vars == input$reg_y_var]) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") 
  })
  
  # Le calcul de la corrÃ©lation reste inchangÃ©
  output$correlation_text <- renderPrint({
    # Assurer que les variables ne sont pas nulles et qu'il y a des donnÃ©es
    req(input$reg_x_var, input$reg_y_var, nrow(filtered_data()) > 0)
    
    cor_val <- cor(
      filtered_data()[[input$reg_x_var]], 
      filtered_data()[[input$reg_y_var]], 
      use = "complete.obs"
    )
    cat("Coefficient (r) :", round(cor_val, 3))
  })
  
  # --- Tableau de SynthÃ¨se par Code Postal (Onglet 5) ---
  output$summary_table_postal <- DT::renderDataTable({
    filtered_data() %>%
      group_by(code_postal) %>%
      summarise(
        Nb_Logements = n(),
        Conso_Moyenne_kwh_m2 = round(mean(consommation_kwh_m2, na.rm = TRUE), 1),
        Surface_Moyenne_m2 = round(mean(surface_m2, na.rm = TRUE), 1),
        Nb_A_B = sum(dpe_classe %in% c("A", "B"), na.rm = TRUE),
        Nb_F_G = sum(dpe_classe %in% c("F", "G"), na.rm = TRUE),
        .groups = 'drop' 
      ) %>%
      arrange(code_postal) %>%
      DT::datatable(
        options = list(pageLength = 10, dom = 'tip'),
        colnames = c('Code Postal', 'Logements (N)', 'Conso Moy. (kWh/mÂ²)', 'Surface Moy. (mÂ²)', 'Nb. A/B', 'Nb. F/G')
      )
  })
  
  # --- C. Exports ---
  
  # Export CSV
  output$download_data_csv <- downloadHandler(
    filename = function() { paste("dpe_lyon_filtre_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write_csv(filtered_data(), file) }
  )
  
  # Export PNG (Histogramme) - Reste basÃ© sur ggplot pour l'export statique
  output$download_histo_png <- downloadHandler(
    filename = "export_histogramme_zoom_0_600.png",
    content = function(file) {
      plot_to_save <- ggplot(filtered_data(), aes(x = consommation_kwh_m2)) +
        # Zoom 0-600
        geom_histogram(fill = "#21908CFF", color = "white", bins = 50) +
        coord_cartesian(xlim = c(0, 600)) + 
        labs(title = "Distribution de la Consommation (kWh/mÂ²) [Zoom 0-600]", x = "Consommation (kWh/mÂ²)", y = "Nb. Logements") +
        theme_minimal(base_size = 14)
      ggsave(file, plot = plot_to_save, device = "png", width = 8, height = 6)
    }
  )
  
} # Fin Serveur

# --- 4. Lancement de l'Application ---
shinyApp(ui = ui, server = server)
