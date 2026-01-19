# Application Shiny pour l'Analyse du Centre de Pression (CoP)
# Calcul de: vitesse, surface, ecart-type vitesse, entropie

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(signal)  # Pour le filtrage

# ====================
# FONCTIONS DE CALCUL
# ====================

# Fonction de filtrage Butterworth avec troncature des bords
apply_butterworth_filter <- function(x, fs, fc, order) {
  if (any(is.na(x))) return(x)
  
  tryCatch({
    mu <- mean(x)
    x_centered <- x - mu
    
    W <- fc / (fs / 2)
    if (W >= 1) W <- 0.99
    
    bf <- signal::butter(n = order, W = W, type = "low")
    y_centered <- signal::filtfilt(bf, x_centered)
    
    # Tronquer les 3*ordre premiers et derniers points
    n_remove <- 3 * order
    if (length(y_centered) > 2 * n_remove) {
      y_centered[1:n_remove] <- y_centered[n_remove + 1]
      y_centered[(length(y_centered) - n_remove + 1):length(y_centered)] <- 
        y_centered[length(y_centered) - n_remove]
    }
    
    return(y_centered + mu)
    
  }, error = function(e) {
    warning(paste("Erreur filtre:", e$message))
    return(x)
  })
}

# Calcul de la vitesse
calculate_velocity <- function(x, y, fs) {
  n <- length(x)
  dt <- 1 / fs
  duration <- (n - 1) * dt  # Duree totale en secondes
  
  dx <- diff(x)
  dy <- diff(y)
  dist_inst <- sqrt(dx^2 + dy^2) # Distance entre chaque point
  
  # Vitesses moyennes (Distance totale / Temps total)
  vX <- sum(abs(dx)) / duration
  vY <- sum(abs(dy)) / duration
  vmoy <- sum(dist_inst) / duration
  
  # Pour les ecart-types et variance, on travaille sur la vitesse instantanee (mm/s)
  v_instante <- dist_inst * fs
  
  sd_vX <- sd(abs(dx) * fs)
  sd_vY <- sd(abs(dy) * fs)
  var_vit <- var(v_instante)
  
  list(
    vX = vX,
    vY = vY,
    vmoy = vmoy,
    sd_vX = sd_vX,
    sd_vY = sd_vY,
    var_vit = var_vit
  )
}

# Calcul de la surface de l'ellipse de confiance a 90%
calculate_ellipse_surface <- function(x, y) {
  COP <- cbind(x, y)
  COP_centered <- scale(COP, center = TRUE, scale = FALSE)
  
  cov_mat <- cov(COP_centered)
  eigen_result <- eigen(cov_mat)
  
  # Longueur des axes de l'ellipse (90% de confiance)
  # 4.605 correspond au chi-carre a 90% pour 2 degres de liberte
  axes_length <- sqrt(4.605) * sqrt(eigen_result$values)
  
  # Surface de l'ellipse
  surface <- pi * prod(axes_length)
  
  # Generer les points de l'ellipse pour la visualisation
  theta <- seq(0, 2*pi, length.out = 100)
  ellipse_circle <- cbind(
    axes_length[1] * cos(theta),
    axes_length[2] * sin(theta)
  )
  
  # Rotation de l'ellipse selon les vecteurs propres
  ellipse_rotated <- ellipse_circle %*% t(eigen_result$vectors)
  ellipse_rotated[,1] <- ellipse_rotated[,1] + mean(x)
  ellipse_rotated[,2] <- ellipse_rotated[,2] + mean(y)
  
  list(
    surface = surface,
    axes = axes_length,
    eigenvectors = eigen_result$vectors,
    ellipse_points = ellipse_rotated
  )
}

# Calcul de la Sample Entropy
calculate_sample_entropy <- function(x, m = 2, r = 0.15) {
  # Normalisation
  x <- x - mean(x)
  s <- sqrt(mean(x^2))
  if (s > 0) x <- x / s
  
  n <- length(x)
  
  # Initialisation
  lastrun <- rep(0, n)
  run <- rep(0, n)
  A <- rep(0, m)
  B <- rep(0, m)
  
  for (i in 1:(n-1)) {
    nj <- n - i
    y1 <- x[i]
    
    for (jjj in 1:nj) {
      j <- jjj + i
      
      if (abs(x[j] - y1) < r) {
        run[jjj] <- lastrun[jjj] + 1
        M1 <- min(m, run[jjj])
        
        for (mm in 1:M1) {
          A[mm] <- A[mm] + 1
          if (j < n) {
            B[mm] <- B[mm] + 1
          }
        }
      } else {
        run[jjj] <- 0
      }
    }
    
    lastrun <- run
  }
  
  N <- n * (n - 1) / 2
  B <- c(N, B[1:(m-1)])
  p <- A / B
  
  # Eviter log(0)
  p[p == 0] <- 1e-10
  e <- -log(p)
  
  return(e[1])
}

# Calcul des amplitudes (range)
calculate_ranges <- function(x, y) {
  list(
    Xrange = max(x) - min(x),
    Yrange = max(y) - min(y)
  )
}

# ====================
# INTERFACE UTILISATEUR
# ====================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Analyse du CoP"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import & Parametres", tabName = "import", icon = icon("upload")),
      menuItem("Resultats", tabName = "results", icon = icon("chart-line")),
      menuItem("Visualisations", tabName = "viz", icon = icon("eye")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    ),
    
    hr(),
    
    h4("Parametres", style = "padding-left: 15px;"),
    
    numericInput("fs", "Frequence echantillonnage (Hz):", 
                 value = 40, min = 1, max = 1000),
    
    numericInput("fc", "Frequence de coupure (Hz):", 
                 value = 8, min = 0.1, max = 50, step = 0.1),
    
    numericInput("filter_order", "Ordre du filtre:", 
                 value = 2, min = 1, max = 8),
    
    checkboxInput("apply_filter", "Appliquer le filtre Butterworth", value = TRUE),
    
    hr(),
    
    h4("Sample Entropy", style = "padding-left: 15px;"),
    
    numericInput("sampen_m", "Parametre m:", 
                 value = 2, min = 1, max = 5),
    
    numericInput("sampen_r", "Parametre r:", 
                 value = 0.15, min = 0.05, max = 0.5, step = 0.05),
    
    checkboxInput("sampen_velocity", "Calculer sur la vitesse", value = FALSE)
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { margin: 10px; }
        .metric-box { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 10px;
          margin: 10px;
          text-align: center;
        }
        .metric-value { font-size: 32px; font-weight: bold; }
        .metric-label { font-size: 14px; margin-top: 5px; }
        .info-text { font-size: 12px; color: #eee; margin-top: 5px; }
      "))
    ),
    
    tabItems(
      # Onglet Import
      tabItem(
        tabName = "import",
        fluidRow(
          box(
            title = "Importer les donnees du CoP", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            fileInput("file", "Choisir un fichier TXT",
                      accept = c(".txt")),
            
            helpText("Format attendu: fichier TXT avec:"),
            helpText("- 2 colonnes (X, Y) representant les coordonnees du centre de pression"),
            helpText("- Separateur de colonnes: TABULATION"),
            helpText("- Separateur decimal: VIRGULE"),
            helpText("- Le fichier peut contenir une ligne d'en-tete"),
            
            hr(),
            
            actionButton("analyze", "ANALYSER LES DONNEES", 
                        icon = icon("calculator"),
                        class = "btn-primary btn-lg",
                        style = "width: 100%;")
          )
        ),
        
        fluidRow(
          box(
            title = "Apercu des donnees importees",
            status = "info",
            width = 12,
            DTOutput("data_preview")
          )
        ),
        
        fluidRow(
          valueBoxOutput("n_points", width = 4),
          valueBoxOutput("duration", width = 4),
          valueBoxOutput("file_status", width = 4)
        )
      ),
      
      # Onglet Resultats
      tabItem(
        tabName = "results",
        
        h3("Parametres Posturographiques", style = "margin: 20px;"),
        
        fluidRow(
          column(6,
                 div(class = "metric-box",
                     div(class = "metric-value", textOutput("vmoy_value")),
                     div(class = "metric-label", "Vitesse moyenne (mm/s)"),
                     div(class = "info-text", "Vitesse globale du deplacement du CoP")
                 )
          ),
          column(6,
                 div(class = "metric-box",
                     div(class = "metric-value", textOutput("surface_value")),
                     div(class = "metric-label", "Surface ellipse 90% (mm2)"),
                     div(class = "info-text", "Surface de l'ellipse de confiance a 90%")
                 )
          )
        ),
        
        fluidRow(
          column(4,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);",
                     div(class = "metric-value", textOutput("vx_value")),
                     div(class = "metric-label", "Vitesse X (mm/s)"),
                     div(class = "info-text", "Axe antero-posterieur")
                 )
          ),
          column(4,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);",
                     div(class = "metric-value", textOutput("vy_value")),
                     div(class = "metric-label", "Vitesse Y (mm/s)"),
                     div(class = "info-text", "Axe medio-lateral")
                 )
          ),
          column(4,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);",
                     div(class = "metric-value", textOutput("var_v_value")),
                     div(class = "metric-label", "Variance vitesse"),
                     div(class = "info-text", "Variabilite de la vitesse")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #fa709a 0%, #fee140 100%);",
                     div(class = "metric-value", textOutput("sampen_x_value")),
                     div(class = "metric-label", "Sample Entropy X"),
                     div(class = "info-text", "Complexite du signal en X")
                 )
          ),
          column(6,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #30cfd0 0%, #330867 100%);",
                     div(class = "metric-value", textOutput("sampen_y_value")),
                     div(class = "metric-label", "Sample Entropy Y"),
                     div(class = "info-text", "Complexite du signal en Y")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%); color: #333;",
                     div(class = "metric-value", textOutput("sdx_value")),
                     div(class = "metric-label", "Ecart-type X (mm)"),
                     div(class = "info-text", "Dispersion en X")
                 )
          ),
          column(6,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #a8edea 0%, #fed6e3 100%); color: #333;",
                     div(class = "metric-value", textOutput("sdy_value")),
                     div(class = "metric-label", "Ecart-type Y (mm)"),
                     div(class = "info-text", "Dispersion en Y")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #ff9a9e 0%, #fecfef 100%); color: #333;",
                     div(class = "metric-value", textOutput("xrange_value")),
                     div(class = "metric-label", "Amplitude X (mm)"),
                     div(class = "info-text", "Etendue des oscillations en X")
                 )
          ),
          column(6,
                 div(class = "metric-box", style = "background: linear-gradient(135deg, #fbc2eb 0%, #a6c1ee 100%); color: #333;",
                     div(class = "metric-value", textOutput("yrange_value")),
                     div(class = "metric-label", "Amplitude Y (mm)"),
                     div(class = "info-text", "Etendue des oscillations en Y")
                 )
          )
        ),
        
        fluidRow(
          box(
            title = "Tableau recapitulatif des resultats",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("results_table")
          )
        )
      ),
      
      # Onglet Visualisations
      tabItem(
        tabName = "viz",
        
        fluidRow(
          box(
            title = "Stabilogramme - Trajectoire du CoP",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("stabilogram", height = "500px")
          ),
          box(
            title = "Ellipse de confiance a 90%",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("ellipse_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Deplacement en X au cours du temps",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("x_time_series", height = "400px")
          ),
          box(
            title = "Deplacement en Y au cours du temps",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("y_time_series", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Vitesse instantanee du CoP",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("velocity_plot", height = "400px")
          )
        )
      ),
      
      # Onglet Export
      tabItem(
        tabName = "export",
        
        fluidRow(
          box(
            title = "Exporter les resultats",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            h4("Telecharger les resultats"),
            
            downloadButton("download_csv", "Telecharger CSV", class = "btn-success"),
            downloadButton("download_xlsx", "Telecharger Excel", class = "btn-success"),
            
            hr(),
            
            h4("Apercu des donnees exportees"),
            verbatimTextOutput("export_preview")
          )
        ),
        
        fluidRow(
          box(
            title = "Informations sur l'analyse",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            verbatimTextOutput("analysis_info")
          )
        )
      )
    )
  )
)

# ====================
# SERVEUR
# ====================

server <- function(input, output, session) {
  
  # Stockage reactif des donnees
  raw_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  # Lecture du fichier
  observeEvent(input$file, {
    req(input$file)
    
    file_ext <- tools::file_ext(input$file$name)
    
    # Verifier que c'est bien un fichier TXT
    if (file_ext != "txt") {
      showNotification("Veuillez selectionner un fichier TXT", type = "error")
      return(NULL)
    }
    
    tryCatch({
      # Lecture avec format europeen: virgule decimale, tabulation separateur
      data <- read.csv(input$file$datapath, header = TRUE, sep = "\t", dec = ",",
                       stringsAsFactors = FALSE, strip.white = TRUE)
      
      # Verifier que nous avons au moins 2 colonnes
      if (ncol(data) < 2) {
        showNotification("Le fichier doit contenir au moins 2 colonnes (X et Y)", type = "error")
        return(NULL)
      }
      
      # Prendre les 2 premieres colonnes comme X et Y
      data <- data[, 1:2]
      colnames(data) <- c("X", "Y")
      
      # Fonction pour convertir en numerique (gere virgule et point)
      to_num <- function(z) {
        z <- as.character(z)
        z <- trimws(z)
        z <- gsub(",", ".", z, fixed = TRUE)
        suppressWarnings(as.numeric(z))
      }
      
      data$X <- to_num(data$X)
      data$Y <- to_num(data$Y)
      data <- data[complete.cases(data$X, data$Y), ]
      
      raw_data(data)
      showNotification("Fichier charge avec succes!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de la lecture du fichier:", e$message), type = "error")
    })
  })
  
  # Apercu des donnees
  output$data_preview <- renderDT({
    req(raw_data())
    datatable(
      head(raw_data(), 100),
      options = list(pageLength = 10, scrollX = TRUE),
      caption = paste("Apercu des", nrow(raw_data()), "points de mesure")
    )
  })
  
  # Value boxes
  output$n_points <- renderValueBox({
    req(raw_data())
    valueBox(
      nrow(raw_data()),
      "Points de mesure",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$duration <- renderValueBox({
    req(raw_data())
    duration <- nrow(raw_data()) / input$fs
    valueBox(
      paste(round(duration, 1), "s"),
      "Duree d'enregistrement",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  output$file_status <- renderValueBox({
    req(raw_data())
    valueBox(
      "Pret",
      "Statut",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  # Analyse des donnees
  observeEvent(input$analyze, {
    req(raw_data())
    
    withProgress(message = 'Analyse en cours...', value = 0, {
      
      data <- raw_data()
      X <- data$X
      Y <- data$Y
      
      incProgress(0.1, detail = "Filtrage des donnees")
      
      # Appliquer le filtre si demande
      if (input$apply_filter) {
        X_filt <- apply_butterworth_filter(X, input$fs, input$fc, input$filter_order)
        Y_filt <- apply_butterworth_filter(Y, input$fs, input$fc, input$filter_order)
        
        # Diagnostic du filtrage
        cat("\n=== DIAGNOSTIC FILTRAGE BUTTERWORTH ===\n")
        cat("Signal X - Avant filtre: Range =", range(X)[2] - range(X)[1], ", SD =", sd(X), "\n")
        cat("Signal X - Apres filtre: Range =", range(X_filt)[2] - range(X_filt)[1], ", SD =", sd(X_filt), "\n")
        cat("Signal X - Ratio SD (filtre/brut) =", sd(X_filt) / sd(X), "(devrait etre < 1)\n")
        cat("Signal Y - Avant filtre: Range =", range(Y)[2] - range(Y)[1], ", SD =", sd(Y), "\n")
        cat("Signal Y - Apres filtre: Range =", range(Y_filt)[2] - range(Y_filt)[1], ", SD =", sd(Y_filt), "\n")
        cat("Signal Y - Ratio SD (filtre/brut) =", sd(Y_filt) / sd(Y), "(devrait etre < 1)\n")
      } else {
        X_filt <- X
        Y_filt <- Y
      }
      
      incProgress(0.2, detail = "Calcul des vitesses")
      
      # Calcul des vitesses
      vel_results <- calculate_velocity(X_filt, Y_filt, input$fs)
      
      incProgress(0.3, detail = "Calcul de la surface")
      
      # Calcul de la surface de l'ellipse
      ellipse_results <- calculate_ellipse_surface(X_filt, Y_filt)
      
      incProgress(0.4, detail = "Calcul des ecarts-types")
      
      # Ecarts-types
      sdX <- sd(X_filt)
      sdY <- sd(Y_filt)
      
      incProgress(0.5, detail = "Calcul des amplitudes")
      
      # Amplitudes
      range_results <- calculate_ranges(X_filt, Y_filt)
      
      incProgress(0.6, detail = "Calcul de la Sample Entropy")
      
      # Sample Entropy
      if (input$sampen_velocity) {
        # Calculer sur la vitesse (diff)
        sampen_X <- calculate_sample_entropy(diff(X_filt), input$sampen_m, input$sampen_r)
        sampen_Y <- calculate_sample_entropy(diff(Y_filt), input$sampen_m, input$sampen_r)
      } else {
        # Calculer directement sur les signaux (par defaut)
        sampen_X <- calculate_sample_entropy(X_filt, input$sampen_m, input$sampen_r)
        sampen_Y <- calculate_sample_entropy(Y_filt, input$sampen_m, input$sampen_r)
      }
      
      incProgress(0.9, detail = "Finalisation")
      
      # Stocker tous les resultats
      all_results <- list(
        velocity = vel_results,
        ellipse = ellipse_results,
        sdX = sdX,
        sdY = sdY,
        ranges = range_results,
        sampen_X = sampen_X,
        sampen_Y = sampen_Y,
        X_filt = X_filt,
        Y_filt = Y_filt
      )
      
      processed_data(data.frame(X = X_filt, Y = Y_filt))
      results(all_results)
      
      incProgress(1, detail = "Termine!")
      
      showNotification("Analyse terminee avec succes!", type = "message", duration = 3)
    })
  })
  
  # Affichage des resultats
  output$vmoy_value <- renderText({
    req(results())
    paste0(round(results()$velocity$vmoy, 2))
  })
  
  output$surface_value <- renderText({
    req(results())
    paste0(round(results()$ellipse$surface, 2))
  })
  
  output$vx_value <- renderText({
    req(results())
    paste0(round(results()$velocity$vX, 2))
  })
  
  output$vy_value <- renderText({
    req(results())
    paste0(round(results()$velocity$vY, 2))
  })
  
  output$var_v_value <- renderText({
    req(results())
    paste0(round(results()$velocity$var_vit, 2))
  })
  
  output$sampen_x_value <- renderText({
    req(results())
    paste0(round(results()$sampen_X, 3))
  })
  
  output$sampen_y_value <- renderText({
    req(results())
    paste0(round(results()$sampen_Y, 3))
  })
  
  output$sdx_value <- renderText({
    req(results())
    paste0(round(results()$sdX, 2))
  })
  
  output$sdy_value <- renderText({
    req(results())
    paste0(round(results()$sdY, 2))
  })
  
  output$xrange_value <- renderText({
    req(results())
    paste0(round(results()$ranges$Xrange, 2))
  })
  
  output$yrange_value <- renderText({
    req(results())
    paste0(round(results()$ranges$Yrange, 2))
  })
  
  # Tableau des resultats
  output$results_table <- renderDT({
    req(results())
    res <- results()
    
    df <- data.frame(
      Parametre = c(
        "Vitesse moyenne (mm/s)",
        "Vitesse X (mm/s)",
        "Vitesse Y (mm/s)",
        "Surface ellipse 90% (mm2)",
        "Ecart-type X (mm)",
        "Ecart-type Y (mm)",
        "Variance vitesse",
        "Amplitude X (mm)",
        "Amplitude Y (mm)",
        "Sample Entropy X",
        "Sample Entropy Y"
      ),
      Valeur = c(
        round(res$velocity$vmoy, 3),
        round(res$velocity$vX, 3),
        round(res$velocity$vY, 3),
        round(res$ellipse$surface, 3),
        round(res$sdX, 3),
        round(res$sdY, 3),
        round(res$velocity$var_vit, 3),
        round(res$ranges$Xrange, 3),
        round(res$ranges$Yrange, 3),
        round(res$sampen_X, 4),
        round(res$sampen_Y, 4)
      )
    )
    
    datatable(df, options = list(pageLength = 15, dom = 't'), rownames = FALSE)
  })
  
  # Visualisation: Stabilogramme
  output$stabilogram <- renderPlotly({
    req(processed_data())
    
    data <- processed_data()
    
    p <- plot_ly(data, x = ~X, y = ~Y, type = 'scatter', mode = 'lines',
                line = list(color = '#667eea', width = 1),
                name = 'Trajectoire') %>%
      add_markers(x = data$X[1], y = data$Y[1], 
                 marker = list(size = 10, color = 'green'),
                 name = 'Debut') %>%
      add_markers(x = tail(data$X, 1), y = tail(data$Y, 1),
                 marker = list(size = 10, color = 'red'),
                 name = 'Fin') %>%
      layout(
        title = "Trajectoire du CoP",
        xaxis = list(title = "X (mm)", scaleanchor = "y", scaleratio = 1),
        yaxis = list(title = "Y (mm)"),
        hovermode = 'closest'
      )
    
    p
  })
  
  # Visualisation: Ellipse
  output$ellipse_plot <- renderPlotly({
    req(processed_data(), results())
    
    data <- processed_data()
    ellipse_points <- results()$ellipse$ellipse_points
    
    p <- plot_ly() %>%
      add_trace(x = data$X, y = data$Y, type = 'scatter', mode = 'markers',
               marker = list(size = 3, color = '#667eea', opacity = 0.3),
               name = 'Points CoP') %>%
      add_trace(x = ellipse_points[,1], y = ellipse_points[,2],
               type = 'scatter', mode = 'lines',
               line = list(color = '#f5576c', width = 3),
               name = 'Ellipse 90%', fill = 'toself', fillcolor = 'rgba(245, 87, 108, 0.2)') %>%
      add_markers(x = mean(data$X), y = mean(data$Y),
                 marker = list(size = 10, color = 'black', symbol = 'x'),
                 name = 'Centre') %>%
      layout(
        title = "Ellipse de confiance a 90%",
        xaxis = list(title = "X (mm)", scaleanchor = "y", scaleratio = 1),
        yaxis = list(title = "Y (mm)"),
        hovermode = 'closest'
      )
    
    p
  })
  
  # Visualisation: Serie temporelle X
  output$x_time_series <- renderPlotly({
    req(processed_data())
    
    data <- processed_data()
    time <- (1:nrow(data)) / input$fs
    
    plot_ly(x = time, y = data$X, type = 'scatter', mode = 'lines',
           line = list(color = '#f093fb', width = 1.5)) %>%
      layout(
        title = "Deplacement en X",
        xaxis = list(title = "Temps (s)"),
        yaxis = list(title = "X (mm)"),
        hovermode = 'x'
      )
  })
  
  # Visualisation: Serie temporelle Y
  output$y_time_series <- renderPlotly({
    req(processed_data())
    
    data <- processed_data()
    time <- (1:nrow(data)) / input$fs
    
    plot_ly(x = time, y = data$Y, type = 'scatter', mode = 'lines',
           line = list(color = '#4facfe', width = 1.5)) %>%
      layout(
        title = "Deplacement en Y",
        xaxis = list(title = "Temps (s)"),
        yaxis = list(title = "Y (mm)"),
        hovermode = 'x'
      )
  })
  
  # Visualisation: Vitesse instantanee
  output$velocity_plot <- renderPlotly({
    req(processed_data())
    
    data <- processed_data()
    dx <- diff(data$X)
    dy <- diff(data$Y)
    vel <- sqrt(dx^2 + dy^2) * input$fs
    time <- (1:length(vel)) / input$fs
    
    plot_ly(x = time, y = vel, type = 'scatter', mode = 'lines',
           line = list(color = '#43e97b', width = 1.5)) %>%
      layout(
        title = "Vitesse instantanee du CoP",
        xaxis = list(title = "Temps (s)"),
        yaxis = list(title = "Vitesse (mm/s)"),
        hovermode = 'x'
      )
  })
  
  # Export CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("resultats_CoP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(results())
      res <- results()
      
      df <- data.frame(
        Parametre = c(
          "Vitesse_moyenne_mm_s",
          "Vitesse_X_mm_s",
          "Vitesse_Y_mm_s",
          "Surface_ellipse_90_mm2",
          "Ecart_type_X_mm",
          "Ecart_type_Y_mm",
          "Variance_vitesse",
          "Amplitude_X_mm",
          "Amplitude_Y_mm",
          "Sample_Entropy_X",
          "Sample_Entropy_Y",
          "Frequence_echantillonnage_Hz",
          "Frequence_coupure_Hz",
          "Ordre_filtre",
          "Filtre_applique",
          "SampEn_m",
          "SampEn_r",
          "SampEn_sur_vitesse"
        ),
        Valeur = c(
          res$velocity$vmoy,
          res$velocity$vX,
          res$velocity$vY,
          res$ellipse$surface,
          res$sdX,
          res$sdY,
          res$velocity$var_vit,
          res$ranges$Xrange,
          res$ranges$Yrange,
          res$sampen_X,
          res$sampen_Y,
          input$fs,
          input$fc,
          input$filter_order,
          input$apply_filter,
          input$sampen_m,
          input$sampen_r,
          input$sampen_velocity
        )
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Export Excel
  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("resultats_CoP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(results())
      res <- results()
      
      df <- data.frame(
        Parametre = c(
          "Vitesse_moyenne_mm_s",
          "Vitesse_X_mm_s",
          "Vitesse_Y_mm_s",
          "Surface_ellipse_90_mm2",
          "Ecart_type_X_mm",
          "Ecart_type_Y_mm",
          "Variance_vitesse",
          "Amplitude_X_mm",
          "Amplitude_Y_mm",
          "Sample_Entropy_X",
          "Sample_Entropy_Y"
        ),
        Valeur = c(
          res$velocity$vmoy,
          res$velocity$vX,
          res$velocity$vY,
          res$ellipse$surface,
          res$sdX,
          res$sdY,
          res$velocity$var_vit,
          res$ranges$Xrange,
          res$ranges$Yrange,
          res$sampen_X,
          res$sampen_Y
        )
      )
      
      writexl::write_xlsx(df, file)
    }
  )
  
  # Apercu export
  output$export_preview <- renderPrint({
    req(results())
    res <- results()
    
    cat("=== RESULTATS DE L'ANALYSE ===\n\n")
    cat(sprintf("Vitesse moyenne: %.3f mm/s\n", res$velocity$vmoy))
    cat(sprintf("Vitesse X: %.3f mm/s\n", res$velocity$vX))
    cat(sprintf("Vitesse Y: %.3f mm/s\n", res$velocity$vY))
    cat(sprintf("Surface ellipse 90%%: %.3f mm2\n", res$ellipse$surface))
    cat(sprintf("Ecart-type X: %.3f mm\n", res$sdX))
    cat(sprintf("Ecart-type Y: %.3f mm\n", res$sdY))
    cat(sprintf("Variance vitesse: %.3f\n", res$velocity$var_vit))
    cat(sprintf("Amplitude X: %.3f mm\n", res$ranges$Xrange))
    cat(sprintf("Amplitude Y: %.3f mm\n", res$ranges$Yrange))
    cat(sprintf("Sample Entropy X: %.4f\n", res$sampen_X))
    cat(sprintf("Sample Entropy Y: %.4f\n", res$sampen_Y))
  })
  
  # Informations sur l'analyse
  output$analysis_info <- renderPrint({
    req(raw_data(), results())
    
    cat("=== INFORMATIONS SUR L'ANALYSE ===\n\n")
    cat(sprintf("Fichier: %s\n", input$file$name))
    cat(sprintf("Nombre de points: %d\n", nrow(raw_data())))
    cat(sprintf("Duree: %.2f secondes\n", nrow(raw_data()) / input$fs))
    cat(sprintf("Frequence d'echantillonnage: %d Hz\n", input$fs))
    cat(sprintf("Filtre applique: %s\n", ifelse(input$apply_filter, "Oui (Butterworth)", "Non")))
    if (input$apply_filter) {
      cat(sprintf("  - Frequence de coupure: %.1f Hz\n", input$fc))
      cat(sprintf("  - Ordre du filtre: %d\n", input$filter_order))
    }
    cat(sprintf("\nSample Entropy:\n"))
    cat(sprintf("  - Parametre m: %d\n", input$sampen_m))
    cat(sprintf("  - Parametre r: %.2f\n", input$sampen_r))
    cat(sprintf("  - Calcule sur: %s\n", ifelse(input$sampen_velocity, "Vitesse", "Position")))
    cat(sprintf("\nDate de l'analyse: %s\n", format(Sys.time(), "%d/%m/%Y %H:%M:%S")))
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
