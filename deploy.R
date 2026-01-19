# Script de déploiement pour shinyapps.io
# =========================================

cat("🚀 Script de déploiement de l'application CoP\n\n")

# 1. Vérifier que rsconnect est installé
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("📦 Installation de rsconnect...\n")
  install.packages("rsconnect")
}

library(rsconnect)

# 2. Vérifier les packages nécessaires
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", 
  "plotly", "signal", "readxl", "writexl"
)

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("📦 Installation des packages manquants:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# 3. Informations sur le compte
cat("\n📋 Configuration du compte shinyapps.io\n")
cat("----------------------------------------\n")
cat("1. Allez sur: https://www.shinyapps.io/admin/#/tokens\n")
cat("2. Cliquez sur 'Show' puis 'Copy to clipboard'\n")
cat("3. Collez la commande rsconnect::setAccountInfo() ci-dessous\n\n")

# Vérifier si le compte est déjà configuré
accounts <- rsconnect::accounts()

if (nrow(accounts) == 0) {
  cat("⚠️  Aucun compte configuré. Veuillez exécuter:\n")
  cat("   rsconnect::setAccountInfo(name='...', token='...', secret='...')\n\n")
  cat("Voulez-vous continuer avec la configuration ? (o/n): ")
  reponse <- readline()
  
  if (tolower(reponse) != "o") {
    cat("❌ Déploiement annulé.\n")
    quit(save = "no")
  }
} else {
  cat("✅ Compte configuré:", accounts$name[1], "\n\n")
}

# 4. Déployer l'application
cat("🚀 Déploiement de l'application...\n")
cat("Cela peut prendre quelques minutes...\n\n")

# Nom de l'application (vous pouvez le changer)
app_name <- "cop-analysis"

# Liste des fichiers à déployer
app_files <- c(
  "app.R",
  "exemple_donnees_CoP.csv"
)

# Vérifier que tous les fichiers existent
missing_files <- app_files[!file.exists(app_files)]
if (length(missing_files) > 0) {
  cat("❌ Fichiers manquants:", paste(missing_files, collapse = ", "), "\n")
  cat("Assurez-vous d'être dans le bon répertoire.\n")
  quit(save = "no")
}

# Déploiement
tryCatch({
  rsconnect::deployApp(
    appName = app_name,
    appTitle = "Centre de Pression - Analyse Posturographique",
    appFiles = app_files,
    forceUpdate = TRUE,
    launch.browser = TRUE  # Ouvre automatiquement dans le navigateur
  )
  
  cat("\n✅ Déploiement réussi! 🎉\n")
  cat("URL: https://", accounts$name[1], ".shinyapps.io/", app_name, "/\n", sep = "")
  
}, error = function(e) {
  cat("\n❌ Erreur lors du déploiement:\n")
  cat(e$message, "\n")
  cat("\nVérifiez:\n")
  cat("1. Que votre compte est bien configuré\n")
  cat("2. Que vous avez une connexion internet\n")
  cat("3. Que le nom de l'app n'est pas déjà pris\n")
})

cat("\n📚 Pour mettre à jour l'application ultérieurement:\n")
cat("   source('deploy.R')\n")
