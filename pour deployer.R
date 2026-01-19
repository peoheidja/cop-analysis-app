# Aller dans votre dossier
setwd("/Users/pierre-oliviermorin/Documents/app shiny/deploiement/cop-analysis-app/cop-analysis-app")
# Installer rsconnect si besoin
# install.packages("rsconnect")
rsconnect::setAccountInfo(name='po-morin', token='1F7F92F7219D13E9CC6974808E62AC2E', secret='nfTBIn2VnlVMEV0MapGk2CXq8MuIrfJvdQfzpBb2')
# Déployer (le script va vous guider)
source("deploy.R")
