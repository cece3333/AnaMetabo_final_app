# Fichier app.R
source("global.R")
source("ui.R") # Interface utilisateur
source("server.R")

shinyApp(ui = ui, server = server)