#global.R
required_packages <- c("shiny", "igraph", "visNetwork", "memoise", "DT", "xml2")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
library(shiny)
library(shinydashboard)
library(visNetwork)
library(xml2)
library(RColorBrewer)
library(DT)
library(memoise)
library(igraph)
library(future)
library(promises)

#fonctions utilitaires
source("utils.R")

# Palette de couleurs pour les compartiments
compartment_colors <- c(
  "cytosol" = "darkred",
  "extracellular region" = "darkgreen",
  "endoplasmic reticulum membrane" = "darkorange",
  "endoplasmic reticulum lumen" = "purple",
  "Golgi membrane" = "gold",
  "endocytic vesicle membrane" = "yellowgreen",
  "nucleoplasm" = "deeppink",
  "endosome lumen" = "cyan",
  "plasma membrane" = "royalblue",
  "unknown" = "blue",
  "nuclear envelope" = "darkmagenta",
  "mitochondrial inner membrane" = "brown3",
  "mitochondrial intermembrane space" = "gray"
)
