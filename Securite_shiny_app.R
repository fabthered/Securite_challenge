

############# Application R-shinny ########################
# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
#library(readr)
library(ggplot2)
library(xlsx)
library(stringr)
library(shinycssloaders)
library(tidyverse)
library(topicmodels)


## Only run examples in interactive R sessions

ui <- fluidPage( titlePanel("CHALLENGE SECURITE MS SISE / OPSIE"),
                 theme = shinythemes::shinytheme('flatly'),
                              sidebarPanel(width = 2,
                                radioButtons(
                                  inputId='protocole',
                                  label=tags$h4(strong("Protocole :"), color = "red"),
                                  choices = c("TCP","UDP","TPC & UDP"),
                                  selected = "TCP",
                                  inline = FALSE,
                                  width = NULL,
                                  choiceNames = NULL,
                                  choiceValues = NULL
                                ),
                                radioButtons(
                                  inputId='port',
                                  label=tags$h4(strong("Ports :"), color = "red"),
                                  choices = c("Inférieur à 1024","Supérieur à 1024","Tous les ports"),
                                  selected = "Inférieur à 1024",
                                  inline = FALSE,
                                  width = NULL,
                                  choiceNames = NULL,
                                  choiceValues = NULL
                                ),
                                sliderInput("parcourir", "Se balader dans parcourir",min = 1, max = 149, value = 1),
                                numericInput(
                                  inputId='nb_classes',
                                  label=tags$h4(strong("Nombre de classes :"), color = "red"),
                                  value=1,
                                  min = 1,
                                  max = 10,
                                  step = 1,
                                  width = NULL
                                ),
                                checkboxInput(
                                  inputId='inertie',
                                  label="Inertie pour CAH",
                                  value = FALSE,
                                  width = NULL)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Synthèse des flux' 
                                           #DT::dataTableOutput("contents") %>% withSpinner(color="darkgrey")
                                           ),
                                  tabPanel('Visualisation données brutes'
                                           #plotly::plotlyOutput('plot_age') %>% withSpinner(color="darkgrey")
                                           ),
                                  tabPanel("Parcourir")
                                  )
                                ) 
                              ) 



server <- function(input, output) {

}


shinyApp(ui, server)
