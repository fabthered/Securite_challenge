

############# Application R-shinny ########################
# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(lubridate)
library(scales)
#library(readr)
#library(xlsx)
#library(stringr)
#library(tidyverse)


logs <- read.table("~/Perso/Univ Lyon2/Challenge/données/logs_fw-3.csv", sep=';', header=T)


## Only run examples in interactive R sessions

ui <- fluidPage( titlePanel("CHALLENGE SECURITE M2 SISE / OPSIE"),
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
                                  tabPanel('Analyse des flux', 
                                           #plotly::plotlyOutput('proto_hist') %>% withSpinner(color="darkgrey"),
                                           plotly::plotlyOutput('proto_bar') %>% withSpinner(color="darkgrey")
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
  
  # # affichage UDP/TCP par heure
  # output$proto_hist <- renderPlot({
  #   hour_of_event <- hour(logs$datetime)
  #   eventdata <- data.frame(datetime = logs$datetime, eventhour = hour_of_event)
  #   eventdata$Horaire <- eventdata$eventhour %in% seq(7, 18)
  #   eventdata$Horaire[eventdata$Horaire =="TRUE"] <-"Horaires ouvrés"
  #   eventdata$Horaire[eventdata$Horaire =="FALSE"] <-"Horaires non ouvrés"
  #   ggplot(eventdata, aes(x = eventhour, fill = Horaire)) +
  #     geom_histogram(breaks = seq(0,24), colour = "grey")
  # })

  output$proto_bar <- renderPlot({
    #création d'un tableau dynamique croisé pour afficher les freq par protocole
    diff_proto_1<-table(logs$proto)
    df_diff_proto_1 <-as.data.frame(diff_proto_1)
    
    barplot(df_diff_proto_1$Freq, main="Histogramme des protocoles utilisés",
            xlab="Protocoles",
            ylab="Nombre de Hits",col=c("lavender","lightcyan1"))
    #on affiche les valeurs de notre graph
    text(x = df_diff_proto_1$Var1, y = 50000, label = df_diff_proto_1$Freq, cex =1, pos=1.9)
    axis(1, at=df_diff_proto_1$Var1, labels=df_diff_proto_1$Var1,
         tick=FALSE, las=1, line=1, cex.axis=1)
  })
  
  
}


shinyApp(ui, server)
