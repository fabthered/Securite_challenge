

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

#logs <- read.table("C:/Users/bapti/Onedrive/Bureau/Securite_challenge/logs_fw-3.csv", sep=';', header=T)
logs <- read.table("~/Perso/Univ Lyon2/Challenge/données/logs_fw-3.csv", sep=';', header=T)

# Pre traitement
hour_of_event <- hour(logs$datetime)
eventdata <- data.frame(datetime = logs$datetime, eventhour = hour_of_event)
eventdata$Horaire <- eventdata$eventhour %in% seq(7, 18)
eventdata$Horaire[eventdata$Horaire =="TRUE"] <-"Horaires ouvrés"
eventdata$Horaire[eventdata$Horaire =="FALSE"] <-"Horaires non ouvrés"

## Only run examples in interactive R sessions

ui <- fluidPage( titlePanel("CHALLENGE SECURITE M2 SISE / OPSIE"),
                 theme = shinythemes::shinytheme('flatly'),
                              sidebarPanel(width = 2,
                                radioButtons(
                                  inputId='protocole',
                                  label=tags$h4(strong("Protocole :"), color = "red"),
                                  choices = NULL,
                                  selected = "TCP",
                                  inline = FALSE,
                                  width = NULL,
                                  choiceNames = c("TCP","UDP","TCP & UDP"),
                                  choiceValues = c("TCP","UDP","TCP & UDP")
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
                                           plotly::plotlyOutput('proto_hist') %>% withSpinner(color="darkgrey"),
                                           plotOutput('proto_coord') %>% withSpinner(color="darkgrey"),
                                           plotOutput('proto_bar') %>% withSpinner(color="darkgrey"),
                                           fluidRow(
                                             column(6, 
                                                     tags$h4(("TOP 5 IP source les plus émettrices en TCP"), color = "red"),
                                                     tableOutput("top5_ipscr_tcp") %>% withSpinner(color="darkgrey")),
                                             column(6, 
                                                    tags$h4(("TOP 5 IP source les plus émettrices en UDP"), color = "red"),
                                                    tableOutput("top5_ipscr_udp") %>% withSpinner(color="darkgrey"))
                                           ),
                                           tags$h4(("TOP 10 des ports inférieurs à 1024 avec un accès autorisé"), color = "red"),
                                           tableOutput("top10_port") %>% withSpinner(color="darkgrey"),
                                           tags$h4(("Accès aux addresses IP non inclues dans le plan d'adressage"), color = "red"),
                                           tableOutput("adresses_ex") %>% withSpinner(color="darkgrey")
                                           ),
                                  tabPanel('Visualisation données brutes', DT::dataTableOutput("data_brut")
                                           
                                           ),
                                  tabPanel("Parcourir")
                                  )
                                ) 
                              ) 


server <- function(input, output) {
  
  #Filtre les données
  logs_filter <- reactive({
    logs %>%
      #filter(dstport = input$port) %>%
      filter(proto = input$protocole)
  })
    
  
  # affichage UDP/TCP par heure
  output$proto_hist <- plotly::renderPlotly({
    ggplot(eventdata, aes(x = eventhour, fill = Horaire)) +
      geom_histogram(breaks = seq(0,24), colour = "grey")
  })
  
  
  # affichage coord polaires
  output$proto_coord <- renderPlot({
    ggplot(eventdata, aes(x = eventhour, fill = Horaire)) +
    geom_histogram(breaks = seq(0,
                                24), colour = "grey") +
    coord_polar(start = 0) + theme_minimal() +
    scale_fill_brewer() + ylab("Somme") + ggtitle("Evenements par heure") +
    scale_x_continuous("", limits = c(0, 24),
                       breaks = seq(0, 24), labels = seq(0, 24))
    })
    
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
  
  #TOP 5 des IP sources les plus émettrices en TCP
  output$top5_ipscr_tcp <- renderTable({
    top5_ipscr_tcp <- data.frame(head(n=5,sort(table(subset(logs, logs$proto=="TCP", select=c(ipsrc))),decreasing = TRUE)))
    colnames(top5_ipscr_tcp) <- c('Adresse IP',"Fréquence")  
    return(top5_ipscr_tcp)
  })
  
  #TOP 5 des IP sources les plus émettrices en UDP
  output$top5_ipscr_udp <- renderTable({
    top5_ipscr_udp <- data.frame(head(n=5,sort(table(subset(logs, logs$proto=="UDP", select=c(ipsrc))),decreasing = TRUE)))
    colnames(top5_ipscr_udp) <- c('Adresse IP',"Fréquence")
    return(top5_ipscr_udp)
  })

  # TOP 10 des ports inférieurs à 1024 avec un accès autorisé
  output$top10_port <- renderTable({
    top10_port <- data.frame(head(n=10,sort(table(subset(logs, logs$dstport<=1024 & logs$action=="PERMIT", select=c(ipsrc))),decreasing = TRUE)))
    colnames(top10_port) <- c('Adresse IP',"Fréquence")
    return(top10_port)
    })

  # lister les accès des adresses non inclues dans le plan d’adressage de l’Université
  output$adresses_ex <- renderTable({
    adresses_ex <- data.frame(head(n=10,sort(table(subset(logs, logs$ipdst!="17.17.17.17", select=c(ipsrc))),decreasing = TRUE)))
    colnames(adresses_ex) <- c('Adresse IP',"Fréquence")
    return(adresses_ex)
  })
  
  
  
  output$data_brut <- DT::renderDataTable({
    logs
    
  })
  
  
}


shinyApp(ui, server)
