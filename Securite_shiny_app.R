

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
library(sqldf)
library(FactoMineR)
library(plotly)
library("factoextra")


#logs <- read.table("C:/Users/bapti/Onedrive/Bureau/Securite_challenge/logs_fw-3.csv", sep=';', header=T)
logs <- read.table("~/Perso/Univ Lyon2/Challenge/données/logs_fw-3.csv", sep=';', header=T)
logs <- logs[1:1000,]

logs$heure <- hour(logs$datetime)
logs$Horaire <- logs$heure %in% seq(7, 18)
logs$Horaire[logs$Horaire =="TRUE"] <-"Horaires ouvrés"
logs$Horaire[logs$Horaire =="FALSE"] <-"Horaires non ouvrés"
logs$date <- date(logs$datetime)
nbdays <- length(unique(logs$date))

analyse <- sqldf("select ipsrc, count(*) as nombre,
                 count(distinct ipdst) as cnbripdst, count(distinct dstport) as cnportdst,
                 sum(case when action like 'PERMIT'then 1 END) as permit,
                 sum(case when action like 'PERMIT' AND dstport < 1024 then 1 END) as inf1024permit,
                 sum(case when action like 'PERMIT' AND dstport >= 1024 then 1 END) as sup1024permit,
                 sum(case when action like 'DENY' AND (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) then 1 END) as adminpermit,
                 sum(case when action like 'DENY' then 1 END) as deny,
                 sum(case when action like 'DENY' AND dstport < 1024 then 1 END) as inf1024deny,
                 sum(case when action like 'DENY' AND dstport >= 1024 then 1 END) as sup1024deny,
                 sum(case when action like 'DENY' AND (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) then 1 END) as admindeny
                 from bdd group by ipsrc")


analyse[is.na(analyse)] <- 0

               
## Only run examples in interactive R sessions

ui <- fluidPage( titlePanel("CHALLENGE SECURITE M2 SISE / OPSIE"),
                 theme = shinythemes::shinytheme('flatly'),
                              sidebarPanel(width = 2,
                                radioButtons(
                                  inputId='protocole',
                                  label=tags$h4(strong("Protocole :"), color = "red"),
                                  choices = NULL,
                                  selected = "TCP & UDP",
                                  inline = FALSE,
                                  width = NULL,
                                  choiceNames = c("TCP & UDP","TCP","UDP"),
                                  choiceValues = c("TCP & UDP","TCP","UDP")
                                ),
                                radioButtons(
                                  inputId='port',
                                  label=tags$h4(strong("Ports :"), color = "red"),
                                  choices = c("Tous les ports","Inférieurs à 1024","Supérieurs à 1024"),
                                  selected = "Tous les ports",
                                  inline = FALSE,
                                  width = NULL,
                                  choiceNames = NULL,
                                  choiceValues = NULL
                                ),
                                # sliderInput("parcourir", "Se balader dans parcourir",min = 1, max = 149, value = 1),
                                # numericInput(
                                #   inputId='nb_classes',
                                #   label=tags$h4(strong("Nombre de classes :"), color = "red"),
                                #   value=1,
                                #   min = 1,
                                #   max = 10,
                                #   step = 1,
                                #   width = NULL
                                # ),
                                # checkboxInput(
                                #   inputId='inertie',
                                #   label="Inertie pour CAH",
                                #   value = FALSE,
                                #   width = NULL)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Analyse des flux', 
                                           tags$h4(("Historique des requêtes par jour par action"), color = "red"),
                                           plotly::plotlyOutput('date_hist') %>% withSpinner(color="darkgrey"),
                                           tags$h4(("Cumul des requêtes par heure par action"), color = "red"),
                                           plotly::plotlyOutput('proto_hist_action') %>% withSpinner(color="darkgrey"),
                                           fluidRow(
                                             column(6, 
                                                    tags$h4(("Cumul des requêtes par horaire"), col = "red"),
                                                    plotOutput('proto_coord') %>% withSpinner(color="darkgrey")),
                                             column(6,
                                                    tags$h4(("Distribution des requêtes TCP/UDP"), color = "red"),
                                                    plotOutput('proto_bar') %>% withSpinner(color="darkgrey"))
                                             ),

                                           fluidRow(
                                             column(6, 
                                                     tags$h4(("TOP 5 IP source les plus émettrices en TCP"), color = "red"),
                                                     tableOutput("top5_ipscr_tcp") %>% withSpinner(color="darkgrey")),
                                             column(6, 
                                                    tags$h4(("TOP 5 IP source les plus émettrices en UDP"), color = "red"),
                                                    tableOutput("top5_ipscr_udp") %>% withSpinner(color="darkgrey"))
                                           ),
                                           
                                           fluidRow(
                                             column(6, 
                                                    tags$h4(("TOP 10 des accès autorisés à des ports inférieurs à 1024"), color = "red"),
                                                    tableOutput("top10_port_inf") %>% withSpinner(color="darkgrey")),
                                             column(6,
                                                    tags$h4(("TOP 10 des accès non-autorisés à des ports supérieurs à 1024"), color = "red"),
                                                    tableOutput("top10_port_sup") %>% withSpinner(color="darkgrey"))
                                           ),
                                           tags$h4(("Accès aux addresses IP non inclues dans le plan d'adressage"), color = "red"),
                                           tableOutput("adresses_ex") %>% withSpinner(color="darkgrey")
                                           ),
                                  tabPanel('Visualisation données brutes', DT::dataTableOutput("data_brut")
                                           
                                           ),

                                  tabPanel("Parcourir",
                                           tags$h4(("Visualisation des requêtes refusées par IP source"), color = "red"),
                                           plotly::plotlyOutput('aggdata_deny') %>% withSpinner(color="darkgrey"),
                                           tags$h4(("Visualisation des requêtes acceptées par IP source"), color = "red"),
                                           plotly::plotlyOutput('aggdata_permit') %>% withSpinner(color="darkgrey")
                                  ),
         
                                  
                                  tabPanel("Data Mining",fluid = TRUE,titlePanel("filtres : "),
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput(inputId = 'nb_clust',label =  "Nombre de groupes", min=2, max=10, value=3, step=1)
                                             ),
                                             mainPanel(
                                               # Lay out the plot and table outputs in the UI as tabs.
                                               tabsetPanel(
                                                 tabPanel("Clustering", plotOutput("cluster")),
                                                 tabPanel("Analyse en composantes principales", plotlyOutput('plot_ACP'))
                                               )

                                    )

                                  )
                                ) 
                              ))) 


server <- function(input, output) {
  
  #Filtre les données
  logs_filter <- reactive({
    if (input$protocole != "TCP & UDP") {
      logs_filter <- logs %>%
        filter(proto == input$protocole)
      if (input$port == "Inférieurs à 1024") {
        logs_filter <- logs_filter %>%
          filter(dstport <= 1024)
      } else if (input$port == "Supérieurs à 1024") {
          logs_filter <- logs_filter %>%
          filter(dstport > 1024)
      } else {
          return(logs_filter)
      } 
    } else {
        if (input$port == "Inférieurs à 1024") {
          logs_filter <- logs %>%
            filter(dstport <= 1024)
        } else if (input$port == "Supérieurs à 1024") {
          logs_filter <- logs %>%
            filter(dstport > 1024)
        } else {
          return(logs)
        }
    }
  })
  
  output$cluster <- renderPlot({
    d = dist(analyse)
    cah = hclust(d, method = "ward.D2")
    plot(cah) 
    rect.hclust(cah, k=input$nb_clust)
  })
  
  output$plot_ACP<- renderPlotly({
    
    res.famd <- PCA(analyse[,-1], graph = FALSE)
    ind <- get_pca_ind(res.famd)
    coord = ind$coord
    coord <- coord[,0:2]
    coord_var <- res.famd$var$coord[,0:2]
    data <- as.data.frame(coord_var)
    graph <-  ggplot()+ theme_bw()  +
      geom_point(aes(coord[,1],coord[,2],text=analyse[,1])) + expand_limits(x = 0, y = 0) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      labs(title="Analyse en Composantes Principales",
           x ="Dimension 1", y = "Dimension 2")
    graph
    
    
  })
    
  output$date_hist <- plotly::renderPlotly({
    ggplot(logs_filter(), aes(x = date, fill=action)) +
      geom_histogram(bins=nbdays,colour = "grey")
  })
  
  # affichage UDP/TCP par heure par action
  output$proto_hist_action <- plotly::renderPlotly({
    ggplot(logs_filter(), aes(x = heure, fill = action)) +
      geom_histogram(breaks = seq(0,24), colour = "grey")
  })
  
  
  # affichage coord polaires
  output$proto_coord <- renderPlot({
    ggplot(logs_filter(), aes(x = heure, fill = Horaire)) +
    geom_histogram(breaks = seq(0,
                                24), colour = "grey") +
    coord_polar(start = 0) + theme_minimal() +
    scale_fill_brewer() + ylab("Somme") + ggtitle("Evenements par heure") +
    scale_x_continuous("", limits = c(0, 24),
                       breaks = seq(0, 24), labels = seq(0, 24))
    })
  
  
output$proto_bar <- renderPlot({  
  data <- as.data.frame(table(logs_filter()$proto))
  ggplot(data, aes(x=Var1, y = Freq)) + 
    geom_bar(stat="identity") +
    geom_text(aes(label=Freq, vjust=-0.5)) +
    xlab("Protocole") +
    ylab("Nombre de requêtes")
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
  output$top10_port_inf <- renderTable({
    top10_port_inf <- data.frame(head(n=10,sort(table(subset(logs, logs$dstport<=1024 & logs$action=="PERMIT", select=c(ipsrc))),decreasing = TRUE)))
    colnames(top10_port_inf) <- c('Adresse IP',"Fréquence")
    return(top10_port_inf)
    })
  
  # TOP 10 des ports supérieurs à 1024 avec un accès non-autorisé
  output$top10_port_sup <- renderTable({
    top10_port_sup <- data.frame(head(n=10,sort(table(subset(logs, logs$dstport>1024 & logs$action=="DENY", select=c(ipsrc))),decreasing = TRUE)))
    colnames(top10_port_sup) <- c('Adresse IP',"Fréquence")
    return(top10_port_sup)
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
  
  # Parcourir
  
  output$aggdata_deny <- plotly::renderPlotly({
  
      table1 <- aggregate(data=logs_filter(), dstport ~ ipsrc, function(x) length(unique(x)))
      table2 <- aggregate(data=logs_filter(), action ~ ipsrc, function(x) sum(x=='DENY'))
      table3 <- aggregate(data=logs_filter(), action ~ ipsrc, function(x) sum(x=='PERMIT'))
    
      aggdata <- cbind(table1, table2, table3)
      aggdata <- aggdata[,c(1,2,4,6)]
      colnames(aggdata) <- c('ipsrc','dstport','deny','permit')
    
      aggdata_deny <- subset(aggdata, aggdata$deny>0)
      
      ggplot(aggdata_deny, aes(x=dstport, y=deny, name=ipsrc)) +
        geom_point(color='red') +
        scale_x_log10() +
        scale_y_log10() + 
        xlab('Nombre de ports requêtés') +
        ylab('Nombre de requêtes refusées')
  
  })
  
  output$aggdata_permit <- plotly::renderPlotly({
    
    table1 <- aggregate(data=logs_filter(), dstport ~ ipsrc, function(x) length(unique(x)))
    table2 <- aggregate(data=logs_filter(), action ~ ipsrc, function(x) sum(x=='DENY'))
    table3 <- aggregate(data=logs_filter(), action ~ ipsrc, function(x) sum(x=='PERMIT'))
    
    aggdata <- cbind(table1, table2, table3)
    aggdata <- aggdata[,c(1,2,4,6)]
    colnames(aggdata) <- c('ipsrc','dstport','deny','permit')
    
    aggdata_permit <- subset(aggdata, aggdata$permit>0)
    
    ggplot(aggdata_permit, aes(x=dstport, y=deny, name=ipsrc)) +
      geom_point(color='green') +
      scale_x_log10() +
      scale_y_log10() + 
      xlab('Nombre de ports requêtés') +
      ylab('Nombre de requêtes acceptées')
    
  })
  
  
}


shinyApp(ui, server)
