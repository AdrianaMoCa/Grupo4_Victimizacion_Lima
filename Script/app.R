library(shiny)
library(shinydashboard)
library(rio)
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)

lima <- lima
vict_final <- vict_final

ui <- dashboardPage(
  dashboardHeader(title = "Sensacion de inseguridad en Lima Metropolitana"),
  dashboardSidebar(
    selectInput("Zona", "Seleccione la zona", choices = lima |> 
                  select(Zona) |>
                  distinct() |> 
                  arrange(Zona))
  ),
  dashboardBody(
    fluidRow(box(plotOutput("Victimizacion")), box(plotOutput("victimizacion_sexo"))), 
    fluidRow(box(plotOutput("vehiculo")), box(plotOutput("vehiculo_sexo"))),
    fluidRow(box(plotOutput("vivienda")), box(plotOutput("vivienda_sexo"))),
    fluidRow(box(plotOutput("sin_violencia")), box(plotOutput("sin_violencia_sexo"))),
    fluidRow(box(plotOutput("con_violencia")), box(plotOutput("con_violencia_sexo"))),
    fluidRow(box(tableOutput("indicador")))
  )
)

server <- function(input, output) {
  
  output$Victimizacion <- renderPlot({
    lima |> 
      ggplot(aes(x= victima_delito,  group=Zona)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
      labs(y = "Percent", fill="¿Usted cree ser víctima de algún delito?") + 
      facet_grid(~Zona) + 
      scale_y_continuous(labels=percent)+
      labs(title="Porcentaje de victimización por zona de Lima", x="¿Usted cree que puede ser víctima de algún delito?")+ 
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom")+ 
      theme(legend.position = "none")
    
  })
  
  output$victimizacion_sexo <- renderPlot({
    vict_sexo <- lima[lima$victima_delito %in% c("Sí"),]
  vict_sexo |> 
    ggplot(aes(x= Sexo,  group= victima_delito)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Sexo") + 
    facet_grid(~Zona) + 
    scale_y_continuous(labels=percent)+
    labs(title="Porcentaje de victimización por zona de Lima y por sexo", x="Sexo")+ 
    scale_fill_brewer(palette = "Set1") +
    theme(legend.position = "bottom")+ 
    theme(legend.position = "none")
    
    
  })
  
  output$vehiculo <- renderPlot({
    lima |> 
      filter(!is.na(Robo_vehiculo)) |> 
      ggplot(mapping = aes(x= Robo_vehiculo,  group=Zona)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop..),
                     y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima del robo de su vehículo?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo de vehículo", x="¿Usted cree que puede ser víctima del robo de su vehículo?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$vehiculo_sexo <- renderPlot({
    vehi_sexo <- lima[lima$Robo_vehiculo %in% c("Sí"),]
  vehi_sexo |> 
    filter(!is.na(Robo_vehiculo)) |> 
    ggplot(mapping = aes(x=Sexo ,  group=Robo_vehiculo)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima del robo de su vehículo?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo de vehículo", x="¿Usted cree que puede ser víctima del robo de su vehículo?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$vivienda <- renderPlot({
    lima |> 
      filter(!is.na(Robo_vivienda)) |> 
      ggplot(lima, mapping = aes(x= Robo_vivienda,  group=Zona)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop..),
                     y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima del robo de su vehículo?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo de vivienda", x="¿Usted cree que puede ser víctima del robo de su vivienda?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$vivienda_sexo <- renderPlot({
    vivi_sexo <- lima[lima$Robo_vivienda %in% c("Sí"),]
  
  vivi_sexo |> 
    filter(!is.na(Robo_vivienda)) |> 
    ggplot(lima, mapping = aes(x= Sexo,  group=Robo_vivienda)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima del robo de su vehículo?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo de vivienda", x="¿Usted cree que puede ser víctima del robo de su vivienda?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$sin_violencia <- renderPlot({
    lima |> 
      filter(!is.na(Robo_sinviolencia)) |> 
      ggplot(lima, mapping = aes(x= Robo_sinviolencia,  group=Zona)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop..),
                     y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo sin violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo sin violenciaa", x="¿Usted cree que puede ser víctima de robo sin violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$sin_violencia_sexo <- renderPlot({
    robosinviol_sexo <- lima[lima$Robo_sinviolencia %in% c("Sí"),]
  
  robosinviol_sexo |> 
    filter(!is.na(Robo_sinviolencia)) |> 
    ggplot(lima, mapping = aes(x= Sexo,  group=Robo_sinviolencia)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo sin violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo sin violencia", x="¿Usted cree que puede ser víctima de robo sin violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$con_violencia <- renderPlot({
    lima |> 
      filter(!is.na(Robo_conviolencia)) |> 
      ggplot(lima, mapping = aes(x= Robo_conviolencia,  group=Zona)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop..),
                     y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo con violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo con violenciaa", x="¿Usted cree que puede ser víctima de robo con violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$con_violencia_sexo <- renderPlot({
    roboconviol_sexo <- lima[lima$Robo_conviolencia %in% c("Sí"),]
  roboconviol_sexo |> 
    filter(!is.na(Robo_conviolencia)) |> 
    ggplot(lima, mapping = aes(x= Sexo,  group=Robo_conviolencia)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo con violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo con violencia", x="¿Usted cree que puede ser víctima de robo con violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    
  })
  
  output$indicador <- function(){
    vict_final |> 
      select(NOMBREDI, Indicador) |>
      arrange(desc(Indicador)) |> 
      kable() |> 
      kable_styling()
    
  }
  
  }


shinyApp(ui, server)
