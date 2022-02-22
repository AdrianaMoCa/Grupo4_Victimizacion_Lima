library(shiny)
library(shinydashboard)
library(rio)
library(factoextra)
library(cluster)
library(ggrepel)
library(tidyverse)
library(maps)
library(kableExtra)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(shinythemes)
library(datasets)
library(geojsonio)
library(sf)



lima <- lima
vict_final <- vict_final
data_mapa <- data_mapa

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("flatly"),
      "Sensación de inseguridad en Lima Metropolitana",
      tabPanel("Datos Iniciales",
               sidebarPanel(
                 tags$h3("Motivación"),
                 tags$h5("En el Perú los estudios sobre percepción de inseguridad ciudadana, son escasos; sin embargo, sumamente necesarios. 
                          Por ello, la siguiente plataforma propone alcanzar algunos datos sobre la situación de sensación de inseguridad en 
                          Lima Metropolitana."),
                 tags$h5("Para ello se hizo uso de la Encuesta Nacional Especializada sobre Victimización del 2017, la cual nos ha permitido 
                          identificar aquellos distritos que muestran mayor grado de inseguridad ciudadana, tomando en cuenta los tipo de delitos
                          patrimoniales y a partir de los cuales se elabora un indicador compuesto por estas variables."),
                 tags$h5("De esta manera, tener focalizados aquellos distritos con mayores índices de percepción de inseguridad y a partir de 
                          ellos elaborar mejores políticas de seguridad pública.")
                
              ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Victimización en Lima",
                            h4("Situación de la Victimización en Lima Metropolitana"),
                            tags$h5("En cuanto a la distribución de la población encuestada, se identifica que el grueso de esta se encuentra en la zona de 
                                    Lima Moderna (4366), seguido por todos aquellos distritos que conforman Lima Norte (3054). Mientras que aquellla zona con 
                                    una distribución menor corresponde a todos aquellos distritos que conforman Lima Centro (1633)"),
                            plotOutput("Victimizacion"),
                            tags$h5("La victimización se encuentra por encima del 80% en todos los sectores de Lima Metropolitana. Aunque, se puede puntualizar 
                                    algunas diferencias. Por ejemplo, la zona con mayor porcentaje de vicitmización es Lima Norte (88.83%) y Lima Este (88.59%), 
                                    mientras que la zona con el menor porcentaje de victimización es la de Lima Moderna (80.07%).")
                   ),
                   tabPanel("Victimización por zona",
                              plotOutput("vehiculo"), 
                              plotOutput("vivienda"), 
                              plotOutput("sin_violencia"), 
                              plotOutput("con_violencia")
                   ),
                   
                   tabPanel("Victimización por sexo", 
                            plotOutput("vehiculo_sexo"), 
                            plotOutput("victimizacion_sexo"),
                            plotOutput("vivienda_sexo"), 
                            plotOutput("sin_violencia_sexo"),
                            plotOutput("con_violencia_sexo")
                            )
                          )
                        )
              ),
      
      
      tabPanel("Indicador", 
               mainPanel(
                tabsetPanel(
                  tabPanel("Indicador PCA",
                           h4("Indicador de Sensación de Inseguridad por Distrito"),
                           tags$h5("A continuación presentaremos un mapa distrital de Lima Metropolitana,
                                   en el cual se evidencia aquellos distritos con mayor nivel de sensación
                                   inseguridad."),
                           plotOutput("data_mapa"),
                           tags$h5("De acuerdo al mapa, el distrito con mayor índice de sensación de inseguridad por 
                                   delitos patrimoniales es San Juan de Lurigancho, destacando muy por encima de los demas 
                                   distritos. Seguido de ello, se tiene a Comas, San Martín de Porres, Villa María del Triunfo 
                                   y Santiago de Surco. Por último, los distritos con menos nivel de sensación de inseguridad 
                                   son Pucusana, Santa Rosa y Punta Hermosa.")
                           ),
                  
                  tabPanel("Clúster No Jerárquico",
                           h4("Indicador de Sensación de Inseguridad por Distrito"),
                           tags$h5("En el siguiente mapa se diferencian 3 grupos de distrtitos: Alta sensación de inseguridad,
                                   Moderada sensación de inseguridad y Baja sensación de inseguridad."),
                           plotOutput("data_mapa1"),
                           tags$h5("De acuerdo al mapa (método no jerárquico - mediodes), si se conforman 3 clústers de acuerdo al 
                                   nivel de víctimización, se tiene que la mayoría de distritos se agruparían como nivel alto (19 distritos). 
                                   Seguido de ello, estarían aquellos con nivel moderado (17 distritos); y por último los de nivel bajo con 
                                   sólo 3 distritos. Añadir que bajo este método se obtiene un Average silhouette widht de 0.48.")
                           ),
                  
                  tabPanel("Clúster Jerárquico",
                           h4("Indicador de Sensación de Inseguridad por Distrito"),
                           tags$h5("En el siguiente mapa se diferencian 3 grupos de distrtitos: Alta sensación de inseguridad,
                                   Moderada sensación de inseguridad y Baja sensación de inseguridad"),
                           plotOutput("data_mapa2"),
                           tags$h5("De acuerdo al mapa (método jerárquico - divisivo), si se conforman 3 clústers de acuerdo al nivel de víctimización, 
                                   se tiene que sólo un distrito se agruparía como nivel alto (San Juan de Lurigancho). Seguido de ello, estarían aquellos 
                                   con nivel moderado, los cuales son la mayoría (35 distritos); y por último los de nivel bajo con sólo 3 distritos. 
                                   Añadir que bajo este método se obtiene un Average silhouette widht de 0.56.")
                           )
                          )
                    )
                  ),
                
      tabPanel("Contrastes y resultados", 
               sidebarPanel(
                 tags$h3("Podemos concluir"),
                 tags$h5("El distrito que muestra una mayor sensación de inseguridad en Lima 
                Metropolitana para el 2017 fue San Juan de Lurigancho, mientras que
                que aquellos clasificados con un bajo nivel de victimización fueron: 
                Punta Hermosa, Pucusana y Santa Rosa. Estos resultados pueden verse
                reflejados en los siguientes gráficos:"),
                plotOutput("data_mapa3"),
                plotOutput("data_mapa4")
                            )
              )
      )
  
  ),
  
server = function(input, output) {
    
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
    
    output$Victimizacion <- renderPlot({
      lima |>
        filter(!is.na(Zona)) |> 
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
        filter(!is.na(Zona)) |>
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
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_vehiculo)) |> 
        ggplot(aes(x= Robo_vehiculo,  group=Zona)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
        labs(y = "Percent", fill="¿Usted cree ser víctima del robo de su vehículo?") + 
        facet_grid(~Zona) + 
        scale_y_continuous(labels=percent)+
        labs(title="Porcentaje de victimización por robo de vehículo", x="¿Usted cree que puede ser víctima del robo de su vehículo?")+ 
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom")+ 
        theme(legend.position = "none")
    })
    
    output$vehiculo_sexo <- renderPlot({
      vehi_sexo <- lima[lima$Robo_vehiculo %in% c("Sí"),]
      vehi_sexo |>
        filter(!is.na(Zona)) |> 
        ggplot(aes(x= Sexo,  group=Zona)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
        labs(y = "Percent", fill="¿Usted cree ser víctima del robo de su vehículo?") + 
        facet_grid(~Zona) + 
        scale_y_continuous(labels=percent)+
        labs(title="Porcentaje de victimización por robo de vehículo por sexo", x="¿Usted cree que puede ser del robo de su vehículo?")+ 
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom")+ 
        theme(legend.position = "none")
    })
    
    output$vivienda <- renderPlot({
      lima |>
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_vivienda)) |> 
        ggplot(aes(x= Robo_vivienda,  group=Zona)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
        labs(y = "Percent", fill="¿Usted cree ser víctima del robo de su vivienda?") + 
        facet_grid(~Zona) + 
        scale_y_continuous(labels=percent)+
        labs(title="Porcentaje de victimización por robo de vivienda", x="¿Usted cree que puede ser víctima del robo de su vivienda?")+ 
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom")+ 
        theme(legend.position = "none")
    })
    
    output$vivienda_sexo <- renderPlot({
      vivi_sexo <- lima[lima$Robo_vivienda %in% c("Sí"),]
      vivi_sexo |> 
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_vehiculo)) |>
        ggplot(lima, mapping = aes(x= Sexo,  group=Robo_vivienda)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima del robo de su vehículo?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo de vivienda por sexo", x="¿Usted cree que puede ser víctima del robo de su vivienda?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    })
    
    output$sin_violencia <- renderPlot({
      lima |> 
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_sinviolencia)) |> 
        ggplot(lima, mapping = aes(x= Robo_sinviolencia,  group=Zona)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo sin violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo sin violencia", x="¿Usted cree que puede ser víctima de robo sin violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    })
    
    output$sin_violencia_sexo <- renderPlot({
      robosinviol_sexo <- lima[lima$Robo_sinviolencia %in% c("Sí"),]
      
      robosinviol_sexo |> 
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_sinviolencia)) |> 
        ggplot(mapping = aes(x= Sexo,  group=Zona)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo sin violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo sin violencia", x="¿Usted cree que puede ser víctima de robo sin violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    })
    
    output$con_violencia <- renderPlot({
      lima |> 
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_conviolencia)) |> 
        ggplot(lima, mapping = aes(x= Robo_conviolencia,  group=Zona)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser vítima de robo con violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo con violencia", x="¿Usted cree que puede ser víctima de robo con violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    })
    
    output$con_violencia_sexo <- renderPlot({
      roboconviol_sexo <- lima[lima$Robo_conviolencia %in% c("Sí"),]
      roboconviol_sexo |> 
        filter(!is.na(Zona)) |>
        filter(!is.na(Robo_conviolencia)) |> 
        ggplot(lima, mapping = aes(x= Sexo,  group=Robo_conviolencia)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="¿Usted cree que puede ser víctima de robo con violencia?") + facet_grid(~Zona) + scale_y_continuous(labels=percent)+labs(title="Porcentaje de victimización por robo con violencia por sexo", x="¿Usted cree que puede ser víctima de robo con violencia?")+ scale_fill_brewer(palette = "Set1")+theme(legend.position = "bottom")+theme(legend.position = "none")
    })
    
    output$data_mapa <- renderPlot({
      ggplot(data_mapa) +
      geom_sf(aes(fill = Indicador), colour = "grey75", size = 0.07) +
      labs(title = "Mapa de sensación de inseguridad en Lima Metropolitana (PCA)",
           caption = "Fuente: INEI 2017
       Elaboración: Grupo 4",
           x="Longitud",
           y="Latitud") +
      scale_fill_gradient2("Sensación de inseguridad", high= "red", mid = "orange", low = "white") +
      theme_bw()
        
        
      })
      
      
      output$data_mapa1 <- renderPlot({
      ggplot(data_mapa1) +
          geom_sf(aes(fill = as.factor(particion)))+
          labs(title = "Mapa de Lima Metropolitana según cluster de victimización",
               caption = "Fuente: INEI 2017
       Elaboración: Grupo 4",
               x="Longitud",
               y="Latitud") +
          scale_fill_discrete(name = "Clusters")
      
    })
    
    output$data_mapa2 <- renderPlot({
      ggplot(data_mapa2) +
        geom_sf(aes(fill = as.factor(divisivo)))+
        labs(title = "Mapa de Lima Metropolitana según cluster de victimización",
             caption = "Fuente: INEI 2017
       Elaboración: Grupo 4",
             x="Longitud",
             y="Latitud") +
        scale_fill_discrete(name = "Clusters")
      
    })
    
    output$data_mapa3 <- renderPlot({
      ggplot(data_mapa) +
        geom_sf(aes(fill = Indicador), colour = "grey75", size = 0.07) +
        labs(title = "Mapa de sensación de inseguridad en Lima Metropolitana (PCA)",
             caption = "Fuente: INEI 2017
       Elaboración: Grupo 4",
             x="Longitud",
             y="Latitud") +
        scale_fill_gradient2("Sensación de inseguridad", high= "red", mid = "orange", low = "white") +
        theme_bw()
      
      
    })
    
    output$data_mapa4 <- renderPlot({
      ggplot(data_mapa2) +
        geom_sf(aes(fill = as.factor(divisivo)))+
        labs(title = "Mapa de Lima Metropolitana según cluster de victimización",
             caption = "Fuente: INEI 2017
       Elaboración: Grupo 4",
             x="Longitud",
             y="Latitud") +
        scale_fill_discrete(name = "Clusters")
      
    })
    
    
    
  }
)