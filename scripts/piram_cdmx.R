##Script para construir pirámide poblacional dinámica

##Borrar datos del entorno
rm(list=ls())

  if(!require('pacman')) install.packages('pacman')
  pacman::p_load(tidyverse, ggpol,extrafont,
                 lubridate, scales, gganimate, gifski)
  
  
  ##establecer directorio
  setwd("C:/Users/ALIENWARE/Documents/ord territorial")
  
##Descargar datos de proyecciones de población de CONAPO  
  pob<-read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv",
                 encoding="latin",
                header=TRUE,
                check.names=FALSE)%>%
  #Nombres a minúsculas
    janitor::clean_names()%>%
    select(!renglon)%>%
    #filtrar datos de CDMX
    filter(cve_geo==9)%>%
    ##Valores negativos para hombres
   mutate(poblacion=ifelse(sexo=="Hombres",
                           poblacion*-1,
                           poblacion))%>%
    #Transofrmar a miles
    mutate(poblacion=poblacion/1000)%>%
  ###Se crean grupos de edad
     mutate(agegroup= case_when ( 
     edad >=0 & edad <5 ~  "00-04",
  edad >4 & edad <10 ~ "05-09",
  edad >9 & edad <15 ~  "10-14",
  edad >14 & edad <20 ~  "15-19",
  edad >19 & edad <25 ~  "20-24",
  edad >24 & edad <30 ~  "25-29",
  edad >29 & edad <35 ~  "30-34",
  edad >34 & edad <40 ~  "35-39",
  edad >39 & edad <45 ~  "40-44",
  edad >44 & edad <50 ~  "45-49",
  edad >49 & edad <55 ~  "50-54",
  edad >54 & edad <60 ~  "55-59",
  edad >59 & edad <65 ~  "60-64",
  edad >64 & edad <70 ~  "65-69",
  edad >69 & edad <75 ~  "70-74",
  edad >74 & edad <80 ~  "75-79",
  edad >79 & edad <85 ~  "80-84",
  edad >=85 ~  "85+")
  )

  
  ##Se crea el gráfico estático
  
  pob%>%
    ggplot(aes(
      x = agegroup,
      y = poblacion,
      fill = sexo)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      values = c("#2ca25f", "#fdae6b")) +
    coord_flip() +
    facet_share(
      ~ sexo,
      dir = "h",
      scales = "free",
      reverse_num = TRUE)+
  ##Tema del gráfico
    theme(
      plot.background = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_text(size = 12),
      legend.key.size = unit(0.75, "cm"),
      legend.text = element_text(
        size = 15,
        face = "bold"
      ),
      plot.title = element_text(
        size = 20,
        hjust = 0.5,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 15,
        hjust = 0.5,
        face = "bold"
      ),
      axis.title.x = element_text(
        size = 10,
        face = "bold"
      ),
      plot.caption = element_text(
        size = 12,
        hjust = 0),
      text=element_text("Century Gothic"))+
    labs(
      title = "Ciudad de Mexico. Población 1970 - 2050\n\n{closest_state}",
      subtitle = "\n\nGrupos de edad",
      y = "\n\nPoblación (en miles)",
      caption = "\n\nFuente: @claudiodanielpc con información de 
CONAPO. Proyecciones de la Población de México 
y de las Entidades Federativas."
    )->p
  
  
  
  ##Animación
  p <- p +
    transition_states(ano,
                      transition_length = 1,
                      state_length = 2) +
    enter_fade() +
    exit_fade() +
    ease_aes("cubic-in-out")
  
  
  animate(
    p,
    fps = 20,
    nframes=100,
    duration = 15,
    width = 500,
    height = 500,
    end_pause = 30,
    renderer = gifski_renderer("piramcdmx.gif")
    )