#Script para estimar el parque habitacional de la Ciudad de México

##Borrar datos del entorno
rm(list=ls())

##Establecer directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/ord territorial")
##Se crea carpeta de almacenamiento
dir.create("estimacdmx/")


##Paquetería necesaria

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, scales,kableExtra,webshot)



##Leer archivos

#Población
pobalca<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/pobla_alcaldia 2035.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)


pobalca<-pobalca%>%
  gather(year, pob,-cve_ent,-cve_mun,-nom_mun)%>%
  mutate(year=as.numeric(year))



#Gráfico población


pobalca%>%
  mutate(pob = ifelse((year>2010 & year<2015) | (year>2015 & year<2020),NA,pob))%>%
  ggplot(.,aes(year,pob))+
  geom_point(size=3)+ 
  geom_vline(xintercept = 2021, linetype="dashed", 
             color = "blue", size=1.5)+
  geom_vline(xintercept = 2030, linetype="dashed", 
             color = "red", size=1.5)+
    scale_y_continuous("Población",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2010, to = 2035, by = 5))+
  theme_bw()+
  labs(
    title =paste0("Población por alcaldía"),
    subtitle = "2010-2035",
    caption = "Nota: La línea vertical punteada azul indica el inicio de la proyección de CONAPO.
La línea roja indica el inicio de estimaciones propias.    
Fuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO.")+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))+
  facet_wrap(~ nom_mun, ncol=4, scale = "free_y")


##Salvar
ggsave("estimacdmx/pobalc.png",
       height=10, width=20, units='in', dpi=300)



#Gráfico población para cada una de las alcaldías


listaalcaldia<-unique(pobalca$nom_mun)

for (i in seq_along(listaalcaldia)) {

pobalca%>%
    filter(nom_mun==listaalcaldia[i])%>%
  mutate(pob = ifelse((year>2010 & year<2015) | (year>2015 & year<2020),NA,pob))%>%
  ggplot(.,aes(year,pob))+
  geom_point(size=3)+ 
  geom_vline(xintercept = 2021, linetype="dashed", 
             color = "blue", size=1.5)+
  geom_vline(xintercept = 2030, linetype="dashed", 
             color = "red", size=1.5)+
  scale_y_continuous("Población",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2010, to = 2035, by = 5))+
  theme_bw()+
  labs(
    title =paste0(listaalcaldia[i],". Población"),
    subtitle = "2010-2035",
    caption = "Nota: La línea vertical punteada azul indica el inicio de la proyección de CONAPO.
La línea roja indica el inicio de estimaciones propias.    
Fuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO.")+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))

##Salvar
ggsave(paste0("estimacdmx/pob",listaalcaldia[i],".png"),
       height=10, width=20, units='in', dpi=300)

}

