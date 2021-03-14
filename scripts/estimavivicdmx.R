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

#Viviendas
vivicdmx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/parquevivicdmx.csv",
                   encoding="latin",header=TRUE,check.names=FALSE)

#Hogares

hogcdmx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/hogarescdmx.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)

#Población
pobcdmx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/proypobcdmx.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)%>%
  #Filtrar datos hasta 2035 que es el límite del PGOT
  filter(year<=2035)


##Datos hogares y vivienda
##Generar copia para la distribución por alcaldía
vivicdmxalc<-vivicdmx
hogalc<-hogcdmx



#Procedimiento para estimar el parque de Ciudad de México----
vivicdmx<-vivicdmx %>% 
  gather(year, value, -nom_mun,-cve_mun)%>%
  mutate(year=as.numeric(year))%>%
  ##Dejar datos a partir de 2000
  filter(year>=2000)%>%
  ##Agrupar para obtener dato agregado
  group_by(year)%>%
  summarise(value=sum(value))


##Datos hogares
hogcdmx<-hogcdmx %>% 
  gather(year, value, -nom_mun,-cve_mun)%>%
  mutate(year=as.numeric(year))%>%
  ##Dejar datos a partir de 2000
  filter(year>=2000)%>%
  ##Agrupar para obtener dato agregado
  group_by(year)%>%
  summarise(value=sum(value))


##Consolidar base de datos
consol<-merge(vivicdmx, hogcdmx, by = "year")
consol<-merge(consol,pobcdmx,by="year",all.y = TRUE)



#Limpiar datos
consol<-consol%>%
  rename(vivi=value.x,
         hog=value.y,
         pob=value)%>%
  
  ##Calcular las tasas de jefatura y relación hogares/vivienda
  mutate(tj=hog/pob,
         hogviv=hog/vivi)


#condición de cierre del período de estimación
jef2035<-consol%>%
  filter(year==2010 | year==2020)%>%
  select(year,tj)%>%
  #Supuesto: tasa de crecimiento de jefaturas de los últimos 10 años
  mutate(jef2035=(tj+(tj-lag(tj,1))))%>%
  filter(year==2020)%>%
  select(jef2035)

##calcular tasa de jefatura de cierre
consol<-consol%>%
  mutate(tj=ifelse(year==2035,jef2035,tj))%>%
  mutate(tj=as.numeric(tj))

##Crecimiento esperado de las tasas de jefatura
crecjef<-consol%>%
  filter(year==2035| year==2020)%>%
  mutate(growth=100*((tj/lag(tj,1))^(1/15)-1))%>%
  filter(year==2035)%>%
  select(growth)%>%
  as.numeric()

consol<-consol%>%
  mutate(crec=crecjef)

#Funcíón para estimar tasas de jefatura de los periodos restantes
n=nrow(consol)
for(i in 2:n){
  if(is.na(consol$tj[i])){
    consol$tj[i] = consol$tj[i - 1] * (1+consol$crec[i]/100)
  }
}


##Calcular hogares con las tasas estimadas
consol<-consol%>%
  mutate(hog=pob*tj)


##calcular promedio de hogares sobre vivienda de los últimos 5 años
##Supuesto: Hogar censal: Hogares=vivienda
hogvivi<-consol%>%
  filter(year>=2015 & year<=2020)%>%
  mutate(media=mean(hogviv))%>%
  filter(year==2020)%>%
  select(media)%>%
  as.numeric()


#Usar la media calculada para obtener el parque estimado

consol<-consol%>%
  mutate(mediahogviv=hogvivi)%>%
  mutate(viviest=hog/mediahogviv,
         dif=viviest-lag(viviest))

#Guardar copia para pegar posteriormente
viviendas<-consol%>%
  select(year,viviest)

##Arreglo de la base de datos para graficar resultado
consol<-consol%>%
  mutate(vivi=ifelse(is.na(vivi),viviest,vivi))%>%
  mutate(tipo=ifelse((year==2020 | year==2015 | year==2010 |
                        year==2005 | year==2000),"Obs",
                     ifelse(year>2020,"Est",NA)))%>%
  #Habitantes por vivienda
  mutate(tam=pob/vivi)

#Ciudad de México. Gráfica parque habitacional----

consol%>%
  select(year,vivi,tipo)%>%
  filter(year>=2010)%>%
  mutate(vivi=ifelse(is.na(tipo),NA,vivi))%>%
  ggplot(.,aes(year,vivi,color=tipo))+
  geom_point(size=6)+ 
  scale_color_manual("Tipo",values=c("#addd8e","#999999"),
                     labels=c("Estimado","Censo, Conteo o Encuesta Intercensal",""))+
  scale_y_continuous("Número de viviendas",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2000, to = 2035, by = 5))+
  theme_minimal()+
  labs(
    title = "Ciudad de México. Viviendas particulares habitadas",
    subtitle = "2010-2035",
    caption = "Fuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))
##Salvar la gráfica

ggsave("estimacdmx/vivicdmx.png", height=10, width=20, units='in', dpi=300)

#Ciudad de México. Tablas con parque habitacional----

##Absolutos
consol%>%
  filter(year==2010|year==2015|year==2020|
           year==2025| year==2030 | year==2035)%>%
  select(year,vivi,tam)%>%
  mutate(dif=vivi-lag(vivi))%>%
  mutate(vivi=format(vivi,big.mark = ","),
         dif=format(round(dif,0),big.mark = ","),
         tam=format(round(tam,2)))%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Viviendas particulares habitadas</b></h><br>
2010-2035<br>',
        format="html",
        align = "c",
        col.names = c("Año",
                      "Viviendas", "Habitantes por vivienda",
                      "Crecimiento absoluto quinquenal"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#addd8e")%>%
  row_spec(1:3, bold = F, color = "black", background = "#bdbdbd")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.
Nota: Los datos sombreados corresponden a lo observado en Censos y Encuesta Intercensal",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablacdmx.png")



##Absolutos
consol%>%
  filter(year==2020|year==2035)%>%
  select(year,vivi)%>%
  mutate(dif=vivi-lag(vivi))%>%
  filter(year==2035)%>%
  select(dif)%>%
  mutate(dif=format(round(dif,0),big.mark = ","))%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Crecimiento absoluto viviendas particulares habitadas</b></h><br>
2020-2035<br>',
        format="html",
        align = "c",
        col.names = c("Crecimiento absoluto 2020-2035"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#addd8e")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.
Nota: Los datos sombreados corresponden a lo observado en Censos y Encuesta Intercensal",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablacdmxcrec.png")



consol%>%
  select(year,pob)%>%
  filter(year>=2010)%>%
  
  ggplot(.,aes(year,pob))+
  geom_line(color="#3cb050",size=3)+ 
  scale_y_continuous("Número de viviendas",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2000, to = 2035, by = 5))+
  theme_minimal()+
  labs(
    title = "Ciudad de México. Población",
    subtitle = "2010-2035",
    caption = "Fuente: Elaboración propia con datos de CONAPO. Proyecciones de población"
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))
##Salvar la gráfica

ggsave("estimacdmx/poblacdmx.png", height=10, width=20, units='in', dpi=300)




#Procedimiento para estimar parque de alcaldías: Modelo 1----
#Supuesto: Participación de 2020 invariable


#Seleccionar datos de 2019 a 2020

vivicdmxalcpart<-vivicdmxalc%>%
  select(cve_ent,cve_mun,nom_mun,"2020")%>%
  gather(year, vivi, -cve_ent,-nom_mun,-cve_mun)%>%
  #Pegar totales
  merge(.,vivicdmx,by="year")%>%
  #Obtener participaciones por año para cada alcaldía
  mutate(part=vivi/value)%>%
  
  ##Obtener participación promedio de cada alcaldía en el parque en 2020: 
  #Se supone que habrá poca diferencia en la estructura del parque en los próximos 15 años.
  
  group_by(nom_mun)%>%
  summarise(meanalc=mean(part))%>%
  ungroup()


viviconsolalc<-consol%>%
  select(year,vivi)

viviconsolalc<-merge(viviconsolalc,vivicdmxalcpart)%>%
  mutate(vivialc=meanalc*vivi)%>%
  select(year,nom_mun,vivialc)%>%
  filter(year>=2010)

##Pegar datos observados
vivicdmxalc<-vivicdmxalc%>%
  select(cve_ent,cve_mun,nom_mun,"2010":"2020")%>%
  gather(year, vivi, -cve_ent,-nom_mun,-cve_mun)%>%
  select(year,vivi,nom_mun)

viviconsolalc<-viviconsolalc%>%
  merge(.,vivicdmxalc,by=c("year","nom_mun"),all = TRUE)%>%
  mutate(vivialc=ifelse(year<=2020,vivi,vivialc))%>%
  select(year,nom_mun,vivialc)


##Crear lista para hacer loop de gráficas
lista_alc <- unique(viviconsolalc$nom_mun)


#Gráficas para cada una de las alcaldías----
for (i in seq_along(lista_alc)) {
  
  
  viviconsolalc%>%
    filter(nom_mun==lista_alc[i])%>%
    ggplot(.,aes(year,vivialc))+
    geom_point(size=6)+ 
    geom_vline(xintercept = 2020, linetype="dashed", 
               color = "blue", size=1.5)+
    scale_y_continuous("Número de viviendas",labels=comma)+
    scale_x_continuous("Años",
                       breaks = seq(from = 2010, to = 2035, by = 5))+
    theme_minimal()+
    labs(
      title =paste0(lista_alc[i],". ","Viviendas particulares habitadas"),
      subtitle = "2010-2035",
      caption = "Nota: La línea vertical punteada indica el inicio de la proyección.\nFuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO.")+
    theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
          plot.caption = element_text(hjust = 0,size=12),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          text=element_text(size=20))->p
  
  ##Salvar
  ggsave(p,file=paste0("estimacdmx/",lista_alc[i],".png"),
         height=10, width=20, units='in', dpi=300)
}    


viviconsolalc%>%
  mutate(vivialc = ifelse((year>2010 & year<2015) | (year>2015 & year<2020),NA,vivialc))%>%
  ggplot(.,aes(year,vivialc))+
  geom_point(size=3)+ 
  geom_vline(xintercept = 2021, linetype="dashed", 
             color = "blue", size=1.5)+
  scale_y_continuous("Número de viviendas",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2010, to = 2035, by = 5))+
  theme_bw()+
  labs(
    title =paste0("Viviendas particulares habitadas"),
    subtitle = "2010-2035",
    caption = "Nota: La línea vertical punteada indica el inicio de la proyección.\nFuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO.")+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))+
  facet_wrap(~ nom_mun, ncol=4, scale = "free_y")

##Salvar
ggsave("estimacdmx/acaldía1.png",
       height=10, width=20, units='in', dpi=300)




##Tabla parque habitacional alcaldías----

viviconsolalc<-viviconsolalc%>%
  
  gather(key, vivialc, -nom_mun, -year)%>% 
  filter(year==2020 | year==2025 |year==2030 | year==2035)%>%
  unite(new.col, c(key, year)) %>%   
  spread(new.col, vivialc)%>%
  #Columna de diferencia absoluta 2020-2035
  mutate(dif1=vivialc_2025-vivialc_2020,
         dif2=vivialc_2030-vivialc_2025,
         dif3=vivialc_2035-vivialc_2030,
         diferencia=vivialc_2035-vivialc_2020)

#Filas de totales

viviconsolalc<-viviconsolalc%>%
  bind_rows(viviconsolalc%>%
              summarise(vivialc_2020=sum(vivialc_2020),
                        vivialc_2025=sum(vivialc_2025),
                        vivialc_2030=sum(vivialc_2030),
                        vivialc_2035=sum(vivialc_2035),
                        dif1=vivialc_2025-vivialc_2020,
                        dif2=vivialc_2030-vivialc_2025,
                        dif3=vivialc_2035-vivialc_2030,
                        diferencia=sum(diferencia))%>%
              mutate(nom_mun="Total"))%>%
  ##Formato a números
  arrange(desc(diferencia))%>%
  mutate(vivialc_2020=format(round(vivialc_2020,0),big.mark = ","),
         vivialc_2025=format(round(vivialc_2025,0),big.mark = ","),
         vivialc_2030=format(round(vivialc_2030,0),big.mark = ","),
         vivialc_2035=format(round(vivialc_2035,0),big.mark = ","),
         dif1=format(round(dif1,0),big.mark = ","),
         dif2=format(round(dif2,0),big.mark = ","),
         dif3=format(round(dif3,0),big.mark = ","),
         diferencia=format(round(diferencia,0),big.mark = ","))




viviconsolalc%>%
  
  ##Crear tabla
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Viviendas particulares habitadas por alcaldía</b></h><br>
2020-2035<br>',
        format="html",
        align = "c",
        col.names = c("Alcaldía",
                      "2020","2025",
                      "2030","2035", "Diferencia 2025-2020",
                      "Diferencia 2030-2025",
                      "Diferencia 2035-2030",
                      "Diferencia absoluta 2020-2035"))%>%
  kable_styling(full_width = F)%>%
  add_header_above(c(" "," "," "," "," ", "Variaciones" = 4),
                   color="black",background="#addd8e")%>%
  row_spec(0, bold = F, color = "black", background = "#addd8e")%>%
  row_spec(1:17, bold = F, color = "black", background = "white")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablaalc.png")



#Modelo 2-----
#Supuesto: Cálculo de cada alcaldía y distribuir de acuerdo a la estimación general

##Importar la población a 2035 a nivel alcaldía.
#Nota: Las proyecciones de población de CONAPO a nivel municipio/alcaldía llegan a 2030
#Para estimar la población a 2035, se obtuvo la tasa de crecmiento de la población
#de cada una de las alcaldías de 2015 a 2020 y ese crecimiento, se aplicó a cada alcaldía


pobalca<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/pobla_alcaldia 2035.csv",
                   encoding="latin",header=TRUE,check.names=FALSE)



#
hogalcx<-hogalc%>% 
  gather(year, hog,-nom_mun,-cve_mun)%>%
  mutate(year=as.numeric(year))%>%
  ##Dejar datos a partir de 2000
  filter(year>=2000)


#
pobalcax<-pobalca%>% 
  gather(year, pob, -nom_mun,-cve_mun)%>%
  mutate(year=as.numeric(year))%>%
  ##Dejar datos a partir de 2000
  filter(year>=2000)



##Consolidar la población y hogares
consolalc<-merge(hogalcx, pobalcax, by = c("cve_mun","nom_mun","year"),all.y = TRUE)%>%
  #Calcular las tasas de jefatura
  mutate(tj=hog/pob)

#condición de cierre del período de estimación
jefalc2035<-consolalc%>%
  filter(year==2010 | year==2020)%>%
  select(cve_mun,nom_mun,year,tj)%>%
  group_by(cve_mun)%>%
  #Supuesto: tasa de crecimiento de jefaturas de los últimos 10 años
  mutate(jefalc2035=(tj+(tj-lag(tj,1))))%>%
  ungroup()%>%
  filter(year==2020)%>%
  select(cve_mun,nom_mun,jefalc2035)%>%
  rename(tj=jefalc2035)%>%
  mutate(year=2035)



##calcular tasa de jefatura de cierre

consolalc<-merge(consolalc,jefalc2035, by=c("cve_mun","nom_mun","year"),all.y=TRUE,all.x = TRUE)%>%
mutate(tj.x=ifelse(year==2035,tj.y,tj.x))%>%
select (-c(tj.y))%>%
  rename(tj=tj.x)


##Crecimiento esperado de las tasas de jefatura
crecjefalc<-consolalc%>%
  select(-c(hog,pob))%>%
  filter(year==2035| year==2020)%>%
  group_by(cve_mun)%>%
  mutate(growth=100*((tj/lag(tj,1))^(1/15)-1))%>%
  ungroup()%>%
  filter(year==2035)%>%
  select(cve_mun,nom_mun,growth)

consolalc<-merge(consolalc,crecjefalc, by=c("cve_mun","nom_mun"))%>%
  arrange(cve_mun)

#Funcíón para estimar tasas de jefatura de los periodos restantes
n=nrow(consolalc)

for(i in 2:n){

    if(is.na(consolalc$tj[i])){
    consolalc$tj[i] = consolalc$tj[i - 1] * (1+consolalc$growth[i]/100)
  }
}


##Calcular hogares con las tasas estimadas
consolalc<-consolalc%>%
  mutate(hog=pob*tj)%>%
  group_by(year)%>%
  mutate(tot=sum(hog))%>%
  ungroup()%>%
  mutate(part=hog/tot)%>%
  select(cve_mun,nom_mun,year,part)%>%
  merge(.,viviendas, by="year")%>%
  mutate(vivialc=part*viviest)



#Gráficas para cada una de las alcaldías----
for (i in seq_along(lista_alc)) {
  
  
  consolalc%>%
    filter(nom_mun==lista_alc[i])%>%
    ggplot(.,aes(year,vivialc))+
    geom_point(size=6)+ 
    geom_vline(xintercept = 2020, linetype="dashed", 
               color = "blue", size=1.5)+
    scale_y_continuous("Número de viviendas",labels=comma)+
    scale_x_continuous("Años",
                       breaks = seq(from = 2010, to = 2035, by = 5))+
    theme_minimal()+
    labs(
      title =paste0(lista_alc[i],". ","Viviendas particulares habitadas"),
      subtitle = "2010-2035",
      caption = "Nota: La línea vertical punteada indica el inicio de la proyección.\nFuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO.")+
    theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
          plot.caption = element_text(hjust = 0,size=12),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          text=element_text(size=20))->p
  
  ##Salvar
  ggsave(p,file=paste0("estimacdmx/",lista_alc[i],"x.png"),
         height=10, width=20, units='in', dpi=300)
}    


consolalc%>%
  mutate(vivialc = ifelse((year>2010 & year<2015) | (year>2015 & year<2020),NA,vivialc))%>%
  ggplot(.,aes(year,vivialc))+
  geom_point(size=3)+ 
  geom_vline(xintercept = 2021, linetype="dashed", 
             color = "blue", size=1.5)+
  scale_y_continuous("Número de viviendas",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2010, to = 2035, by = 5))+
  theme_bw()+
  labs(
    title =paste0("Viviendas particulares habitadas"),
    subtitle = "2010-2035",
    caption = "Nota: La línea vertical punteada indica el inicio de la proyección.\nFuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO.")+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))+
  facet_wrap(~ nom_mun, ncol=4, scale = "free_y")

##Salvar
ggsave("estimacdmx/acaldía2.png",
       height=10, width=20, units='in', dpi=300)




##Tabla parque habitacional alcaldías----

consoltabla<-consolalc%>%
  select(year,nom_mun,vivialc)%>%
  gather(key, vivialc, -nom_mun, -year)%>% 
  filter(year==2020 | year==2025 |year==2030 | year==2035)%>%
  unite(new.col, c(key, year)) %>%   
  spread(new.col, vivialc)%>%
  #Columna de diferencia absoluta 2020-2035
  mutate(dif1=vivialc_2025-vivialc_2020,
         dif2=vivialc_2030-vivialc_2025,
         dif3=vivialc_2035-vivialc_2030,
        diferencia=vivialc_2035-vivialc_2020)
#Filas de totales

consoltabla<-consoltabla%>%
  bind_rows(consoltabla%>%
              summarise(vivialc_2020=sum(vivialc_2020),
                        vivialc_2025=sum(vivialc_2025),
                        vivialc_2030=sum(vivialc_2030),
                        vivialc_2035=sum(vivialc_2035),
                        dif1=sum(dif1),
                        dif2=sum(dif2),
                        dif3=sum(dif3),
                        diferencia=sum(diferencia))%>%
              mutate(nom_mun="Total"))%>%
  ##Formato a números
arrange(desc(diferencia))%>%  
  mutate(vivialc_2020=format(round(vivialc_2020,0),big.mark = ","),
         vivialc_2025=format(round(vivialc_2025,0),big.mark = ","),
         vivialc_2030=format(round(vivialc_2030,0),big.mark = ","),
         vivialc_2035=format(round(vivialc_2035,0),big.mark = ","),
         dif1=format(round(dif1,0),big.mark = ","),
         dif2=format(round(dif2,0),big.mark = ","),
         dif3=format(round(dif3,0),big.mark = ","),
         diferencia=format(round(diferencia,0),big.mark = ","))




consoltabla%>%

  ##Crear tabla
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Viviendas particulares habitadas por alcaldía</b></h><br>
2020-2035<br>',
        format="html",
        align = "c",
        col.names = c("Alcaldía",
                      "2020","2025",
                      "2030","2035", "Diferencia 2025-2020",
                      "Diferencia
                      2030-2025",
                      "Diferencia
                      2035-2030",
                      "Diferencia absoluta
                      2020-2035"))%>%
  #column_spec(1:9,width = "30cm") %>%
  add_header_above(c(" "," "," "," "," ", "Variaciones" = 4),
                   color="black",background="#addd8e")%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#addd8e")%>%
  row_spec(1:17, bold = F, color = "black", background = "white")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablaalc2.png")
