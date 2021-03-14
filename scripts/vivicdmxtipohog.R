#Script para estimar el parque habitacional de la Ciudad de México por género

#Nota: Para poder calibrar, es necesario correr primero el script estimavivicdmx


##Establecer directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/ord territorial")
##Se crea carpeta de almacenamiento
dir.create("estimacdmx/")


##Paquetería necesaria

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, scales,kableExtra)


#Datos género

pobcdmx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/proypobcdmx.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)%>%
  #Datos a partir de 2010 y hasta 2035
  filter(year>=2010 & year<=2035)


hogarvivi<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/hogtipocdmx.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)





  
  

hogarvivi<-hogarvivi%>% 
  gather(year, hog,-tipo)%>%
  mutate(year=as.numeric(year))%>%
  left_join(pobcdmx)%>%
  #Calcular las tasas de jefatura
  mutate(tj=hog/value)



#condición de cierre del período de estimación
jefa2035<-hogarvivi%>%
  filter(year==2010 | year==2020)%>%
  select(tipo,year,tj)%>%
  group_by(tipo)%>%
  #Supuesto: tasa de crecimiento de jefaturas de los últimos 10 años
  mutate(jefa2035=(tj+(tj-lag(tj,1))))%>%
  ungroup()%>%
  filter(year==2020)%>%
  select(tipo,jefa2035)%>%
  rename(tj=jefa2035)%>%
  mutate(year=2035)



##calcular tasa de jefatura de cierre

hogarvivi<-merge(hogarvivi,jefa2035, by=c("tipo","year"),all.y=TRUE,all.x = TRUE)%>%
  mutate(tj.x=ifelse(year==2035,tj.y,tj.x))%>%
  select (-c(tj.y))%>%
  rename(tj=tj.x)





##Crecimiento esperado de las tasas de jefatura
crecjefa<-hogarvivi%>%
  select(-c(hog,value))%>%
  filter(year==2035| year==2020)%>%
  group_by(tipo)%>%
  mutate(growth=100*((tj/lag(tj,1))^(1/15)-1))%>%
  ungroup()%>%
  filter(year==2035)%>%
  select(tipo,growth)



hogarvivi<-merge(hogarvivi,crecjefa, by=c("tipo"))



#Funcíón para estimar tasas de jefatura de los periodos restantes
n=nrow(hogarvivi)

for(i in 2:n){
  
  if(is.na(hogarvivi$tj[i])){
    hogarvivi$tj[i] = hogarvivi$tj[i - 1] * (1+hogarvivi$growth[i]/100)
  }
}




##Calcular hogares con las tasas estimadas
hogarvivi<-hogarvivi%>%
  mutate(hog=value*tj)%>%
  group_by(year)%>%
  mutate(tot=sum(hog))%>%
  ungroup()%>%
  mutate(part=hog/tot)%>%
  select(tipo,year,part)%>%
  merge(.,viviendas, by="year")%>%
  mutate(vivi=part*viviest)




hogarvivi%>%
  #mutate(vivialc = ifelse((year>2010 & year<2015) | (year>2015 & year<2020),NA,vivialc))%>%
  ggplot(.,aes(year,vivi,color=tipo))+
  geom_point(size=5 )+ 
  geom_vline(xintercept = 2021, linetype="dashed", 
             color = "blue", size=1.5)+
  scale_color_manual("Tipo de hogar",
                     values=c("#636363",
                              "#31a354",
                              "#fec44f",
                              "#8856a7",
                              "#9ecae1"))+
  scale_y_continuous("Número de viviendas",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2010, to = 2035, by = 5))+
  theme_bw()+
  labs(
    title =paste0("Viviendas particulares habitadas"),
    subtitle = "2010-2035",
    caption = "Nota: La línea vertical punteada indica el inicio de la proyección.\nFuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO. Proyecciones de población")+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))

##Salvar la gráfica

ggsave("estimacdmx/vivicdmxtipohog.png", height=10, width=20, units='in', dpi=300)




hogarvivitab<-hogarvivi%>%
  select(year,tipo,vivi)%>%
  gather(key, vivi, -tipo, -year)%>% 
  filter(year==2020 | year==2025 |year==2030 | year==2035)%>%
  unite(new.col, c(key, year)) %>%   
  spread(new.col, vivi)%>%
  #Columna de diferencia absoluta 2020-2035
  mutate(dif1=vivi_2025-vivi_2020,
         dif2=vivi_2030-vivi_2025,
         dif3=vivi_2035-vivi_2030,
         diferencia=vivi_2035-vivi_2020)



#Filas de totales

hogarvivitab<-hogarvivitab%>%
  bind_rows(hogarvivitab%>%
              summarise(vivi_2020=sum(vivi_2020),
                        vivi_2025=sum(vivi_2025),
                        vivi_2030=sum(vivi_2030),
                        vivi_2035=sum(vivi_2035),
                        dif1=sum(dif1),
                        dif2=sum(dif2),
                        dif3=sum(dif3),
                        diferencia=sum(diferencia))%>%
              mutate(tipo="Total"))%>%



##Formato a números
arrange(desc(diferencia))%>%  
  mutate(vivi_2020=format(round(vivi_2020,0),big.mark = ","),
         vivi_2025=format(round(vivi_2025,0),big.mark = ","),
         vivi_2030=format(round(vivi_2030,0),big.mark = ","),
         vivi_2035=format(round(vivi_2035,0),big.mark = ","),
         dif1=format(round(dif1,0),big.mark = ","),
         dif2=format(round(dif2,0),big.mark = ","),
         dif3=format(round(dif3,0),big.mark = ","),
         diferencia=format(round(diferencia,0),big.mark = ","))




hogarvivitab%>%
  
  ##Crear tabla
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Viviendas particulares habitadas por tipo de hogar</b></h><br>
2020-2035<br>',
        format="html",
        align = "c",
        col.names = c("Tipo",
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
                   color="black",background="#3cb050")%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#3cb050")%>%
  row_spec(1:6, bold = F, color = "black", background = "white")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda y Conapo. Proyecciones de población.",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablavivihogtipo.png")
