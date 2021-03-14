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

gencdmx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/pob_sexo.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)%>%
  #Datos a partir de 2010 y hasta 2035
  filter(year>=2010 & year<=2035)%>%
  #Tasas de jefatura por sexo
  mutate(tjh=hog_hom/hombre,
         tjm=hog_muj/mujer)



#condición de cierre del período de estimación
jef2035<-gencdmx%>%
  filter(year==2010 | year==2020)%>%
  select(year,tjm,tjh)%>%
  #Supuesto: tasa de crecimiento de jefaturas de los últimos 10 años
  mutate(jef2035_m=(tjm+(tjm-lag(tjm,1))),
         jef2035_h=(tjh+(tjh-lag(tjh,1)))
         )%>%
  filter(year==2020)%>%
  select(jef2035_m, jef2035_h)


##calcular tasa de jefatura de cierre
gencdmx<-gencdmx%>%
  mutate(tjh=ifelse(year==2035,jef2035$jef2035_h,tjh),
         tjm=ifelse(year==2035,jef2035$jef2035_m,tjm))%>%
  mutate(tjm=as.numeric(tjm),
         tjh=as.numeric(tjh))



##Crecimiento esperado de las tasas de jefatura
crecjef<-gencdmx%>%
  filter(year==2035| year==2020)%>%
  mutate(growthm=100*((tjm/lag(tjm,1))^(1/15)-1),
         growthh=100*((tjh/lag(tjh,1))^(1/15)-1)
         )%>%
  filter(year==2035)%>%
  select(growthm, growthh)



gencdmx<-gencdmx%>%
  mutate(crecm=crecjef$growthm,
         crech=crecjef$growthh)


#Funcíón para estimar tasas de jefatura de los periodos restantes
n=nrow(gencdmx)

for(i in 2:n){
  if(is.na(gencdmx$tjh[i])){
    gencdmx$tjh[i] = gencdmx$tjh[i - 1] * (1+gencdmx$crech[i]/100)
    
    for(i in 2:n){
      if(is.na(gencdmx$tjm[i])){
        gencdmx$tjm[i] = gencdmx$tjm[i - 1] * (1+gencdmx$crecm[i]/100)
        
        
      }
    }
    
  }
}

#calcular hogares

gencdmx<-gencdmx%>%
  mutate(hog_muj=ifelse(is.na(hog_muj),mujer*tjm,
                        hog_muj),
         hog_hom=ifelse(is.na(hog_hom),hombre*tjh,
                        hog_hom))%>%
  #Generar total y participación para calibrar
  mutate(tothog=hog_hom+hog_muj,
         partmujer=hog_muj/tothog)%>%
  select(year,partmujer)

#Hacer el pegado de la participación para calibrar

gentabla<-consol%>%
  left_join(gencdmx)%>%
  select(year, vivi,partmujer)%>%
  filter(year>=2010)%>%
  mutate(vivimujer=partmujer*vivi,
         vivihombre=vivi-vivimujer)




#Ciudad de México. Tablas con parque habitacional por tipo jefatura----

##Absolutos
gentabla%>%
  filter(year==2010|year==2015|year==2020|
           year==2025| year==2030 | year==2035)%>%
  mutate(parthombre=1-partmujer)%>%
  select(year, vivi,vivimujer,
         vivihombre,partmujer,
         parthombre)%>%
  mutate(vivi=format(vivi,big.mark = ","),
         vivimujer=format(round(vivimujer,0),big.mark = ","),
         vivihombre=format(round(vivihombre,0),big.mark = ","),
         partmujer=format(round(partmujer,3)*100),
         parthombre=format(round(parthombre,3)*100))%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Viviendas particulares habitadas y sexo del jefe del hogar</b></h><br>
2010-2035<br>',
        format="html",
        align = "c",
        col.names = c("Año",
                      "Viviendas", 
                      "Viviendas con jefatura femenina",
                      "Viviendas con jefatura masculina",
                      "% mujer",
                      "% hombre"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#3cb050")%>%
  row_spec(1:6, bold = F, color = "black", background = "white")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.
Nota: Se ocupa el concepto de hogar censal; es decir, un hogar=una vivienda",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablacdmxgen.png")




##Absolutos
gentabla%>%
  filter(year==2020|year==2035)%>%
  select(!partmujer)%>%
  mutate(diftot=vivi-lag(vivi),
         difmujer=vivimujer-lag(vivimujer),
         difhombre=vivihombre-lag(vivihombre))%>%
  filter(year==2035)%>%
  select(diftot,difmujer,difhombre)%>%
  mutate(diftot=format(round(diftot,0),big.mark = ","),
         difmujer=format(round(difmujer,0),big.mark = ","),
         difhombre=format(round(difhombre,0),big.mark = ","))%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Crecimiento absoluto viviendas particulares habitadas por jefatura</b></h><br>
2020-2035<br>',
        format="html",
        align = "c",
        col.names = c("Crecimiento absoluto total",
                      "Crecimiento viviendas con jefatura femenina",
                      "Crecimiento viviendas con jefatura masculina"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#3cb050")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.
Nota: Los datos sombreados corresponden a lo observado en Censos y Encuesta Intercensal",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablacdmxcrecjef.png")







#Ciudad de México. Gráfica parque habitacional----

gentabla%>%
  select(year,vivimujer,
         vivihombre)%>%
  mutate(tipo=ifelse(year=="2010" | year=="2015" | year=="2020",
                     "Censo o Intercensal", 
                     ifelse(year>2020,"Estimado","Nada")))%>%
                            
  gather(jefatura, vivi, -year, -tipo)%>%
  mutate(vivi=ifelse(tipo=="Nada",NA,vivi))%>%
  ggplot(.,aes(year,vivi,color=jefatura))+
  geom_point(size=6)+ 
  geom_vline(xintercept = 2021, linetype="dashed", 
             color = "blue", size=1.5)+
  scale_color_manual("Tipo",values=c("#999999","#3cb050"),
                     labels=c("Jefatura masculina",
                              "Jefatura femenina",""))+
  scale_y_continuous("Número de viviendas",labels=comma)+
  scale_x_continuous("Años",
                     breaks = seq(from = 2000, to = 2035, by = 5))+
  theme_minimal()+
  labs(
    title = "Ciudad de México. Viviendas particulares habitadas",
    subtitle = "2010-2035",
    caption = "Nota: La línea punteada azul muestra el inicio de la estimación. Previo a ésta, los datos corresponden a los Censos y Encuesta Intercensal.
Fuente: Elaboración propia con datos de INEGI. Censos de Población y Vivienda y CONAPO."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))
##Salvar la gráfica

ggsave("estimacdmx/vivicdmxjefatura.png", height=10, width=20, units='in', dpi=300)



##Estimación a nivel alcaldía====



pobalsx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/pob_sexo_alc.csv",
                  encoding="latin",header=TRUE,check.names=FALSE)


hogalcsx<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/pgot/main/hog_gen_alc.csv",
                   encoding="latin",header=TRUE,check.names=FALSE)


consolgen<-pobalsx%>%
  left_join(hogalcsx)%>%
  mutate(tj=hog/pob)


#condición de cierre del período de estimación
jefalc2035<-consolgen%>%
  select(nom_mun,year,gen,tj)%>%
  filter(year==2020 | year==2010)%>%
  group_by(nom_mun,gen)%>%
  #Supuesto: tasa de crecimiento de jefaturas de los últimos 10 años
  mutate(jefalc2035=(tj+(tj-lag(tj,1))))%>%
  ungroup()%>%
  filter(year==2020)%>%
  select(nom_mun,gen,jefalc2035)%>%
  rename(tj=jefalc2035)%>%
  mutate(year=2035)


consolgen<-merge(consolgen,jefalc2035, by=c("nom_mun","gen","year"),all.y=TRUE,all.x = TRUE)%>%
  mutate(tj.x=ifelse(year==2035,tj.y,tj.x))%>%
  select (-c(tj.y))%>%
  rename(tj=tj.x)




crecjefalc<-consolgen%>%
  select(nom_mun,year,gen,tj)%>%
  filter(year==2035 | year==2020)%>%
  group_by(nom_mun,gen)%>%
  mutate(growth=100*((tj/lag(tj,1))^(1/15)-1))%>%
  ungroup()%>%
  filter(year==2035)%>%
  select(nom_mun,gen,growth)




consolgen<-merge(consolgen,crecjefalc, by=c("nom_mun","gen"))



#Funcíón para estimar tasas de jefatura de los periodos restantes
n=nrow(consolgen)

for(i in 2:n){
  if(is.na(consolgen$tj[i])){
    consolgen$tj[i] = consolgen$tj[i - 1] * (1+consolgen$growth[i]/100)
    
    }
    
  }




#calcular hogares

consolgen<-consolgen%>%
  mutate(hog=ifelse(is.na(hog),pob*tj,
                        hog))%>%
#Generar total y participación para calibrar
  group_by(year,nom_mun)%>%
  mutate(tot=sum(hog),
         part=hog/tot)


#Hacer el pegado de la participación para calibrar

alcaldia<-consolalc%>%
  select(nom_mun,year,vivialc)

consolgen<-consolgen%>%
  left_join(alcaldia)%>%
  mutate(viviestimada=part*vivialc)%>%
  group_by(year,gen)%>%
  mutate(parttot=viviestimada/sum(viviestimada))%>%
  ungroup()
  


#Traer estimación de género

gen<-gentabla%>%
  select(year,vivimujer,vivihombre)%>%
  gather(gen, vivi, -year)%>%
  mutate(gen=ifelse(gen=="vivimujer","Mujeres","Hombres"))%>%
  filter(year==2010 | year==2015 | year>=2020 )

consolgen<-consolgen%>%
  left_join(gen)%>%
  mutate(vivi2=vivi*parttot/1000)


##Tabla
tablagen<-consolgen%>%
  select(year,nom_mun,vivi2,gen)

tablagen%>%
  filter(year==2020 | year==2035)%>%
  gather(variable,value,-nom_mun,-year,-gen) %>%
  unite(temp, year, gen)%>%
  spread(temp,value)%>%
  select(-variable)%>%
  mutate(difmuj=`2035_Mujeres`-`2020_Mujeres`,
         difhom=`2035_Hombres`-`2020_Hombres`)%>%
  arrange(desc(difmuj))%>%
  mutate(`2020_Hombres`=format(round(`2020_Hombres`,0),
                                 big.mark = ","),
           `2020_Mujeres`=format(round(`2020_Mujeres`,0),
                                 big.mark = ","),
         
           `2035_Hombres`=format(round(`2035_Hombres`,0),
                                 big.mark = ","),
           `2035_Mujeres`=format(round(`2035_Mujeres`,0),
                                 big.mark = ","),
         difmuj=format(round(difmuj,0),
                       big.mark = ","),
         difhom=format(round(difhom,0),
                       big.mark = ",")
         
         )%>%
           
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Viviendas particulares habitadas y sexo del jefe del hogar</b></h><br>
2020-2035<br>(miles)',
        format="html",
        align = "c",
        col.names = c("Alcaldía",
                      "Hogares 2020 hombres", 
                      "Hogares 2020 mujeres",
                      "Hogares 2035 hombres",
                      "Hogares 2035 mujeres",
                      "Crecimiento mujeres",
                      "Crecimiento hombres"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#3cb050")%>%
  row_spec(1:16, bold = F, color = "black", background = "white")%>%
  footnote(general = "Elaboración propia con datos de INEGI. Censos y Conteos de Población y Vivienda.
Nota: Se ocupa el concepto de hogar censal; es decir, un hogar=una vivienda",
           general_title = "
Fuente: ")%>%
  as_image(file="estimacdmx/tablacdmxgenalc.png")


