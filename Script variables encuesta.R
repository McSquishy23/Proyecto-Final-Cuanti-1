library(haven)
install.packages("labelled")
library(labelled)
library(tidyverse)
library(magrittr)
library(dplyr)

#############################################
############ Generando variables ############

#1. Generando variable de edad
Base_de_Datos_raw$edadCod <- factor(Base_de_Datos_raw$edad,
                                    labels = c("Menor de 18 años", "De 18 a 22", "De 23 a 27", "De 28 a 32", "De 41 a 50"))
#2. Generando variable de género
Base_de_Datos_raw$generoCod <- factor(Base_de_Datos_raw$genero, labels = c("Mujer", "Hombre",
                                                                           "No binario", "Prefiero no decir"))
table(Base_de_Datos_raw$genero) # vemos las que son usadas

#3. Generando variable de ser o no trans
Base_de_Datos_raw$es_transCod <- factor(Base_de_Datos_raw$es_trans, labels = c("Sí", "No"))
table(Base_de_Datos_raw$es_trans) # vemos las que son usadas

#4. Generando variable de sexualidad
Base_de_Datos_raw$sexualidadCod <- factor(Base_de_Datos_raw$sexualidad, labels = c("Queer", "Heterosexual",
                                                                                   "Prefiero no decirlo"))
table(Base_de_Datos_raw$sexualidad) # vemos las que son usadas

#5. Generando variable de provincia
Base_de_Datos_raw$provinciaCod <- factor(Base_de_Datos_raw$provincia, 
                                         labels = c("Alajuela", "Cartago", "Guanacaste",
                                                    "Heredia", "Limón", "Puntarenas",
                                                    "San José", "Otro"))

#6. Generando variable de carrera 1
Base_de_Datos_raw$carrera1Cod <- factor(Base_de_Datos_raw$carrera1, 
                                        labels = c("Ciencias Políticas", "Trabajo Social", "Geografía", "Antropología",
                                                   "Psicología", "Comunicación Colectiva", "Historia", "Derecho",
                                                   "Filología Clásica","Inglés", "Francés", "Estadística",
                                                   "Economía", "Dirección de Empresas", "Contaduría Publica",
                                                   "Administración Aduanera y Comercio Exterior", "Enseñanza de la Filosofía",
                                                   "Enseñanza de la Matemática", "Enseñanza de las Ciencias Naturales", "Biblotecología",
                                                   "Diseño Plástico", "Artes Dramáticas", "Ingeniería Industrial", 
                                                   "Ingeniería Eléctrica", "Ingeniería Quimica", "Ingeniería Mecanica", "Computación",
                                                   "Ingeniería Agricola y de Biosistemas","Agronomía", "Ingeniería en Alimentos","Física",
                                                   "Geología","Salud Ambiental", "Enfermería", "Nutrición", "Farmacia", "Odontología"))
#Respuestas son 37 diferentes, hay un problema con la carrera de Física en el código que estaba antes;
#la agregué en la posición correspondiente según la codificación. Si no corre el código de esto revisar en la tabla la
#cantidad de respuestas etc
table(Base_de_Datos_raw$carrera1)
#7. Generando variable de carrera 2
#aquí va el código
Base_de_Datos_raw$carrera2Cod <- factor(Base_de_Datos_raw$carrera2, 
                                        labels = c("Sociologia", "Filosofia", "Economia", "Musica"))

#8. Generando variable de año de ingreso
#aquí va el código
Base_de_Datos_raw$ano_ingresoCod <- factor(Base_de_Datos_raw$ano_ingreso, 
                                        labels = c("2024", "2023", "2022", "2021","2020", "2019", "2018", "2016", "2015"))

#9. Generando variable de ideología política - espectro
Base_de_Datos_raw$id_ideologia_espectroCod <-factor(Base_de_Datos_raw$id_ideologia_espectro, 
                                                    labels = c("Izquierda", "Centro izquierda","Centro",
                                                               "Centro derecha", "Derecha", "Otro"))

#10. Generando variable de ideología política - términos1
Base_de_Datos_raw$id_ideologia_terminos1Cod <- factor(Base_de_Datos_raw$id_ideologia_terminos1, 
                                                      labels = c("Anarquista", "Comunista", "Conservador",
                                                                 "Ecologista", "Fascista", "Liberal",
                                                                 "Libertario", "Nacionalista", "Progresista",
                                                                 "Socialdemócrata", "Socialista", "Otro"))
table(Base_de_Datos_raw$id_ideologia_terminos1)

#11. Generando variable de ideología política - términos2
Base_de_Datos_raw$id_ideologia_terminos2Cod <- factor(Base_de_Datos_raw$id_ideologia_terminos2, 
                                                      labels = c("Comunista", "Ecologista", "Liberal",
                                                                 "Libertario", "Nacionalista", "Progresista",
                                                                 "Socialdemócrata", "Socialista", "Otro"))
table(Base_de_Datos_raw$id_ideologia_terminos2)


#12. Generando variable de ideología política - términos3
Base_de_Datos_raw$id_ideologia_terminos3Cod <- factor(Base_de_Datos_raw$id_ideologia_terminos3, 
                                                      labels = c("Ecologista", "Liberal", "Libertario",
                                                                 "Nacionalista", "Neoliberal", "Progresista",
                                                                 "Socialdemócrata", "Socialista", "Otro"))
table(Base_de_Datos_raw$id_ideologia_terminos3)

#13. Generando variable de preferencia de partido político
Base_de_Datos_raw$pref_partido_polCod <-factor(Base_de_Datos_raw$pref_partido_pol, 
                                               labels = c( "PPSD", "PLN", "PNR", "PLP",
                                                           "FA", "Ninguno", "Otro"))

#14. Generando variable de militancia en partido político (sí/no)
Base_de_Datos_raw$militancia_partido_polCod <- factor(Base_de_Datos_raw$militancia_partido_pol,
                                                      labels = c("Sí", "No"))
table(Base_de_Datos_raw$militancia_partido_polCod)

#15. Generando variable de militancia en partido político (identificación por partido)
#aquí va el código

#16. Generando variable de preferencia de partido federativo
Base_de_Datos_raw$pref_partido_fedCod <- factor(Base_de_Datos_raw$pref_partido_fed, 
                                                labels = c("Alternativa", "Integra", 
                                                           "Ya Basta", "Ninguno", "Otro"))
table(Base_de_Datos_raw$pref_partido_fedCod)

#17. Generando variable de militancia en partido federativo (sí/no)
Base_de_Datos_raw$militancia_partido_fedCod <- factor(Base_de_Datos_raw$militancia_partido_fed,
                                                      labels = c("Sí", "No"))
table(Base_de_Datos_raw$militancia_partido_fedCod) 

#18. Generando variable de militancia en partido federativo (identificación por partido)
#aquí va el código


#19. Generando variable de participación en instancias universitarias (sí/no)
Base_de_Datos_raw$particip_inst_universitariasCod <- factor(Base_de_Datos_raw$particip_inst_universitarias,
                                                            labels = c("Sí", "No"))
table(Base_de_Datos_raw$particip_inst_universitariasCod)

#20. Generando variable de participación en instancias universitarias 1 (identificación por términos)
#aquí va el código
Base_de_Datos_raw$id_particip_inst_universitarias1Cod <- factor(Base_de_Datos_raw$id_particip_inst_universitarias1, 
                                           labels = c("Comisiones ante la Escuela de su carrera", "Junta Directiva",
                                                      "Representacion estudiantil ante la escuela", "Tribunal Electoral Estudiantil"
                                                      ,"Asociacion de Estudiantes de su Carrera", "Otra"))

#21. Generando variable de participación en instancias universitarias 2 (identificación por términos)
#aquí va el código
Base_de_Datos_raw$id_particip_inst_universitarias2Cod <- factor(Base_de_Datos_raw$id_particip_inst_universitarias2, 
                                                                labels = c("Junta Directiva","Representacion estudiantil ante la Escuela de su carrera",
                                                                           "Tribunal Electoral Estudiantil", "Asociacion de Estudiantes de su carrera"))

#22. Generando variable de participación en instancias universitarias 3 (identificación por términos)
#aquí va el código
Base_de_Datos_raw$id_particip_inst_universitarias2Cod <- factor(Base_de_Datos_raw$id_particip_inst_universitarias3, 
                                                               labels = c("Representacion estudiantil ante la Escuela de su carrera",
                                                                          "Tribunal Electoral Estudiantil", "Asociacion de Estudiantes de su carrera"))

#23. Generando variable de participación en instancias externas (sí/no)
Base_de_Datos_raw$particip_inst_externasCod <- factor(Base_de_Datos_raw$particip_inst_externas,
                                                      labels = c("Sí", "No"))
#24. Generando variable de participación en instancias externas 1 (identificación por términos)
#aquí va el código

#25. Generando variable de participación en instancias externas 2 (identificación por términos)
#aquí va el código

#26. Generando variable de participación en instancias externas 3 (identificación por términos)
#aquí va el código

#27. Generando variable de participación política en el futuro
Base_de_Datos_raw$particip_futuraCod <- factor(Base_de_Datos_raw$particip_futura,
                                               labels = c("Totalmente de desacuerdo", "En desacuerdo", "Ni en desacuerdo ni de acuerdo", "De acuerdo", "Totalmente de acuerdo"))


##############################################
############ Graficando variables ############

#1. Graficando variable de edad
tablaEdad <-table(Base_de_Datos_raw$edadCod)
tablaEdad
tablaEdadRel <- prop.table(x=tablaEdad)*100
tablaEdadRel
barplot(tablaEdadRel, main= "Gráfico 1: Porcentaje de participantes por edad", 
        xlab = "Edad", ylab = "Porcentaje",
        col = c("mediumorchid", "lightgreen", "lightyellow", "lightpink", "lightblue"),
        ylim = c(0, 80))

#2. Graficando variable de género
tablaGenero <- table(Base_de_Datos_raw$generoCod)
tablaGenero
tablaGeneroRel <- prop.table(x=tablaGenero)*100
tablaGeneroRel
barplot(tablaGeneroRel, main = "Gráfico 2: Porcentaje de identidad de género",
        xlab = "Género", ylab = "Porcentaje",
        col = c("mediumorchid", "palegreen4", "goldenrod1", "ivory4"),
        ylim = c(0, 60))

#3. Graficando variable de ser o no trans
tablaEs_trans <- table(Base_de_Datos_raw$es_transCod)
tablaEs_trans
tablaEs_transRel <- prop.table(x=tablaEs_trans)*100
tablaEs_transRel
barplot(tablaEs_transRel, main = "Gráfico 3: Porcentaje de identidad trans",
        xlab = "Es o no es trans", ylab = "Porcentaje",
        col = c("plum1", "lightblue1"),
        ylim = c(0, 100))


#4. Graficando variable de sexualidad
tablaSexualidad <- table(Base_de_Datos_raw$sexualidadCod)
tablaSexualidad
tablaSexualidadRel <- prop.table(x=tablaSexualidad)*100
tablaSexualidadRel
barplot(tablaSexualidadRel, main = "Gráfico 4: Porcentaje de identificación de sexualidad",
        xlab = "Sexualidad", ylab = "Porcentaje",
        col = c("palevioletred", "steelblue3", "wheat3"),
        ylim = c(0, 70))

#5. Graficando variable de provincia
tablaProvincia <-table(Base_de_Datos_raw$provinciaCod)
tablaProvincia
tablaProvinciaRel <- prop.table(x=tablaProvincia)*100
tablaProvinciaRel
plot(Base_de_Datos_raw$provinciaCod, main = "Gráfico 5: Provincia de origen",
     xlab = "provincia", ylab = "Frecuencia",  col = c("royalblue", "seagreen", "hotpink4",
                                                       "mistyrose3", "firebrick2",
                                                       "olivedrab4", "red4", "mediumpurple4"))

#6. Graficando variable de carrera 1
tablaCarrera1 <-table(Base_de_Datos_raw$carrera1Cod)
tablaCarrera1
tablacarrera1Rel <- prop.table(x=tablaCarrera1)*100
tablacarrera1Rel
plot(Base_de_Datos_raw$carrera1Cod, main = "Gráfico 6: Carrera del participante",
     xlab = "Carrera", ylab = "Frecuencia",  col = c("red", "green", "royalblue", "pink", "yellow",
                                                     "orange", "beige", "brown", "salmon", "gray", "violet", "purple","magenta",
                                                     "coral", "gold", "azure", "maroon", "cyan", "red4", "tomato", "sienna", "tan2",
                                                     
                                                     "turquoise", "white", "yellowgreen","tomato3" , "brown", "cadetblue", "gold4", "orange2",
                                                     "orchid", "snow", "palegreen", "steelblue3", "blueviolet", "olivedrab", "chartreuse1" ))

#7. Graficando variable de carrera 2
#aquí va el código
tablacarrera2 <-table(Base_de_Datos_raw$carrera2Cod)
tablacarrera2
tablacarrera2Rel <- prop.table(x=tablacarrera2)*100
tablacarrera2Rel
plot(Base_de_Datos_raw$carrera2Cod, main = "Gráfico 7. :Segunda Carrera del participante",
     xlab = "Carrera", ylab = "Frecuencia",  col = c("red","blue", "yellow", "pink" ))


#8. Graficando variable de año de ingreso
#aquí va el código
tablaano_ingreso <-table(Base_de_Datos_raw$ano_ingresoCod)
tablaano_ingreso
tablaano_ingresoRel <- prop.table(x=tablaano_ingreso)*100
tablaano_ingresoRel
plot(Base_de_Datos_raw$ano_ingresoCod, main = "Gráfico 7. :Ano de ingreso del participante",
     xlab = "Ano de ingreso", ylab = "Frecuencia",  col = c("red","blue", "yellow", "pink", "orange", "green", "brown", 'coral', "violet" ))


#9. Graficando variable de ideología política - espectro
tablaId_ideologia_espectro <-table(Base_de_Datos_raw$id_ideologia_espectroCod)
tablaId_ideologia_espectro
tablaId_ideologia_espectroRel <- prop.table(x=tablaId_ideologia_espectro)*100
tablaId_ideologia_espectroRel
plot(Base_de_Datos_raw$id_ideologia_espectroCod, main = "Gráfico 9: Porcentaje de identificación ideológica dentro del espectro político", xlab =
       "Espectro político", ylab = "Frecuencia",  col = c("red", "green", "royalblue", "pink", "yellow", "orange"))


#10. Graficando variable de ideología por términos 1
tablaIdeologiaTermin1 <- table(Base_de_Datos_raw$id_ideologia_terminos1Cod)
tablaIdeologiaTermin1
tablaIdeologiaTermin1Rel <- prop.table(x=tablaIdeologiaTermin1)*100
tablaIdeologiaTermin1Rel
barplot(tablaIdeologiaTermin1Rel, main = "Gráfico 10: Porcentaje de identificación ideológica por términos - 1",
        xlab = "Ideología", ylab = "Porcentaje",
        col = c("hotpink4", "mistyrose3", "firebrick2",
                "olivedrab4", "red4", "mediumpurple4",
                "salmon2", "cyan4", "sienna",
                "violetred", "dodgerblue3", "palegreen3"),
        ylim = c(0, 40))

#11. Graficando variable de ideología por términos 2
tablaIdeologiaTermin2 <- table(Base_de_Datos_raw$id_ideologia_terminos2Cod)
tablaIdeologiaTermin2
tablaIdeologiaTermin2Rel <- prop.table(x=tablaIdeologiaTermin2)*100
tablaIdeologiaTermin2Rel
barplot(tablaIdeologiaTermin2Rel, main = "Gráfico 11: Porcentaje de identificación ideológica por términos - 2",
        xlab = "Ideología", ylab = "Porcentaje",
        col = c("hotpink4", "olivedrab4", "mediumpurple4",
                "salmon2", "cyan4", "sienna",
                "violetred", "dodgerblue3", "palegreen3"),
        ylim = c(0, 40))


#12. Graficando variable de ideología por términos 3
tablaIdeologiaTermin3 <- table(Base_de_Datos_raw$id_ideologia_terminos3Cod)
tablaIdeologiaTermin3
tablaIdeologiaTermin3Rel <- prop.table(x=tablaIdeologiaTermin3)*100
tablaIdeologiaTermin3Rel
barplot(tablaIdeologiaTermin3Rel, main = "Gráfico 12: Porcentaje de identificación ideológica por términos - 3",
        xlab = "Ideología", ylab = "Porcentaje",
        col = c("olivedrab4", "mediumpurple4", "salmon2",
                "cyan4", "darkolivegreen2", "sienna",
                "violetred", "dodgerblue3", "palegreen3"),
        ylim = c(0, 40))


#13. Graficando variable de preferencia de partido político
tablapref_partido_pol<-table(Base_de_Datos_raw$pref_partido_polCod)
tablapref_partido_pol
tablapref_partido_polRel <- prop.table(x=tablapref_partido_pol)*100
tablapref_partido_polRel
plot(Base_de_Datos_raw$pref_partido_polCod, main = "Gráfico 13: Porcentaje de preferencia por un partido político",
     xlab = "Partido Político", ylab = "Frecuencia",  col = c("red", "green", "royalblue",
                                                                            "pink", "yellow", "orange", "beige"))

#14. Graficando variable de militancia en partido político (sí/no)
tablaMilitanciaPartidoPolitico <- table(Base_de_Datos_raw$militancia_partido_polCod)
tablaMilitanciaPartidoPolitico
MilitanciaPartidoPoliticoRel <- prop.table(x=tablaMilitanciaPartidoPolitico)*100
MilitanciaPartidoPoliticoRel
barplot(MilitanciaPartidoPoliticoRel, main = "Gráfico 14. Porcentaje de militancia en un partido político",
        xlab = "Militancia", ylab = "Porcentaje", col = c("pink", "lightblue"))

#15. Graficando variable de militancia en partido político (identificación por partido)
#aquí va el código


#16. Graficando variable de preferencia de partido federativo
tablaPreferenciaPartidoFederativo <- table(Base_de_Datos_raw$pref_partido_fedCod)
tablaPreferenciaPartidoFederativo
PreferenciaPartidoFederativoRel <- prop.table(x=tablaPreferenciaPartidoFederativo)*100
PreferenciaPartidoFederativoRel
barplot(PreferenciaPartidoFederativoRel, 
        main = "Gráfico 16. Porcentaje de preferencia en partidos federativos", 
        xlab = "Preferencia", ylab = "Porcentaje", 
        col = c("red", "goldenrod1", "brown", "lightblue", "pink"))

#17. Graficando variable de militancia en partido federativo (sí/no)
tablaMilitanciaPartidoFederativo <- table(Base_de_Datos_raw$militancia_partido_fedCod)
tablaMilitanciaPartidoFederativo
MilitanciaPartidoFederativoRel <- prop.table(x=tablaMilitanciaPartidoFederativo)*100
MilitanciaPartidoFederativoRel
barplot(MilitanciaPartidoFederativoRel, 
        main = "Gráfico 17. Porcentaje de militancia en partidos federativos", 
        xlab = "Militancia", ylab = "Porcentaje", col = c("seagreen", "lightblue"))

#18. Graficando variable de militancia en partido federativo (identificación por partido)
#aquí va el código


#19. Graficando variable de participación en instancias universitarias (sí/no)
tablaParticipacionInstanciasUniversitarias <- table(Base_de_Datos_raw$particip_inst_universitariasCod)
tablaParticipacionInstanciasUniversitarias
ParticipacionInstanciasUniversitariasRel <- prop.table(x=tablaParticipacionInstanciasUniversitarias)*100
ParticipacionInstanciasUniversitariasRel
barplot(ParticipacionInstanciasUniversitariasRel, 
        main = "Gráfico 19. Porcentaje de participación en instancias universitarias", xlab = "Participación", 
        ylab = "Porcentaje", col = c("brown", "royalblue"))

#20. Graficando variable de participación en instancias universitarias 1 (identificación por términos)
#aquí va el código
tablaid_particip_inst_universitarias1 <-table(Base_de_Datos_raw$id_particip_inst_universitarias1Cod)
tablaid_particip_inst_universitarias1
tablaid_particip_inst_universitarias1Rel <- prop.table(x=tablaid_particip_inst_universitarias1)*100
tablaid_particip_inst_universitarias1Rel


plot(Base_de_Datos_raw$id_particip_inst_universitarias1Cod, main = "Gráfico 20. :Participacion en instancias universitarias 1 ",
     xlab = "Instancia universitaria", ylab = "Frecuencia",  col = c( "pink", "orange", "green", "brown", 'coral', "violet" ))

#21. Graficando variable de participación en instancias universitarias 2 (identificación por términos)
#aquí va el código
tablaid_particip_inst_universitarias2 <-table(Base_de_Datos_raw$id_particip_inst_universitarias2)
tablaid_particip_inst_universitarias2
tablaid_particip_inst_universitarias2Rel <- prop.table(x=tablaid_particip_inst_universitarias2)*100
tablaid_particip_inst_universitarias2Rel

plot(Base_de_Datos_raw$id_particip_inst_universitarias2Cod, main = "Gráfico 21. :Participacion en instancias universitarias 2 ",
     xlab = "Instancia universitaria", ylab = "Frecuencia",  col = c("green", "brown",'coral',"violet"))

#22. Graficando variable de participación en instancias universitarias 3 (identificación por términos)
#aquí va el código
tablaid_particip_inst_universitarias3 <-table(Base_de_Datos_raw$id_particip_inst_universitarias3)
tablaid_particip_inst_universitarias3
tablaid_particip_inst_universitarias3Rel <- prop.table(x=tablaid_particip_inst_universitarias3)*100
tablaid_particip_inst_universitarias3Rel

plot(Base_de_Datos_raw$id_particip_inst_universitarias2Cod, main = "Gráfico 22. :Participacion en instancias universitarias 3 ",
     xlab = "Instancia universitaria", ylab = "Frecuencia",  col = c( "brown",'coral',"violet"))


#23. Graficando variable de participación en instancias externas (sí/no)
#aquí va el código
tablaParticipacionInstanciasExternas <- table(Base_de_Datos_raw$particip_inst_externasCod)
tablaParticipacionInstanciasExternas
tablaParticipacionInstanciasExternasRel <- prop.table (x=tablaParticipacionInstanciasExternas)*100
tablaParticipacionInstanciasExternasRel
barplot(tablaParticipacionInstanciasExternasRel,
        main = "Gráfico 23. Porcentaje de participación en instancias externas",
        xlab = "Participacion", ylab = "Porcentaje", col = c("lightgreen", "lightyellow"))


#24. Graficando variable de participación en instancias externas 1 (identificación por términos)
#aquí va el código

#25. Graficando variable de participación en instancias externas 2 (identificación por términos)
#aquí va el código

#26. Graficando variable de participación en instancias externas 3 (identificación por términos)
#aquí va el código

#27. Graficando variable de participación política en el futuro
#aquí va el código
tablaParticipacionFutura <- table(Base_de_Datos_raw$particip_futuraCod)
tablaParticipacionFutura
tablaParticipacionFuturaRel <- prop.table (x=tablaParticipacionFutura)*100
tablaParticipacionFuturaRel
barplot(tablaParticipacionFuturaRel, 
        main = "Gráfico 27. Porcentaje de personas que participarían en un futuro", 
        xlab = "¿Participaría?", ylab = "Porcentaje", col = c("purple", "lightgreen", "lightyellow", "lightpink", "lightblue"))


##################################################################
############ Estableciendo relaciones entre variables ############

#Relación entre género y sexualidad
tablaGenSex <- table(Base_de_Datos_raw$generoCod, Base_de_Datos_raw$sexualidadCod)
tablaGenSex
tablaGenSexRel <- prop.table(x=tablaGenSex)*100
tablaGenSexRel <- round(tablaGenSexRel, digits = 2)
tablaGenSexRel
#Graficando
barplot(tablaGenSexRel, main = "Relación entre género y sexualidad",
        xlab = "Sexualidad", ylab = "Género",
        col = c("violetred", "dodgerblue3", "palegreen3", "yellow"),
        ylim = c(0, 60))
par(xpd=TRUE)
#no sé cómo hacer para que las imágenes no se sobrelapen, wip
legend("left", inset=c(-0.6,0), xpd = NA,
       legend = c("Mujer", "Hombre", "No binario", "Prefiero no decir"),
       fill = c("violetred", "dodgerblue3", "palegreen3", "yellow"),
       title = ("Género"), bg="lightgrey")


Base_de_Datos_raw$generoCod <- factor(Base_de_Datos_raw$genero, labels = c("Mujer", "Hombre",
                                                                           "No binario", "Prefiero no decir"))

#Relación entre género e ideología por términos 1
tablaGenIdeolTerm1 <- table(Base_de_Datos_raw$generoCod, Base_de_Datos_raw$id_ideologia_terminos1Cod)
tablaGenIdeolTerm1
tablaGenIdeolTerm1Rel <- prop.table(x=tablaGenIdeolTerm1)*100
tablaGenIdeolTerm1Rel <- round(tablaGenIdeolTerm1Rel, digits = 2)
tablaGenIdeolTerm1Rel

#Relación entre género e ideología por términos 2
tablaGenIdeolTerm2 <- table(Base_de_Datos_raw$generoCod, Base_de_Datos_raw$id_ideologia_terminos2Cod)
tablaGenIdeolTerm2
tablaGenIdeolTerm2Rel <- prop.table(x=tablaGenIdeolTerm2)*100
tablaGenIdeolTerm2Rel <- round(tablaGenIdeolTerm2Rel, digits = 2)
tablaGenIdeolTerm2Rel

#Relación entre género e ideología por términos 3
tablaGenIdeolTerm3 <- table(Base_de_Datos_raw$generoCod, Base_de_Datos_raw$id_ideologia_terminos3Cod)
tablaGenIdeolTerm3
tablaGenIdeolTerm3Rel <- prop.table(x=tablaGenIdeolTerm3)*100
tablaGenIdeolTerm3Rel <- round(tablaGenIdeolTerm3Rel, digits = 2)
tablaGenIdeolTerm3Rel

#Relación entre Edad y Participación Futura
tablaEdadPartFutura <- table(Base_de_Datos_raw$particip_futuraCod, Base_de_Datos_raw$edadCod)
tablaEdadPartFutura
tablaEdadPartFuturaRel <- prop.table(x=tablaEdadPartFutura, margin = 2)*100
tablaEdadPartFuturaRel
#Graficando
barplot(tablaEdadPartFuturaRel, main = "Relación entre Edad y Participación Futura",
        xlab = "Edad", ylab= "Participación Futura", col = c("purple", "lightgreen", "lightyellow", "lightpink", "lightblue"))
legend(x="topright", legend = c("Totalmente en desacuerdo", "En desacuerdo", "Ni de acuerdo ni en desacuerdo", "De acuerdo", "Totalmente de acuerdo"),fill = c("purple", "lightgreen", "lightyellow", "lightpink", "lightblue"),
       title = ("Posición"))
