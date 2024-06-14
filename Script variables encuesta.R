library(haven)
install.packages("labelled")
library(labelled)
library(tidyverse)
library(magrittr)
library(dplyr)

#############################################
############ Generando variables ############

#1. Generando variable de edad
#aquí va el código

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
#aquí va el código

#6. Generando variable de carrera 1
#aquí va el código

#7. Generando variable de carrera 2
#aquí va el código

#8. Generando variable de año de ingreso
#aquí va el código

#9. Generando variable de ideología política - espectro
#aquí va el código

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
#aquí va el código

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

#21. Generando variable de participación en instancias universitarias 2 (identificación por términos)
#aquí va el código

#22. Generando variable de participación en instancias universitarias 3 (identificación por términos)
#aquí va el código

#23. Generando variable de participación en instancias externas (sí/no)
#aquí va el código

#24. Generando variable de participación en instancias externas 1 (identificación por términos)
#aquí va el código

#25. Generando variable de participación en instancias externas 2 (identificación por términos)
#aquí va el código

#26. Generando variable de participación en instancias externas 3 (identificación por términos)
#aquí va el código

#27. Generando variable de participación política en el futuro
#aquí va el código

##############################################
############ Graficando variables ############

#1. Graficando variable de edad
#aquí va el código

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
#aquí va el código

#6. Graficando variable de carrera 1
#aquí va el código

#7. Graficando variable de carrera 2
#aquí va el código

#8. Graficando variable de año de ingreso
#aquí va el código

#9. Graficando variable de ideología política - espectro
#aquí va el código


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
#aquí va el código

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

#21. Graficando variable de participación en instancias universitarias 2 (identificación por términos)
#aquí va el código

#22. Graficando variable de participación en instancias universitarias 3 (identificación por términos)
#aquí va el código

#23. Graficando variable de participación en instancias externas (sí/no)
#aquí va el código

#24. Graficando variable de participación en instancias externas 1 (identificación por términos)
#aquí va el código

#25. Graficando variable de participación en instancias externas 2 (identificación por términos)
#aquí va el código

#26. Graficando variable de participación en instancias externas 3 (identificación por términos)
#aquí va el código

#27. Graficando variable de participación política en el futuro
#aquí va el código



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
        xlab = "Sexualidad", ylab = "Género",  col = c("violetred", "dodgerblue3", "palegreen3", "yellow"),
        ylim = c(0, 60))




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
