

library(readxl)
library(tidyverse)
library(DT)
library(rstatix)




options(scipen=999)


dfsalarios <- read_excel("Data/SalariosFut.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "date", "text"))
View(dfsalarios)


names(dfsalarios) <- str_replace(names(dfsalarios), 
                                 pattern = " ", replacement = "_")

dfsalariosFut <-  dfsalarios %>% 
  mutate(Edad=as.numeric(str_sub(JUGADOR, -7, -6)),
         NombreyP=str_sub(JUGADOR, 1, -8),
         Nombre=str_replace(NombreyP, "[A-Z]{3,}", ""),
         Pais_Origen=str_extract(JUGADOR, "[A-Z]{3,}"),
         Salario_a=str_replace(str_sub(SALARIO_ANUAL, 1, -2), ",", "."),
         Factor_anual=ifelse(
           str_sub(SALARIO_ANUAL, -1)=="M",
           1000000,
           1000),
         Salario_m=str_replace(str_sub(SALARIO_MENSUAL, 1, -2), ",", "."),
         Factor_mensual=ifelse(
           str_sub(SALARIO_MENSUAL, -1)=="M",
           1000000,
           1000),
         Salario_anual_EUR=as.numeric(Salario_a)*Factor_anual,
         Salario_mensual_EUR=as.numeric(Salario_m)*Factor_mensual,
         Salario_mensual_USD=Salario_mensual_EUR*1.05,
         Salario_mensual_MX=Salario_mensual_EUR*18.87) %>%
  select(Nombre,Edad, Pais_Origen,Salario_anual_EUR,Salario_mensual_EUR,
         Salario_mensual_USD, Salario_mensual_MX, Equipo, HASTA) %>% 
  na.omit() %>% 
  tibble()
view(dfsalariosFut)
  

dfsalariosGeneral <- read_csv("Data/SalaryGeneral_Data.csv")
names(dfsalariosGeneral) <- str_replace(names(dfsalariosGeneral), 
                                 pattern = " ", replacement = "_")

View(dfsalariosGeneral)


dfsalariosGlobal <- read_csv("Data/salary_Global.csv")
dfsalariosGlobal <- dfsalariosGlobal %>% 
  mutate(across(ends_with("salary"), .fns = ~.x * 18.01, .names = "{.col}_{.fn}"))
View(dfsalariosGlobal)
str(dfsalariosGlobal)

dfsalariosFut %>% 
  group_by(Equipo) %>% 
  summary()
# aqui vemos un resumen de los datos

unique(dfsalariosFut$Equipo)
# Los equipos
# 

unique(dfsalariosFut$Pais_Origen) 


fut_pais <- dfsalariosFut %>% 
  group_by(Pais_Origen) %>% 
  summarise(Conteo=n()) %>% 
  arrange(desc(Conteo))
# Pais de origen de los jugadores ordenados de mayor a menor
# 

ggplot(head(fut_pais, n = 12), mapping = aes(x = Pais_Origen, y = Conteo))+
  geom_col()



ggplot(head(fut_pais, n = 12), 
       mapping = aes(x = reorder(Pais_Origen, -Conteo), 
                     y = Conteo))+
  geom_col()

# Grafica de pais de origen de los jugadores
  

dfsalariosFut %>% 
  ggplot(aes(Edad))+
  geom_histogram(bins = 25)
# Cual es la eddad de los jugadores

# dfsalariosFut %>% 
#   ggplot(aes(Edad))+
#   geom_density()


# Salarios ----------------------------------------------------------------



dfsalariosFut %>% 
  filter(Salario_mensual_MX==max(Salario_mensual_MX, na.rm = T)) %>% 
  view()
# EL jugador mejor pagado

dfsalariosFut %>% 
  filter(Salario_mensual_MX==min(Salario_mensual_MX, na.rm = T)) %>% 
  view()
# El peor pagado
# 
dfsalariosFut$Salario_mensual_MX %>% 
  mean(trim = 0.05, na.rm = T)
# El promedio ajustado de salario mensual en pesos
# 

sd(dfsalariosFut$Salario_mensual_MX)
#  esto podria sugerir que los salarioss son muy desiguales,
#  asi que veamos

# ver sus distribuciones 


ggplot(dfsalariosFut, aes(Salario_mensual_MX))+
  geom_density()
# ver la distribucion de la variable salario


ggplot(dfsalariosFut, aes(Salario_mensual_MX))+
  geom_histogram(bins=25)





dfsalariosFut %>% 
  arrange(Salario_mensual_MX) %>% 
  view()


dfsalariosFut %>% 
  arrange(desc(Salario_mensual_MX)) %>% 
  view()


dfsalariosFut %>% 
  ggplot(aes(Salario_mensual_MX))+
  geom_boxplot()
# ver compportamiento de salario mensual

dfsalariosFut %>% 
  ggplot(aes(Equipo, Salario_mensual_MX, fill=Equipo))+
  geom_boxplot()
# boxplot por  equipos





# comparacion de medias ---------------------------------------------------

prueba_posthoc <- dfsalariosFut %>% 
  games_howell_test(Salario_mensual_MX ~ Equipo) %>% 
  filter(p.adj < 0.05) %>% 
  add_xy_position(x = "Equipo") %>% 
  mutate(p.corregido = p_format(p.adj, 
                                accuracy = 0.001, 
                                leading.zero = TRUE))

grafica <- dfsalariosFut %>% 
  ggplot(aes(Equipo, Salario_mensual_MX, fill=Equipo))+
  geom_boxplot()
grafica




# no incluir ---------------------------------------------------------


ggboxplot(dfsalariosFut, "Equipo", "Salario_mensual_MX") +  
 stat_pvalue_manual(prueba_posthoc, 
                     label = "{p.corregido}{'  '}{p.adj.signif}",
                     step.increase = 0.2,
                     bracket.nudge.y = 0.1,
                     vjust = -0.1)+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) %>%
  labs(subtitle = "Games Howell Test") +
  xlab("Transectos")+
  ylab("Indice de vegetacion")




# boxplot con grados de significancia
ggboxplot(Indices_Landsat, "Transect", "NDVI") +
  stat_pvalue_manual(prueba_posthoc, 
                     label = "{p.corregido}{'  '}{p.adj.signif}",
                     step.increase = 0.2,
                     bracket.nudge.y = 0.1,
                     vjust = -0.1)+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) %>%
  labs(subtitle = "Games Howell Test") +
  xlab("Transectos")+
  ylab("Indice de vegetacion")



