#Código proyecto final de la asignatura Recuperación y extracción de información del MPGI. Autor: Miguel, Profesores: Riva y Fredy.

#Paquetes utilizados----

library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(netstat)
library(readr)
library(tidyverse)
library(tidyr)
library(beepr)
library(janitor)
library(gsubfn)
library(pdftools)


#Conexión remota al buscador avanzado de la CNA----

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]
remDr$navigate("https://www.cnachile.cl/paginas/buscador-avanzado.aspx")

#Extracción de metadatos-----

#Se realiza una previa navegación manual, seleccionando: Tipo de acreditación: "Pregrado", Año decisión "2019", Área del conocimiento "Educación" y finalmente "Buscar".

#Extracción página 1.----

html_cna2019 <- remDr$getPageSource()[[1]]

tabla <- read_html(html_cna2019)%>%
  html_elements(".RadGrid_Default")%>%
  html_table()%>%
  .[[1]]%>%
  janitor::clean_names()%>%
  slice(-1)%>%
  select(institucion:historial_de_decisiones)%>%
  filter(!is.na(historial_de_decisiones))%>%
  separate_rows(historial_de_decisiones)

enlaces <- read_html(html_cna2019)%>%
  html_elements("td:nth-child(2) a")%>%
  html_attr("href")%>%
  paste0()%>%
  .[-1:-5]

tabla_final_p1 <- tabla %>%
  mutate(enlaces_decision = enlaces)

#Extracción página 2.----

remDr$findElement(using = "name", value = "ctl00$ctl41$g_9b4714f1_9f31_478f_87d6_b2a9b7a73db2$ctl00$grid$ctl00$ctl03$ctl01$ctl14")$clickElement()

html_cna2019 <- remDr$getPageSource()[[1]]

tabla <- read_html(html_cna2019)%>%
  html_elements(".RadGrid_Default")%>%
  html_table()%>%
  .[[1]]%>%
  janitor::clean_names()%>%
  slice(-1)%>%
  select(institucion:historial_de_decisiones)%>%
  filter(!is.na(historial_de_decisiones))%>%
  separate_rows(historial_de_decisiones)%>%
  .[-31,]

enlaces <- read_html(html_cna2019)%>%
  html_elements("td:nth-child(2) a")%>%
  html_attr("href")%>%
  .[-1:-5]

tabla_final_p2 <- tabla %>%
  mutate(enlaces_decision = enlaces)

#Extracción páginas 3-5.----

extraer_3_5 <- function (){
  
  remDr$findElement(using = "name", value = "ctl00$ctl41$g_9b4714f1_9f31_478f_87d6_b2a9b7a73db2$ctl00$grid$ctl00$ctl03$ctl01$ctl14")$clickElement()
  
  Sys.sleep(10)
  
  html_cna2019 <- remDr$getPageSource()[[1]]

  tabla <- read_html(html_cna2019)%>%
    html_elements(".RadGrid_Default")%>%
    html_table()%>%
   .[[1]]%>%
    janitor::clean_names()%>%
    slice(-1)%>%
    select(institucion:historial_de_decisiones)%>%
    filter(!is.na(historial_de_decisiones))%>%
    separate_rows(historial_de_decisiones)
  
  enlaces <- read_html(html_cna2019)%>%
    html_elements("td:nth-child(2) a")%>%
    html_attr("href")%>%
    .[-1:-5]
  
  tabla_final_p3_5 <- tabla %>%
  mutate(enlaces_decision = enlaces)
  
  }

tabla_final_p3_p5 <- map_df(seq_len(3), ~extraer_3_5())

#Extracción página 6.----

remDr$findElement(using = "name", value = "ctl00$ctl41$g_9b4714f1_9f31_478f_87d6_b2a9b7a73db2$ctl00$grid$ctl00$ctl03$ctl01$ctl14")$clickElement()

html_cna2019 <- remDr$getPageSource()[[1]]

tabla <- read_html(html_cna2019)%>%
  html_elements(".RadGrid_Default")%>%
  html_table()%>%
  .[[1]]%>%
  janitor::clean_names()%>%
  slice(-1)%>%
  select(institucion:historial_de_decisiones)%>%
  filter(!is.na(historial_de_decisiones))%>%
  separate_rows(historial_de_decisiones)%>%
  .[-26,]

enlaces <- read_html(html_cna2019)%>%
  html_elements("td:nth-child(2) a")%>%
  html_attr("href")%>%
  .[-1:-5]

tabla_final_p6 <- tabla %>%
  mutate(enlaces_decision = enlaces)

#Extracción páginas 7 y 8.----

extraer_7_8 <- function (){
  
  remDr$findElement(using = "name", value = "ctl00$ctl41$g_9b4714f1_9f31_478f_87d6_b2a9b7a73db2$ctl00$grid$ctl00$ctl03$ctl01$ctl14")$clickElement()
  
  Sys.sleep(10)
  
  html_cna2019 <- remDr$getPageSource()[[1]]
  
  tabla <- read_html(html_cna2019)%>%
    html_elements(".RadGrid_Default")%>%
    html_table()%>%
    .[[1]]%>%
    janitor::clean_names()%>%
    slice(-1)%>%
    select(institucion:historial_de_decisiones)%>%
    filter(!is.na(historial_de_decisiones))%>%
    separate_rows(historial_de_decisiones)
  
  enlaces <- read_html(html_cna2019)%>%
    html_elements("td:nth-child(2) a")%>%
    html_attr("href")%>%
    .[-1:-5]
  
  tabla_final_p7_p8 <- tabla %>%
    mutate(enlaces_decision = enlaces)
  
  }

tabla_final_p7_p8 <- map_df(seq_len(2), ~extraer_3_5())

# Unión de todas las tablas.----

tabla_final <- rbind(tabla_final_p1, tabla_final_p2, tabla_final_p3_p5,tabla_final_p6, tabla_final_p7_p8)%>%
  filter(historial_de_decisiones == 2019)

#Creación de códigos de institución y carreras y consolidación de tabla final.----

codigo_institucion <- tabla_final %>%
  distinct(institucion)%>%
  mutate(codigo_institucion = c("SEK","UOH","UTA","UVM","UDLA","UADV","UNAB","UCSC","UANDES","UCT","UPLA","UCSH","UDP","UAP","ULS","UCEN","USS","UST","UAHC","UAH","UA","UACH","UBO","UC","UCN","UAT","UDEC","UDEL","UTAL","UMCE","UMAG"))

codigo_carrera <- tabla_final %>%
  distinct(carrera)%>%
  mutate(codigo_carrera = c("PARVULO","PARVULO","PARVULO","BASICA","BASICA","HISTORIA","INGLES","ARTES","LENGUAJE","BASICA","FILOSOFIA","MATEMATICAS","BIOLOGIA","ED-FISICA","MUSICA","QUIMICA","HISTORIA","LENGUAJE","BASICA","PEDAGOGIA","LENGUAJE","PEDAGOGIA","HISTORIA","LENGUAJE","BIOLOGIA","MATEMATICAS","FILOSOFIA","MATEMATICAS","ED-FISICA","CIENCIAS","DIFERENCIAL","INGLES","INGLES","QUIMICA","ARTES","LENGUAJE","INGLES","ALEMAN","PARVULO","FILOSOFIA","HISTORIA","BASICA","DIFERENCIAL","PEDAGOGIA","DIFERENCIAL","DIFERENCIAL","FISICA","MATEMATICAS","QUIMICA","ARTES","PEDAGOGIA","MATEMATICAS","DIFERENCIAL","DIFERENCIAL","BASICA","ALEMAN","ED-FISICA"))

tabla_final <- tabla_final%>%
  left_join(codigo_carrera)

tabla_final <- tabla_final%>%
  left_join(codigo_institucion) 

tabla_final <- tabla_final%>%
  mutate(nombre_archivo = paste0("datos_2019/",historial_de_decisiones, "_", codigo_institucion,"_",codigo_carrera, ".pdf"))

tabla_final <- tabla_final%>%
  mutate(link_final =  str_replace_all(enlaces_decision," ", "%20"))%>%
  mutate(link_final =  str_replace_all(link_final,"Ó", "%C3%93"))
  
tabla_final <- arrange(tabla_final, nombre_archivo)

#Descarga de archivos de acuerdos de acreditación en formato pdf.----

walk2(tabla_final$link_final, tabla_final$nombre_archivo, download.file, mode = "wb")

getwd()

setwd("C:/Users/miguel.mora/OneDrive - UNIVERSIDAD ANDRES BELLO/Escritorio/tesis_final/datos_2019")

lista_pdf <- list.files(path=getwd(), pattern=NULL, all.files=FALSE,
           full.names=FALSE)

texto_pdf <- lapply(lista_pdf, pdf_text)%>%
  as.character()

tabla_final <- tabla_final %>%
  mutate(texto_acuerdo = texto_pdf)

setwd("C:/Users/miguel.mora/OneDrive - UNIVERSIDAD ANDRES BELLO/Escritorio/tesis_final/tabla_final")

write_csv(tabla_final, "tabla_final.csv")



