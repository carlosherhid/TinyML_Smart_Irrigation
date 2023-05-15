#Script creado para obtener los datos de temperatura y humedad de cada estación meteorológica y asignarlos a cada contrato.
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(plyr)
library(knitr)
library(readr)
dat <- read_csv2(file.path("datos","TempHum","CA91.txt"),col_types = cols(.default = "c")) #Hay que leerlo todo como caracteres porque si no, lee mal ciertos campos de algunos .txt
ini_mediciones = as.Date("2018-01-16")  #Fecha de inicio para filtrar a partir de ahí los datos sobre los que queremos hacer clustering
fin_mediciones = as.Date("2021-03-31") #Fecha de fin para filtrar los datos sobre los que queremos aplicar clustering
#Suprimimos las columnas que no necesitamos
new <- dat %>%
  dplyr::select(fecha,tmed,hrmed)
#Transformamos los datos de temperatura y humedad a numéricos. Eran "Name characters"
#eliminamos los nombres
names(new$tmed) <- NULL 
names(new$hrmed) <- NULL
#Hay que transformar chr en num
new$tmed <- as.numeric(new$tmed)
new$hrmed <- as.numeric(new$hrmed)
new$fecha <- as.Date(new$fecha, format = "%d/%m/%y")
#Quitamos las filas que contengan NAs
new <- na.omit(new)
#Agrupamos por fechas y obtenemos la media de las mediciones de cada día
h <- ddply(new, "fecha",colwise(mean))
#Nos quedamos solo con las fechas para las que tenemos datos de NVDI
h1 <- h %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)