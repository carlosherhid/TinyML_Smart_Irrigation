#Script created to retrieve temperature and humidity data from each weather station and assign them to each contract.
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(plyr)
library(knitr)
library(readr)
dat <- read_csv2(file.path("datos","TempHum","CA91.txt"),col_types = cols(.default = "c")) #It is necessary to read everything as characters because if not, it reads certain fields from some .txt files incorrectly.
ini_mediciones = as.Date("2018-01-16")  #Start date for filtering, from which we want to perform clustering on the data.
fin_mediciones = as.Date("2021-03-31") #End date for filtering the data on which we want to apply clustering.
#We remove the columns that are not needed.
new <- dat %>%
  dplyr::select(fecha,tmed,hrmed)
#We convert the temperature and humidity data to numeric values. They were in "Name characters."
#We remove the names.
names(new$tmed) <- NULL 
names(new$hrmed) <- NULL
#We need to convert characters to numbers.
new$tmed <- as.numeric(new$tmed)
new$hrmed <- as.numeric(new$hrmed)
new$fecha <- as.Date(new$fecha, format = "%d/%m/%y")
#We remove the rows containing NAs.
new <- na.omit(new)
#We group by dates and calculate the average of the measurements for each day.
h <- ddply(new, "fecha",colwise(mean))
#We keep only the dates for which we have NDVI data.
h1 <- h %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)