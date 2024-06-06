#Secondary script used for obtaining all the data related to irrigation zones and parks in a .csv
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
library(geosphere)
library(purrr)
table_parks <- DatGeo$reunirTablas() 
# Rename column names
colnames(table_parks) <- c("ID", "GreenAreas", "Polygons", "Largest", "Smallest", "Area", "Longest", "Shortest", "Perimeter", "MaxNodes", "MinNodes", "Nodes", "Abscissa", "North", "Contract", "Activity", "ExtInt", "PropTenant", "Address", "Indication", "AddressComplement", "Locality", "Neighborhood", "Meter", "MeterInstallDate", "Reading", "MeterType", "MeterModel", "Caliber", "ContractType", "Geometry")
# Save the correspondence between real and fake IDs to a CSV file
library(sf)
st_geometry(table_parks) <- "Geometry"
correspondence <- data.frame(ID_Real = table_parks$ID, ID_False = seq_len(nrow(table_parks)))
write.csv(correspondence, file = "correspondence_parks_b.csv", row.names = FALSE)

# Update the "ID" column with fake IDs starting from 1
table_parks$ID <- seq_len(nrow(table_parks))
write.csv(table_parks, file = "table_parks.csv", row.names = FALSE)
mapa <- DatGeo$plotMapa() #we can plot the map of all the irrigation zones
# We Generate summary statistics for the parks' dimensions
# Select the dimensions columns
dimension_columns <- c("Largest", "Smallest", "Area", "Longest", "Shortest", "Perimeter")

# Generate summary statistics for the selected columns
dimension_summary <- summary(table_parks[dimension_columns])

# Print the summary statistics
print(dimension_summary)