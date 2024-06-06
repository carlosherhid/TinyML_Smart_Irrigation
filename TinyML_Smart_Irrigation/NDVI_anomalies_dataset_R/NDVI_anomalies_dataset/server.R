#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
dg <- dflt$dg()
#Inicialización de parámetros para el Clustering
################################################
ind = dflt$ind   #índice de medición, en este caso NDVI 
mask = dflt$mask #máscara, en este caso "scl_7_8_9"
agg = dflt$agg #"mean"
serie = dflt$serie #tipo de serie, "Verdor"
################################################
#Preparamos los contratos
################################################
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
if (serie == "Riego")
  contratos %<>% intersect(dflt$contratos("ConConsumos"))
# Replace content with numbers from 1 to the length of 'contratos'
replaced_contratos <- seq_len(length(contratos))

# Create a mapping between original and replaced IDs
contratos_mapping <- data.frame(
  Original = contratos,
  Replaced = replaced_contratos
)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$mapa <- leaflet::renderLeaflet({
    if (input$Tipo == "pols")
      DatGeo$plotMapa(dg, "Barrio", "Localidad", input$Localidad, contratos_mapping = contratos_mapping)
    else
      DatGeo$plotMapa(sf::st_centroid(dg), "Barrio", "Localidad", input$Localidad, contratos_mapping = contratos_mapping)
  })
    
    output$mymap <- leaflet::renderLeaflet({
      selected_replaced_id <- input$contrato
      selected_original_id <- contratos_mapping$Original[contratos_mapping$Replaced == selected_replaced_id]
      
      # Use selected_original_id for mapping back to original contract ID
      original_contract_id <- selected_original_id
      
      # Now use the original contract ID in your function
      DatGeo$plotMapa(dg, "Contrato", "Contrato", original_contract_id)
    })

})
