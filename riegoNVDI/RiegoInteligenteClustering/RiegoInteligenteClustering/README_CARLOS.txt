Esta carpeta contiene los archivos necesarios modificados para la ejecución del dashboard desarrollado en shiny que muestra todos los datos y análisis de RiegoInteligente y los datos del Clustering de parques almacenados en LOCAL. (carpeta "datos").
El Dashboard desarrollado en Shiny son dos archivos:
-"ui.R" - Es el archivo que se tiene que ejecutar como app, contiene la interfaz de usuario de la Shiny app.
-"server.R" - Es el archivo donde se realizan todos los cálculos relacionados con el server (actualización de datos, inputs, plots, etc).

El script en R "ClustC.R" es un script que realiza el clustering de parques a partir de unas series iniciales de forma simplificada. (una sola disimilitud y par aun número de clústers concreto).