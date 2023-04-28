import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
import sklearn.metrics as sm
import numpy as np


# Leemos los datos de temperatura, anomalías, NDVI y humedad de las 5 estaciones meteorológicas
datos_CA42  = pd.read_csv('anomaliesCA42.csv')
datos_CA42 = datos_CA42.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_CA91  = pd.read_csv('anomaliesCA91.csv')
datos_CA91 = datos_CA91.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MO12  = pd.read_csv('anomaliesMO12.csv')
datos_MO12 = datos_MO12.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MU21  = pd.read_csv('anomaliesMU21.csv')
datos_MU21 = datos_MU21.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MU62  = pd.read_csv('anomaliesMU62.csv')
datos_MU62 = datos_MU62.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg

pd.set_option('max_columns', 99) # para poder ver todas las columnas
#print(datos_CA42.head(5))# Imprimimos las 5 primeras filas del dataframe para comprobar que está bien
#print(datos_CA42.describe()) # imprimimos un resumen de cada una de las columnas

# Seleccionamos el índice de anomalías producido en cada fecha, la temperatura y la humedad
# La temperatura y la humedad serán los inputs y el índice de anomalías el output del modelo
#CA42
labels_CA42 = np.array(datos_CA42['anomaliesAgg']) # índice de anomalías por fecha, valor que queremos predecir
print('Anomalías Ca42',labels_CA42)
names_CA42  = list(datos_CA42.columns) # Guardamos los nombres de las columnas
features_CA42  = np.array(datos_CA42[['tmed', 'hrmed']]) # temperatura y humedad, caracteristicas usadas como input del modelo
#CA91
labels_CA91 = np.array(datos_CA91['anomaliesAgg']) # índice de anomalías por fecha, valor que queremos predecir
print('Anomalías Ca91',labels_CA91)
names_CA91  = list(datos_CA91.columns) # Guardamos los nombres de las columnas
features_CA91  = np.array(datos_CA91[['tmed', 'hrmed']]) # temperatura y humedad, caracteristicas usadas como input del modelo
#MO12
labels_MO12 = np.array(datos_MO12['anomaliesAgg']) # índice de anomalías por fecha, valor que queremos predecir
print('Anomalías MO12',labels_MO12)
names_MO12  = list(datos_MO12.columns) # Guardamos los nombres de las columnas
features_MO12  = np.array(datos_MO12[['tmed', 'hrmed']]) # temperatura y humedad, caracteristicas usadas como input del modelo
#MU21
labels_MU21 = np.array(datos_MU21['anomaliesAgg']) # índice de anomalías por fecha, valor que queremos predecir
print('Anomalías MU21',labels_MU21)
names_MU21  = list(datos_MU21.columns) # Guardamos los nombres de las columnas
features_MU21  = np.array(datos_MU21[['tmed', 'hrmed']]) # temperatura y humedad, caracteristicas usadas como input del modelo
#MU62
labels_MU62 = np.array(datos_CA42['anomaliesAgg']) # índice de anomalías por fecha, valor que queremos predecir
print('Anomalías MU62',labels_MU62)
names_MU62  = list(datos_CA42.columns) # Guardamos los nombres de las columnas
features_MU62  = np.array(datos_CA42[['tmed', 'hrmed']]) # temperatura y humedad, caracteristicas usadas como input del modelo
#print(features_CA42)
#print(names_CA42)
#print(labels_CA42)

# Ahora unimos todos los datos de temperatura y humedad y los índices de anomalía de las cinco estaciones meteorológicas
labels = np.concatenate((labels_MU62,labels_MU21,labels_MO12,labels_CA91,labels_CA42))
#print(labels.shape) # tenemos 1137 mediciones del índice de anomalía en total
features = np.concatenate((features_MU62,features_MU21,features_MO12,features_CA91,features_CA42))
#print(features.shape)
# Dividimos el data set en conjunto de entrenamiento y test
train_features, test_features, train_labels, test_labels = \
   train_test_split(features, labels, test_size=0.25, random_state=42)
# Comprobamos que la división es correcta
print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# Establecemos una baseline que superar? por ejemplo que nuestro modelo tenga un error menor que el generado al hacer
# la media de todos los índices de anomalías?

# Realizamos el entrenamiento usando un modelo de regresión de random forest de skicit-learn
# Instanciamos el modelo con 1000 árboles de decisión

rf = RandomForestRegressor(n_estimators=1000, random_state=42)

# Entrenamos el modelo con los datos de training
rf.fit(train_features, train_labels)

# Realizamos predicciones usando el modelo entrenado y los datos de test
predicciones = rf.predict(test_features)
print(predicciones)
print(test_labels)
# calculamos la precision
# https://subscription.packtpub.com/book/data/9781789808452/1/ch01lvl1sec12/computing-regression-accuracy
print("Mean absolute error =", round(sm.mean_absolute_error(test_labels, predicciones), 2))
print("Mean squared error =", round(sm.mean_squared_error(test_labels, predicciones), 2))
print("Median absolute error =", round(sm.median_absolute_error(test_labels, predicciones), 2))
print("Explain variance score =", round(sm.explained_variance_score(test_labels, predicciones), 2))
print("R2 score =", round(sm.r2_score(test_labels, predicciones), 2))

