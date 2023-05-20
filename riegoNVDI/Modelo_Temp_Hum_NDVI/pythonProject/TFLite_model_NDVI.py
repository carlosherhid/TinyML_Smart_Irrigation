import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow import keras

# 1. Preparar los datos de entrenamiento y prueba
# Leemos los datos de temperatura, anomalías, NDVI y humedad de las 5 estaciones meteorológicas
datos_CA42 = pd.read_csv('anomaliesCA42.csv', sep=';')
datos_CA42 = datos_CA42.dropna(
    subset=['anomaliesAgg'])  # Eliminamos las filas que contienen valores NaN en anomaliesAgg
df = datos_CA42
frecuencia = datos_CA42['anomaliesAgg'].value_counts()
correlacionT1 = df['tmed'].corr(df['anomaliesAgg'])
correlacionH1 = df['hrmed'].corr(df['anomaliesAgg'])
correlacionT2 = df['tmed'].corr(df['anomaliesAgg2'])
correlacionH2 = df['hrmed'].corr(df['anomaliesAgg2'])

df = datos_CA42.loc[datos_CA42['n_mediciones'] > 4]
df['tmed'].corr(df['anomaliesAgg'])
df['hrmed'].corr(df['anomaliesAgg'])
df['tmed'].corr(df['anomaliesAgg2'])
df['hrmed'].corr(df['anomaliesAgg2'])

datos_CA91 = pd.read_csv('anomaliesCA91.csv', sep=';')
datos_CA91 = datos_CA91.dropna(
    subset=['anomaliesAgg'], )  # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MO12 = pd.read_csv('anomaliesMO12.csv', sep=';')
datos_MO12 = datos_MO12.dropna(
    subset=['anomaliesAgg'])  # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MU21 = pd.read_csv('anomaliesMU21.csv', sep=';')
datos_MU21 = datos_MU21.dropna(
    subset=['anomaliesAgg'])  # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MU62 = pd.read_csv('anomaliesMU62.csv', sep=';')
datos_MU62 = datos_MU62.dropna(
    subset=['anomaliesAgg'])  # Eliminamos las filas que contienen valores NaN en anomaliesAgg

d1 = datos_MU21[
    ['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
     'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
     'mean_hum_last10', 'anomaliesAgg', 'anomaliesAgg2', 'anomaliesAgg3', 'n_mediciones']]
d2 = datos_MU62[
    ['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
     'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
     'mean_hum_last10', 'anomaliesAgg', 'anomaliesAgg2', 'anomaliesAgg3', 'n_mediciones']]
d3 = datos_CA91[
    ['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
     'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
     'mean_hum_last10', 'anomaliesAgg', 'anomaliesAgg2', 'anomaliesAgg3', 'n_mediciones']]
d4 = datos_CA42[
    ['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
     'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
     'mean_hum_last10', 'anomaliesAgg', 'anomaliesAgg2', 'anomaliesAgg3', 'n_mediciones']]
d5 = datos_MO12[
    ['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
     'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
     'mean_hum_last10', 'anomaliesAgg', 'anomaliesAgg2', 'anomaliesAgg3', 'n_mediciones']]

df = pd.concat([d1, d2, d3, d4, d5])

df['n_mediciones'].value_counts()
df = df.loc[df['n_mediciones'] > 70]

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
import numpy as np

# Split the data into input features and output variable
X = df[['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
        'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
        'mean_hum_last10', 'n_mediciones']]
y = df['anomaliesAgg3']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
y_testPREV = y_test
# Scale the input features
scaler = StandardScaler()
scalerY = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

y_train_scaled = scalerY.fit_transform(y_train.to_numpy().reshape(-1, 1)).ravel()
y_test_scaled = scalerY.transform(y_test.to_numpy().reshape(-1, 1)).ravel()
# x_train = X_train.astype('float32') / 255.0
# x_test = X_test.astype('float32') / 255.0

# 2. Definir la arquitectura del modelo de redes neuronales
model = keras.Sequential([
    keras.layers.Flatten(input_shape=[X_train.shape[1]]),
    keras.layers.Dense(128, activation='relu'),
    keras.layers.Dense(10, activation='softmax')
])

# 3. Compilar y entrenar el modelo
model.compile(optimizer='adam',
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])

model.fit(X_train, y_train, epochs=5, batch_size=32, verbose=1)

# 4. Guardar el modelo entrenado
model.save('modelo_entrenado.h5')

# 5. Convertir el modelo a TensorFlow Lite con cuantización
converter = tf.lite.TFLiteConverter.from_keras_model(model)
converter.optimizations = [tf.lite.Optimize.DEFAULT]
tflite_model = converter.convert()

# 6. Guardar el modelo cuantizado
with open('modelo_cuantizado.tflite', 'wb') as f:
    f.write(tflite_model)

# 7. Cargar el modelo cuantizado de TensorFlow Lite
interpreter = tf.lite.Interpreter(model_path='modelo_cuantizado.tflite')
interpreter.allocate_tensors()

# 8. Obtener los detalles de entrada y salida del modelo
input_details = interpreter.get_input_details()
output_details = interpreter.get_output_details()

# 9. Realizar predicciones sobre el conjunto de prueba
predictions = []
for data in X_test:
    # Preparar los datos de entrada
    input_data = np.expand_dims(data, axis=0)
    input_data = np.float32(input_data)
    interpreter.set_tensor(input_details[0]['index'], input_data)

    # Realizar la inferencia
    interpreter.invoke()

    # Obtener los resultados
    output_data = interpreter.get_tensor(output_details[0]['index'])
    predictions.append(np.argmax(output_data))

# 10. Calcular la precisión del modelo
accuracy = np.mean(np.equal(predictions, y_test)) * 100
print("Precisión del modelo cuantizado: {:.2f}%".format(accuracy))
