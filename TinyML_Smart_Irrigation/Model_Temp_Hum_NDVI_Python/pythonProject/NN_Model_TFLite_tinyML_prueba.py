import keras
import pandas as pd


# Leemos los datos de temperatura, anomalías, NDVI y humedad de las 5 estaciones meteorológicas
from tensorflow.python.keras.models import load_model

datos_CA42 = pd.read_csv('anomaliesCA42.csv', sep=';')
datos_CA42 = datos_CA42.dropna(
    subset=['anomaliesAgg'])  # Eliminamos las filas que contienen valores NaN en anomaliesAgg
df = datos_CA42
frecuencia = datos_CA42['anomaliesAgg'].value_counts()
correlacionT1 = df['tmed'].corr(df['anomaliesAgg'])
correlacionH1 = df['hrmed'].corr(df['anomaliesAgg'])
correlacionT2 = df['tmed'].corr(df['anomaliesAgg2'])
correlacionH2 = df['hrmed'].corr(df['anomaliesAgg2'])

df = datos_CA42.loc[datos_CA42['n_measurements'] > 4]
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

d1 = datos_MU21
d2 = datos_MU62
d3 = datos_CA91
d4 = datos_CA42

d5 = datos_MO12

df = pd.concat([d1, d2, d3, d4, d5])

df['n_measurements'].value_counts()
df = df.loc[df['n_measurements'] > 70]

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
import numpy as np
from keras.models import Sequential
from keras.layers import Dense

# Split the data into input features and output variable
X = df[['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
        'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
        'mean_hum_last10', 'n_measurements']]
y = df['anomaliesAgg3']
# Inicializamos una lista vacía para almacenar las cadenas de los distintos indices de anomalia
cadenas = []

# Utilizamos un bucle for para generar las cadenas y agregarlas a la lista
for i in range(1, 40):
    cadena = f"Anomaly_{i}Agg"
    cadenas.append(cadena)

# Convertimos la lista en un array
indices = cadenas
# Set the seed for reproducibility
import sys

archivo_salida = open('resultados_pruebas_indices.txt', 'w')
sys.stdout = archivo_salida
i=4
y = df[indices[i]]
seed = 42
np.random.seed(seed)

# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=seed)

# Scale the input features
scaler = StandardScaler()
scalerY = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Scale the target variable
y_train_scaled = scalerY.fit_transform(y_train.to_numpy().reshape(-1, 1)).ravel()



    # Neural Network Model
model = Sequential()
model.add(Dense(64, activation='relu', input_dim=X_train.shape[1]))
model.add(Dense(32, activation='relu'))
model.add(Dense(1))
model.compile(loss='mean_squared_error', optimizer=keras.optimizers.SGD())
model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
y_pred_scaled_nn = model.predict(X_test)
y_pred_nn = scalerY.inverse_transform(y_pred_scaled_nn).flatten()

    # Calculate RMSE and CVRMSE for the test set
rmse_rf = np.sqrt(mean_squared_error(y_test, y_pred_nn))
cvrmse_rf = np.mean(np.abs((y_test - y_pred_nn) / y_test)) * 100
rmse_nn = np.sqrt(mean_squared_error(y_test, y_pred_nn))
cvrmse_nn = np.mean(np.abs((y_test - y_pred_nn) / y_test)) * 100

    # Plot the predictions and real values of the test set




    #print("Mean RMSE:", np.mean(rmse_nn))
    #print("Mean CVRMSE:", np.mean(cvrmse_nn))
    #print("Standard Deviation RMSE:", np.std(rmse_nn))
    #print("Standard Deviation CVRMSE:", np.std(cvrmse_nn))

    import matplotlib.pyplot as plt
    import numpy as np
    from keras.models import Sequential
    from keras.layers import Dense

    # Set the seed for reproducibility
    seed = 42
    np.random.seed(seed)

    # Initialize lists to store error values
    rmse_values = []
    cvrmse_values = []
    y_pred_list = []

    # Perform 10 runs
    for z in range(10):
        # Split the data into training and test sets
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=seed)
        # Scale the input features
        scaler = StandardScaler()
        scalerY = StandardScaler()
        X_train = scaler.fit_transform(X_train)
        X_test = scaler.transform(X_test)
        # Scale the target variable
        y_train_scaled = scalerY.fit_transform(y_train.to_numpy().reshape(-1, 1)).ravel()
        # Neural Network Model
        model = Sequential()
        model.add(Dense(64, activation='relu', input_dim=X_train.shape[1]))
        model.add(Dense(32, activation='relu'))
        model.add(Dense(1))
        model.compile(loss='mean_squared_error', optimizer=keras.optimizers.SGD(), metrics=['accuracy'])
        model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
        # Predict the output variable for the test set
        # Generar nombre de archivo único usando el contador
        nombre_archivo = f'modelo_{z}_{i}.h5'

        # Guardar el modelo con el nombre de archivo generado
        model.save(nombre_archivo)
        y_pred_scaled = model.predict(X_test)
        y_pred = scalerY.inverse_transform(y_pred_scaled).flatten()
        y_pred_list.append(y_pred)
        # Calculate RMSE and CVRMSE for the test set
        rmse = np.sqrt(mean_squared_error(y_test, y_pred))
        cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
        # Append error values to the lists
        rmse_values.append(rmse)
        cvrmse_values.append(cvrmse)

    # Find the index of the best and worst scenarios
    best_index = np.argmin(rmse_values)
    worst_index = np.argmax(rmse_values)
    print("########################################")
    print(f'PREDICCIÓN ÍNDICE ANOMALÍA NÚMERO {i+1}.\n Considera anomalía de NDVI si el valor está en los percentiles {99-i} y {1+i} de todas las mediciones tomadas')
    print("########################################\n\n")
    print("Mean RMSE:", np.mean(rmse_values))
    print("Mean CVRMSE:", np.mean(cvrmse_values))
    print("Standard Deviation RMSE:", np.std(rmse_values))
    print("Standard Deviation CVRMSE:", np.std(cvrmse_values))

    fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(12, 6))

    axes[0].plot(y_test.to_numpy(), label='Real')
    axes[0].plot(y_pred_list[best_index], label='Predicted (Best)')
    axes[0].set_title('Best Scenario')
    axes[0].legend()

    axes[1].plot(y_test.to_numpy(), label='Real')
    axes[1].plot(y_pred_list[worst_index], label='Predicted (Worst)')
    axes[1].set_title('Worst Scenario')
    axes[1].legend()

    plt.tight_layout()
    # Guardar el gráfico como archivo .svg
    nombre_archivo = f'model_nn_worst_best_scenario{i}.svg'
    plt.savefig(nombre_archivo, format='svg')
    #plt.show()

    plt.plot(y_test.to_numpy(), label='Real')
    plt.plot(y_pred_list[best_index], label='Predicted')
    plt.legend(fontsize='large')
    # Agregar nombres a los ejes
    plt.xlabel('Observation Number', fontsize='large')
    plt.ylabel('NDVI Anomaly Index', fontsize='large')
    # Cambiar las etiquetas del eje X
    # etiquetas_x = fechas
    # plt.xticks(range(len(etiquetas_x)), etiquetas_x)
    plt.yticks(fontsize='x-large')
    plt.xticks(fontsize='x-large')
    # Generamos el nombre del archivo con un número distintivo
    nombre_archivo = f'./modelo_redes_best_{i}.svg'
    # Guardar el gráfico como archivo .svg
    plt.savefig(nombre_archivo, format='svg')
    #plt.show()
    # Print the mean and standard deviation of errors
    print("Best RMSE:", np.mean(rmse_values[best_index]))
    print("Best CVRMSE:", np.mean(cvrmse_values[best_index]))
    print("worst RMSE:", np.mean(rmse_values[worst_index]))
    print("Worst CVRMSE:", np.mean(cvrmse_values[worst_index]))
########################################################################
#OPTIMIZACION
########################################################################
    print("######################################################")
    print("OPTIMIZACIÓN")
    print("######################################################\n\n")
    # Generar nombre de archivo único usando el índice del mejor modelo
    nombre_archivo = f'modelo_{best_index}_{i}.h5'

    # Cargar el modelo con el nombre de archivo generado
    modelo_cargado = load_model(nombre_archivo)
    import tensorflow as tf

    # convertir el modelo a tf lite
    converter = tf.lite.TFLiteConverter.from_keras_model(modelo_cargado)
    tflite_model = converter.convert()
    # aplicar la cuantizacion
    converter.optimizations = [tf.lite.Optimize.DEFAULT]
    tflite_quantized_model = converter.convert()
    # guardar el modelo cuantizado
    with open(f'model_{i}_quantized.tflite', 'wb') as f:
        f.write(tflite_quantized_model)
    # Cargamos el modelo sin cuantizar
    modelo_sin_cuantizar = load_model(f'modelo_{best_index}_{i}.h5')
    # Aplanar los datos de prueba si son imágenes (ejemplo)

    # Evaluar el modelo sin cuantizar en datos de prueba
    resultado_sin_cuantizar = modelo_sin_cuantizar.evaluate(X_test, y_test)
    # Cargar y evaluar el modelo cuantizado:
    # Cargar el modelo cuantizado
    with open(f'model_{i}_quantized.tflite', 'rb') as f:
        tflite_quantized_model = f.read()

    # Crear un intérprete TensorFlow Lite
    interpreter = tf.lite.Interpreter(model_content=tflite_quantized_model)
    interpreter.allocate_tensors()

    # Obtener los índices de entrada y salida del modelo
    entrada_indice = interpreter.get_input_details()[0]['index']
    salida_indice = interpreter.get_output_details()[0]['index']

    # Realizar inferencia en datos de prueba
    resultado_cuantizado = []
    for x in X_test:
        # Preparar los datos de entrada
        entrada = np.expand_dims(x, axis=0).astype(np.float32)
        interpreter.set_tensor(entrada_indice, entrada)

        # Realizar la inferencia
        interpreter.invoke()

        # Obtener los resultados
        salida = interpreter.get_tensor(salida_indice)

        resultado_cuantizado.append(salida)

    resultado_cuantizado = np.concatenate(resultado_cuantizado)

    # Comparar el rendimiento y la precisión de los modelos:
    print("Rendimiento del modelo sin cuantizar")
    print("RMSE:", np.mean(rmse_values[best_index]))
    print("CVRMSE:", np.mean(cvrmse_values[best_index]))
    print("Rendimiento y precisión del modelo cuantizado:")

    # Calculate RMSE and CVRMSE for the test set
    resultado_cuantizado = scalerY.inverse_transform(resultado_cuantizado).flatten()
    # Cálculo del RMSE
    rmse = np.sqrt(np.mean((y_test - resultado_cuantizado) ** 2))
    cvrmse = np.mean(rmse / np.mean(y_test)) * 100
    print("RMSE:", rmse)
    print("CVRMSE:", cvrmse)

    # comparar el tamaño
    import os

    tamano_sin_cuantizar = os.path.getsize(f'modelo_{best_index}_{i}.h5')
    tamano_cuantizado = os.path.getsize(f'model_{i}_quantized.tflite')

    print("Tamaño del modelo sin cuantizar:", tamano_sin_cuantizar, "bytes")
    print("Tamaño del modelo cuantizado:", tamano_cuantizado, "bytes")

    plt.plot(y_test.to_numpy(), label='Real')
    plt.plot(y_pred_list[worst_index], label='Predicted')
    plt.legend(fontsize='large')
    # Agregar nombres a los ejes
    plt.xlabel('Observation Number', fontsize='large')
    plt.ylabel('NDVI Anomaly Index', fontsize='large')
    # Cambiar las etiquetas del eje X
    # etiquetas_x = fechas
    # plt.xticks(range(len(etiquetas_x)), etiquetas_x)
    plt.yticks(fontsize='x-large')
    plt.xticks(fontsize='x-large')
    # Guardar el gráfico como archivo .svg
    #plt.savefig('./modelo_cuantizado.svg', format='svg')
    #plt.show()
archivo_salida.close()
