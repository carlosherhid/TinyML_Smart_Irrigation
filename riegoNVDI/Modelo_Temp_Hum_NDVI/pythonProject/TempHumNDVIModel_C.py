import pandas as pd
from sklearn.ensemble import RandomForestRegressor

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
     'mean_hum_last10','n_mediciones']]
y = df['anomaliesAgg3']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
y_testPREV = y_test
# Scale the input features
scaler = StandardScaler()
scalerY = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

y_train_scaled = scalerY.fit_transform(y_train.to_numpy().reshape(-1, 1)).ravel()
#y_test_scaled = scalerY.transform(y_test.to_numpy().reshape(-1, 1)).ravel()


# Realizamos el entrenamiento usando un modelo de regresión de random forest de skicit-learn
# Instanciamos el modelo con 1000 árboles de decisión

rf = RandomForestRegressor(n_estimators=1000, random_state=42)

# Entrenamos el modelo con los datos de training
rf.fit(X_train, y_train_scaled)


# Realizamos predicciones usando el modelo entrenado y los datos de test
y_pred = rf.predict(X_test)

# Train the linear regression model
#regressor = LinearRegression()
#regressor.fit(X_train, y_train_scaled)

# Predict the output variable for the test set
#y_pred = regressor.predict(X_test)

# Rescale the predicted and test values to their original scale
#y_pred = scalerY.inverse_transform(y_pred.reshape(-1, 1)).flatten()
# Rescale the predicted and test values to their original scale
y_pred = scalerY.inverse_transform(y_pred.reshape(-1, 1)).flatten()
# Calculate RMSE and CVRMSE for the test set
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100




# Print the results
print("RMSE:", rmse)
print("CVRMSE:", cvrmse)

# Plot the predictions and real values of the test set
import matplotlib.pyplot as plt

plt.plot(y_test.to_numpy(), label='Real')
plt.plot(y_pred, label='Predicted')
plt.legend()
plt.show()

import svgwrite
dwg = svgwrite.Drawing('test.svg', profile='tiny')
plt.plot(y_test.to_numpy(), label='Real')
plt.plot(y_pred, label='Predicted')
plt.legend()
dwg.save()



from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
import numpy as np
from keras.models import Sequential
from keras.layers import Dense

# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

# Scale the input features
scaler = StandardScaler()
scalerY = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Scale the target variable
y_train_scaled = scalerY.fit_transform(y_train.to_numpy().reshape(-1, 1)).ravel()

# Build the neural network model
model = Sequential()
model.add(Dense(64, activation='relu', input_dim=X_train.shape[1]))
model.add(Dense(32, activation='relu'))
model.add(Dense(1))

# Compile the model
model.compile(loss='mean_squared_error', optimizer='adam')

# Train the model
model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)

# Make predictions on the test set
y_pred_scaled = model.predict(X_test)

# Rescale the predicted values to their original scale
y_pred = scalerY.inverse_transform(y_pred_scaled).flatten()

# Calculate RMSE and CVRMSE for the test set
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100


# Initialize lists to store error values
rmse_values = []
cvrmse_values = []
y_pred_list = []

# Perform 10 runs
for _ in range(10):
	# Build the neural network model
	model = Sequential()
	model.add(Dense(64, activation='relu', input_dim=X_train.shape[1]))
	model.add(Dense(32, activation='relu'))
	model.add(Dense(1))
	# Compile the model
	model.compile(loss='mean_squared_error', optimizer='adam')
	# Train the model
	model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
	# Make predictions on the test set
	y_pred_scaled = model.predict(X_test)
	# Rescale the predicted values to their original scale
	y_pred = scalerY.inverse_transform(y_pred_scaled).flatten()
	# Calculate RMSE and CVRMSE for the test set
	rmse = np.sqrt(mean_squared_error(y_test, y_pred))
	cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
	y_pred_list.append(y_pred)
	rmse_values.append(rmse)
	cvrmse_values.append(cvrmse)

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
plt.show()


# UNA RED SECUENCIAL UN POQUITO MÁS COMPLEJA

# Initialize lists to store error values
rmse_values = []
cvrmse_values = []
y_pred_list = []

# Perform 10 runs
for _ in range(10):
	# Build the neural network model
	model = Sequential()
	model.add(Dense(128, activation='relu', input_dim=X_train.shape[1]))
	model.add(Dense(64, activation='relu'))
	model.add(Dense(32, activation='relu'))
	model.add(Dense(1))
	# Compile the model
	model.compile(loss='mean_squared_error', optimizer='adam')
	# Train the model
	model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
	# Make predictions on the test set
	y_pred_scaled = model.predict(X_test)
	# Rescale the predicted values to their original scale
	y_pred = scalerY.inverse_transform(y_pred_scaled).flatten()
	# Calculate RMSE and CVRMSE for the test set
	rmse = np.sqrt(mean_squared_error(y_test, y_pred))
	cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
	y_pred_list.append(y_pred)
	rmse_values.append(rmse)
	cvrmse_values.append(cvrmse)

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
plt.show()


# UNA RED NEURONAL CNN
from keras.models import Sequential
from keras.layers import Conv1D, MaxPooling1D, Flatten, Dense
# Initialize lists to store error values
rmse_values = []
cvrmse_values = []
y_pred_list = []
input_length = X_train.shape[1]
# Perform 10 runs
for _ in range(10):
	model = Sequential()
	model.add(Conv1D(32, kernel_size=3, activation='relu', input_shape=(input_length, 1)))
	model.add(MaxPooling1D(pool_size=2))
	model.add(Conv1D(64, kernel_size=3, activation='relu'))
	model.add(MaxPooling1D(pool_size=2))
	model.add(Flatten())
	model.add(Dense(128, activation='relu'))
	model.add(Dense(1))
	# Compile the model
	model.compile(loss='mean_squared_error', optimizer='adam')
	# Train the model
	model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
	# Make predictions on the test set
	y_pred_scaled = model.predict(X_test)
	# Rescale the predicted values to their original scale
	y_pred = scalerY.inverse_transform(y_pred_scaled).flatten()
	# Calculate RMSE and CVRMSE for the test set
	rmse = np.sqrt(mean_squared_error(y_test, y_pred))
	cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
	y_pred_list.append(y_pred)
	rmse_values.append(rmse)
	cvrmse_values.append(cvrmse)

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
plt.show()

# UNA RED NEURONAL CNN + LSTM

from keras.models import Sequential
from keras.layers import Conv1D, MaxPooling1D, Flatten, Dense, LSTM
# Initialize lists to store error values
rmse_values = []
cvrmse_values = []
y_pred_list = []
input_length = X_train.shape[1]
# Perform 10 runs
for _ in range(10):
	model = Sequential()
	model.add(Conv1D(32, kernel_size=3, activation='relu', input_shape=(input_length, 1)))
	model.add(MaxPooling1D(pool_size=2))
	model.add(Conv1D(64, kernel_size=3, activation='relu'))
	model.add(MaxPooling1D(pool_size=2))
	model.add(LSTM(64, activation='relu'))
	model.add(Dense(128, activation='relu'))
	model.add(Dense(1))
	# Compile the model
	model.compile(loss='mean_squared_error', optimizer='adam')
	# Train the model
	model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
	# Make predictions on the test set
	y_pred_scaled = model.predict(X_test)
	# Rescale the predicted values to their original scale
	y_pred = scalerY.inverse_transform(y_pred_scaled).flatten()
	# Calculate RMSE and CVRMSE for the test set
	rmse = np.sqrt(mean_squared_error(y_test, y_pred))
	cvrmse = np.mean(np.abs((y_test - y_pred) / y_test)) * 100
	y_pred_list.append(y_pred)
	rmse_values.append(rmse)
	cvrmse_values.append(cvrmse)

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
plt.show()

