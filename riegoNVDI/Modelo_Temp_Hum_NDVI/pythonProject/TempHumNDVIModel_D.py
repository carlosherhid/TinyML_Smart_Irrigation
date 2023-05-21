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
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
import numpy as np
from keras.models import Sequential
from keras.layers import Dense

# Split the data into input features and output variable
X = df[['tmed', 'hrmed', 'temp1', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6', 'temp7', 'temp8', 'temp9', 'temp10',
     'hum1', 'hum2', 'hum3', 'hum4', 'hum5', 'hum6', 'hum7', 'hum8', 'hum9', 'hum10', 'mean_temp_last10',
     'mean_hum_last10','n_mediciones']]
y = df['anomaliesAgg3']


# Set the seed for reproducibility
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

# Random Forest Model
rf = RandomForestRegressor(n_estimators=1000, random_state=seed)
rf.fit(X_train, y_train_scaled)
y_pred_rf = rf.predict(X_test)

# Neural Network Model
model = Sequential()
model.add(Dense(64, activation='relu', input_dim=X_train.shape[1]))
model.add(Dense(32, activation='relu'))
model.add(Dense(1))
model.compile(loss='mean_squared_error', optimizer='adam')
model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
y_pred_scaled_nn = model.predict(X_test)
y_pred_nn = scalerY.inverse_transform(y_pred_scaled_nn).flatten()

# Calculate RMSE and CVRMSE for the test set
rmse_rf = np.sqrt(mean_squared_error(y_test, y_pred_rf))
cvrmse_rf = np.mean(np.abs((y_test - y_pred_rf) / y_test)) * 100
rmse_nn = np.sqrt(mean_squared_error(y_test, y_pred_nn))
cvrmse_nn = np.mean(np.abs((y_test - y_pred_nn) / y_test)) * 100


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


print("Mean RMSE:", np.mean(rmse_values))
print("Mean CVRMSE:", np.mean(cvrmse_values))
print("Standard Deviation RMSE:", np.std(rmse_values))
print("Standard Deviation CVRMSE:", np.std(cvrmse_values))


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
for _ in range(10):
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
    model.compile(loss='mean_squared_error', optimizer='adam')
    model.fit(X_train, y_train_scaled, epochs=100, batch_size=32, verbose=0)
    # Predict the output variable for the test set
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

# Print the mean and standard deviation of errors
print("Mean RMSE:", np.mean(rmse_values))
print("Mean CVRMSE:", np.mean(cvrmse_values))
print("Standard Deviation RMSE:", np.std(rmse_values))
print("Standard Deviation CVRMSE:", np.std(cvrmse_values))



