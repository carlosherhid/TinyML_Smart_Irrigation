import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
import sklearn.metrics as sm
import numpy as np


# Leemos los datos de temperatura, anomalías, NDVI y humedad de las 5 estaciones meteorológicas
datos_CA42  = pd.read_csv('anomaliesCA42.csv', sep=';')
datos_CA42 = datos_CA42.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
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


datos_CA91  = pd.read_csv('anomaliesCA91.csv',sep=';')
datos_CA91 = datos_CA91.dropna(subset=['anomaliesAgg'],) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MO12  = pd.read_csv('anomaliesMO12.csv',sep=';')
datos_MO12 = datos_MO12.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MU21  = pd.read_csv('anomaliesMU21.csv',sep=';')
datos_MU21 = datos_MU21.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg
datos_MU62  = pd.read_csv('anomaliesMU62.csv',sep=';')
datos_MU62 = datos_MU62.dropna(subset=['anomaliesAgg']) # Eliminamos las filas que contienen valores NaN en anomaliesAgg



d1 = datos_MU21[['tmed', 'hrmed', 'anomaliesAgg', 'anomaliesAgg2', 'n_mediciones']]
d2 = datos_MU62[['tmed', 'hrmed', 'anomaliesAgg', 'anomaliesAgg2', 'n_mediciones']]
d3 = datos_CA91[['tmed', 'hrmed', 'anomaliesAgg', 'anomaliesAgg2', 'n_mediciones']]
d4 = datos_CA42[['tmed', 'hrmed', 'anomaliesAgg', 'anomaliesAgg2', 'n_mediciones']]
d5 = datos_MO12[['tmed', 'hrmed', 'anomaliesAgg', 'anomaliesAgg2', 'n_mediciones']]

df = pd.concat([d1, d2, d3, d4, d5])

df['n_mediciones'].value_counts()
df = df.loc[df['n_mediciones'] > 70]


from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
import numpy as np
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LinearRegression

# Split the data into input features and output variable
X = df[['tmed', 'hrmed', 'n_mediciones']]
y = df['anomaliesAgg']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
y_testPREV = y_test
# Scale the input features
scaler = StandardScaler()
scalerY = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

y_train_scaled = scalerY.fit_transform(y_train.to_numpy().reshape(-1, 1)).ravel()
#y_test_scaled = scalerY.transform(y_test.to_numpy().reshape(-1, 1)).ravel()


# Train the linear regression model
regressor = LinearRegression()
regressor.fit(X_train, y_train_scaled)

# Predict the output variable for the test set
y_pred = regressor.predict(X_test)

# Rescale the predicted and test values to their original scale
y_pred = scalerY.inverse_transform(y_pred.reshape(-1, 1)).flatten()
#y_test = scalerY.inverse_transform(y_test.values.reshape(-1, 1)).flatten()

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

