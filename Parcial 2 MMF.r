library(tseries)

datos <- read.csv("C:\\Users\\naran\\Downloads\\NU (20250401000000000 _ 20240401000000000).csv", header = TRUE, sep = ",")
data <- datos$Close
adf_test <- adf.test(data)
print(adf_test)
# p = 0.5108 > 0.05, entonces se diferencia

data_diff <- diff(data, differences = 1)
adf_test <- adf.test(data_diff)
print(adf_test)
# p = 0.01 < 0.05, entonces es estacionario, luego d = 1


# Graficar la serie original y la serie diferenciada
plot(datos$Close, type = "l", col = "blue", xlab = "Tiempo", ylab = "Precio de cierre", main = "Precio de cierre de la acción")
plot(data_diff, type = "l", col = "red", xlab = "Tiempo", ylab = "Precio de cierre diferenciado", main = "Precio de cierre de la acción diferenciado")




# Calcular ACF

auto_corr <- function(Y, k) {
  n <- length(Y)
  gamma_k <- sum((Y[1:(n-k)] - mean(Y)) * (Y[(k+1):n] - mean(Y))) / n
  gamma_0 <- sum((Y - mean(Y))^2) / n
  return(gamma_k / gamma_0)
}

# Ejemplo para k desde 0 hasta 20
k_values <- 0:20
acf_values <- sapply(k_values, function(k) auto_corr(data_diff, k))
acf_values <- data.frame(k = k_values, acf = acf_values)
plot(acf_values, type = "h", xlab = "Lag (k)", ylab = "ACF", main = "ACF de los datos diferenciados", col = "blue")
abline(h = 0, col = "black")
abline(h = c(1.96 / sqrt(length(data_diff)), -1.96 / sqrt(length(data_diff))), col = "red", lty = 2)

# Como hay 2 picos, se puede suponer que q =  2

# Calcular PACF
partial_autocor <- function(Y, max_lag) {
  pacf_values <- numeric(max_lag)  
  for (k in 1:max_lag) {
    lagged_matrix <- embed(Y, k + 1)
    response <- lagged_matrix[, 1] 
    predictors <- lagged_matrix[, -1]
    model <- lm(response ~ predictors)
    pacf_values[k] <- coef(model)[k + 1]
  }
  return(pacf_values)
}


# Ejemplo para k desde 0 hasta 20
k <- 20

pacf_vals <- partial_autocor(data_diff, k)

# Graficar los valores de PACF
plot(1:k, pacf_vals, type = "h", xlab = "Lag", ylab = "PACF", main = "PACF de los datos diferenciados", col = "blue")
abline(h = 0, col = "black")
abline(h = c(1.96 / sqrt(length(data_diff)), -1.96 / sqrt(length(data_diff))), col = "red", lty = 2)

# Como hay 1 pico, se puede suponer que p = 1

model <- arima(data, order = c(2, 1, 1), include.mean = TRUE)
# Graficar los residuos del modelo
residuals <- residuals(model)
plot(residuals, type = "l", col = "blue", xlab = "Tiempo", ylab = "Resiuduos", main = "Residuos del modelo ARIMA(2,1,1)")
abline(h = 0, col = "black")
fitted_values <- data - residuals(model)

# Graficar la serie original y la serie ajustada
plot(data, type = "l", lty = 2 , col = "blue", xlab = "Tiempo", ylab = "Precio de cierre", main = "Estimado vs real")
lines(fitted_values, type = "l", col = "red", xlab = "", ylab = "")
legend("topright", legend = c("Original", "Modelo"), col = c("blue", "red"), lty = 1)

RMSE <- sqrt(mean((data - fitted_values)^2))
print(paste("RMSE:", RMSE))
MAE <- mean(abs(data - fitted_values))
print(paste("MAE:", MAE))
