if (!require("quantmod")) install.packages("quantmod")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tibble")) install.packages("tibble")
if (!require("stats")) install.packages("stats")

library(quantmod)
library(ggplot2)
library(tibble)
library(stats) 

amzn_data <- getSymbols("AMZN", src = "yahoo", 
                        from = "2014-01-01", 
                        to = "2025-02-01", 
                        auto.assign = FALSE)


price_data <- Cl(amzn_data)

train_data <- window(price_data, end = "2024-12-31")

test_data <- window(price_data, start = "2025-01-01", end = "2025-01-31")

predictions <- numeric(length(test_data))

history <- train_data

cat("Processing rolling predictions for", length(test_data), "trading days in Jan 2025...\n")

for (i in 1:length(test_data)) {
  
  fit <- arima(history, order = c(1, 1, 1))
  
  fc <- predict(fit, n.ahead = 1)
  
  predictions[i] <- fc$pred[1]
  
  history <- c(history, test_data[i])
}

results_tbl <- tibble(
  Date = index(test_data),
  Actual = round(as.numeric(test_data), 2),  # Round to 2 decimal places
  Predicted = round(predictions, 2)          # Round to 2 decimal places
)

print(results_tbl)

ggplot(results_tbl, aes(x = Date)) +
  
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2) +
  
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", size = 1) +
  geom_point(aes(y = Predicted, color = "Predicted"), size = 2, shape = 1) +
  
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  
  scale_x_date(
    date_breaks = "1 day", 
    date_labels = "%d",
    limits = c(as.Date("2025-01-01"), as.Date("2025-01-31")),
    expand = expansion(mult = 0.05) 
  ) +
  
  scale_y_continuous(
    n.breaks = 10,  
    labels = scales::dollar_format()
  ) +
  
  labs(
    title = "AMAZON Stock Prediction: January 2025",
    x = "Day of January",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    panel.grid.minor.x = element_blank() 
  )

