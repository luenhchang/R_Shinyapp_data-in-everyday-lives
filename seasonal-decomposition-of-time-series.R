library(forecast)

# Example data (replace with your own)
dates <- seq.Date(from = as.Date("2024-12-01"), by = "day", length.out = 30)
expenditure <- c(50, 20, 30, 60, 10, 25, 15, 40, 30, 50, 20, 10, 45, 35, 20, 30, 60, 10, 25, 15, 40, 30, 50, 20, 10, 45, 35, 20, 30, 60) # length(expenditure) 30

# Create time-series objects
## Set the time series with a frequency of 7 for daily intervals:
## Set the time series with a frequency of 1 for weekly intervals:
ts_data <- ts(expenditure, frequency = 7)  # Weekly seasonality

# Apply decomposition
decomposed <- decompose(ts_data)

# Plot the decomposition
plot(decomposed)

# The trend plot shows the smoothed underlying direction or movement in the data by removing short-term fluctuations and seasonality.
# It highlights long-term changes while minimizing the effect of random noise.
# The observed plot combines all components: trend, seasonality, and random noise.

# Seasonality Component in Decomposition:
#The seasonal component captures the regular, cyclical patterns that occur at fixed intervals within each year (or any other specified period) in the time series.
#The seasonality plot shows how the data fluctuates within each cycle (e.g., month, quarter, week) based on the average value of the time series for that specific period.

#What Does the Seasonality Plot Show?
# It reveals recurrent patterns or cycles in the data. For instance, if you are analyzing monthly sales data, the seasonal component might show higher sales around the holiday season each year.
# The plot visualizes the average seasonal effect at each period within a cycle. For example, if the data is monthly, the seasonal component will show the expected value for each month (e.g., January, February, etc.), which might repeat each year.
# It isolates the cyclic behavior of the data, removing the trend and noise.

# What to look for in trend, seasonality and random components?
# 1. Trend Component
# The trend represents the long-term direction of the data, excluding seasonal and random variations.
# What to Look For:
# Directionality: Check if the trend is increasing, decreasing, or stable over time.
# Major Shifts: Look for abrupt changes or turning points that might indicate significant events or structural changes in the data.
# Long-Term Patterns: Identify if the trend aligns with expected real-world phenomena (e.g., gradual increase in grocery spending due to inflation or lifestyle changes).

# Interpretation Example:
# A steadily increasing trend might suggest rising spending habits.
# A fluctuating or erratic trend could indicate inconsistencies or irregularities in the data.

# 2. Seasonal Component
# The seasonal component captures repeating patterns at regular intervals (e.g., weekly or yearly).
# What to Look For:
# Cyclic Behavior: Look for predictable peaks and troughs (e.g., higher spending every weekend or during holidays).
# Magnitude of Seasonality: Check if the amplitude of seasonal variations (distance between peaks and troughs) is significant or subtle.
# Timing of Peaks: Verify that the seasonal patterns occur at expected intervals (e.g., weekly or yearly).

# Interpretation Example:
# A clear seasonal pattern could indicate higher spending during weekends or holiday seasons.
# A lack of seasonality might suggest that spending is not strongly influenced by time-related factors.

# 3. Random (Residual) Component
# The random component (also called noise or remainder) represents what’s left after removing the trend and seasonality. Ideally, this should contain only random fluctuations.

# What to Look For:
# Randomness: Check if the residuals appear random with no discernible pattern.
# Outliers: Look for spikes or dips that deviate significantly from the baseline, which might indicate data anomalies or one-off events.
# Constant Variance: Ensure that the spread of the residuals is relatively uniform over time.

# Interpretation Example:
# Large or structured patterns in the residuals might suggest that important information is missing or the model doesn’t fully capture the data structure.
# Truly random residuals indicate that the decomposition has effectively separated trend and seasonality.
