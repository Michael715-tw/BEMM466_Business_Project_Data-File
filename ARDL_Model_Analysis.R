#ARDL model with all data have log transformed

library(dplyr)
library(dynlm)
library(urca)
library(lmtest)
library(tseries)
library(ARDL)
library(zoo)
library(strucchange)

# Reading CSV files from GitHub
sp500_data <- read.csv("https://raw.githubusercontent.com/Michael715-tw/BEMM466_Business_Project_Data-File/main/S%26P%20500%20from%2001012013%20to%2030042024.csv")
interest_rate_data <- read.csv("https://raw.githubusercontent.com/Michael715-tw/BEMM466_Business_Project_Data-File/main/interest%20rate%20from%2001012013%20to%2030042024.csv")
cpi_data <- read.csv("https://raw.githubusercontent.com/Michael715-tw/BEMM466_Business_Project_Data-File/main/CPI%20DATA%20from%2001012013%20to%2030042024.csv")
unemployment_data <- read.csv("https://raw.githubusercontent.com/Michael715-tw/BEMM466_Business_Project_Data-File/main/unemployment%20rate%20from%2001012013%20to%2030042024.csv")

# Output the first 5 rows of data
head(sp500_data)
head(interest_rate_data)
head(cpi_data)
head(unemployment_data)

# Convert date formats
sp500_data <- sp500_data %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
interest_rate_data <- interest_rate_data %>% mutate(Date = as.Date(DATE, format = "%Y/%m/%d"))
cpi_data <- cpi_data %>% mutate(Date = as.Date(DATE, format = "%Y-%m-%d"))
unemployment_data <- unemployment_data %>% mutate(Date = as.Date(DATE, format = "%Y-%m-%d"))

# Log transform S&P 500 closing price
sp500_data <- sp500_data %>%
  mutate(Log_Close = log(Close))

# Calculate inflation rate (using log change of CPI, year-over-year percentage change)
cpi_data <- cpi_data %>%
  mutate(Inflation = c(rep(NA, 12), diff(log(CPIAUCSL), lag = 12) * 100))

# Filter data from January 2014 onwards
sp500_data <- sp500_data %>% filter(Date >= as.Date("2014-01-01"))
interest_rate_data <- interest_rate_data %>% filter(Date >= as.Date("2014-01-01"))
cpi_data <- cpi_data %>% filter(Date >= as.Date("2014-01-01"))
unemployment_data <- unemployment_data %>% filter(Date >= as.Date("2014-01-01"))

# Log transform interest rate and unemployment rate
interest_rate_data <- interest_rate_data %>%
  mutate(Log_InterestRate = log(FEDFUNDS))

unemployment_data <- unemployment_data %>%
  mutate(Log_Unemployment = log(UNRATE))

# Merge data into one dataframe
merged_data <- sp500_data %>%
  full_join(interest_rate_data, by = "Date") %>%
  full_join(cpi_data, by = "Date") %>%
  full_join(unemployment_data, by = "Date") %>%
  dplyr::select(Date, Close, Log_Close, FEDFUNDS, Log_InterestRate, UNRATE, Log_Unemployment, CPIAUCSL, Inflation) %>%
  na.omit()

# View the first few rows of merged data
head(merged_data)

# Rename columns
colnames(merged_data) <- c("Date", "SP500_Close", "Log_SP500_Close", "InterestRate", "Log_InterestRate", "UnemploymentRate", "Log_UnemploymentRate", "CPI", "InflationRate")

# Convert data into time series
log_sp500_ts <- ts(merged_data$Log_SP500_Close, frequency = 12)
log_interest_rate_ts <- ts(merged_data$Log_InterestRate, frequency = 12)
log_unemployment_rate_ts <- ts(merged_data$Log_UnemploymentRate, frequency = 12)
inflation_rate_ts <- ts(merged_data$InflationRate, frequency = 12)

# Set the graphical layout to 2x2 to display 4 plots on one canvas
par(mfrow = c(2, 2))

# Plot log of S&P 500
ts.plot(log_sp500_ts, 
        col = "black", 
        lty = 1, 
        lwd = 2, 
        main = "Log S&P 500 Close",
        ylab = "Log SP500")

# Plot log of interest rate
ts.plot(log_interest_rate_ts, 
        col = "blue", 
        lty = 1, 
        lwd = 2, 
        main = "Log Interest Rate",
        ylab = "Log Interest Rate")

# Plot log of unemployment rate
ts.plot(log_unemployment_rate_ts, 
        col = "red", 
        lty = 1, 
        lwd = 2, 
        main = "Log Unemployment Rate",
        ylab = "Log Unemployment Rate")

# Plot inflation rate
ts.plot(inflation_rate_ts, 
        col = "green", 
        lty = 1, 
        lwd = 2, 
        main = "Inflation Rate",
        ylab = "Inflation Rate")

# Reset graphical layout
par(mfrow = c(1, 1))

# Perform ADF test on S&P 500
adf_sp500 <- ur.df(log_sp500_ts, type = "trend", selectlags = "AIC")
summary(adf_sp500)

# Perform ADF test on interest rate
adf_interest_rate <- ur.df(log_interest_rate_ts, type = "trend", selectlags = "AIC")
summary(adf_interest_rate)

# Perform ADF test on unemployment rate
adf_unemployment_rate <- ur.df(log_unemployment_rate_ts, type = "trend", selectlags = "AIC")
summary(adf_unemployment_rate)

# Perform ADF test on inflation rate
adf_inflation_rate <- ur.df(inflation_rate_ts, type = "trend", selectlags = "AIC")
summary(adf_inflation_rate)

# First difference of S&P 500
diff_log_sp500_ts <- diff(log_sp500_ts)
adf_diff_sp500 <- ur.df(diff_log_sp500_ts, type = "trend", selectlags = "AIC")
summary(adf_diff_sp500)

# First difference of interest rate
diff_log_interest_rate_ts <- diff(log_interest_rate_ts)
adf_diff_interest_rate <- ur.df(diff_log_interest_rate_ts, type = "trend", selectlags = "AIC")
summary(adf_diff_interest_rate)

# First difference of unemployment rate
diff_unemployment_rate_ts <- diff(log_unemployment_rate_ts)
adf_diff_unemployment_rate <- ur.df(diff_unemployment_rate_ts, type = "trend", selectlags = "AIC")
summary(adf_diff_unemployment_rate)

# First difference of inflation rate
diff_inflation_rate_ts <- diff(inflation_rate_ts)
adf_diff_inflation_rate <- ur.df(diff_inflation_rate_ts, type = "trend", selectlags = "AIC")
summary(adf_diff_inflation_rate)

# Define breakpoint location
breakpoint <- which(merged_data$Date == "2020-03-01")

# Build full model
full_model <- lm(Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate, data = merged_data)

# Model before breakpoint
model_before <- lm(Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate, 
                   data = merged_data[merged_data$Date <= as.Date("2020-03-01"), ])

# Model after breakpoint
model_after <- lm(Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate, 
                  data = merged_data[merged_data$Date > as.Date("2020-03-01"), ])

# Perform Chow Test
chow_test <- sctest(Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate, 
                    type = "Chow", point = breakpoint, data = merged_data)

# Display chow test result
print(chow_test)

# Define breakpoint date
break_date <- as.Date("2020-03-01")

# Add dummy variable: 1 if date is on or after breakpoint date, 0 otherwise
merged_data$dummy_break <- ifelse(merged_data$Date >= break_date, 1, 0)

dummy_break_ts <- ts(merged_data$dummy_break, frequency = 12)

# Ensure date format is correct
merged_data$Date <- as.Date(merged_data$Date, format = "%Y-%m-%d")

# Create zoo time series
merged_data_ts <- zoo(merged_data[, -1], order.by = merged_data$Date)

# check results
print(head(merged_data_ts))

# Calculate the optimal lag order based on AIC value
best_ardl_model <- auto_ardl(
  Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate + dummy_break,
  data = merged_data_ts,
  max_order = 6,  
  selection = "AIC"
)

# The top 20 models according to the AIC
best_ardl_model$top_orders

# Build ARDL model with dummy variable
ardl_model <- ardl(Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate + dummy_break, 
                   data = merged_data_ts, 
                   order = c(2,2,2,1,1))
                
summary(ardl_model)

# Perform bounds test at 10% significance level
bounds_test <- bounds_f_test(ardl_model, ,alpha = 0.10, case = 3)
print(bounds_test)

# Perform bounds test at 5% significance level
bounds_test <- bounds_f_test(ardl_model, ,alpha = 0.05, case = 3)
print(bounds_test)

# Perform bounds test at 1% significance level
bounds_test <- bounds_f_test(ardl_model, ,alpha = 0.01, case = 3)
print(bounds_test)

# Estimate long-run coefficients
long_run_estimates <- multipliers(ardl_model)
print(long_run_estimates)

# Build ARDL model with dummy variable for ECM
ardl_model_for_ecm <- ardl(Log_SP500_Close ~ Log_InterestRate + Log_UnemploymentRate + InflationRate + dummy_break, 
                   data = merged_data, 
                   order = c(2,2,2,1,1))
# Build ECM
ecm_model <- recm(ardl_model_for_ecm, case = 3)
summary(ecm_model)

# Breusch-Godfrey test for autocorrelation
bg_test <- bgtest(ecm_model, order = 1) 
print(bg_test)

# Jarque-Bera test for normality
jb_test <- jarque.bera.test(residuals(ecm_model))
print(jb_test)

# Breusch-Pagan test for heteroscedasticity
bp_test <- bptest(ecm_model)
print(bp_test)

# Calculate residuals of the model
ecm_residuals <- residuals(ecm_model)     

# Create a data frame
residuals_df <- data.frame(residuals = ecm_residuals)

# Perform CUSUM stability test
cusum_test <- efp(residuals ~ 1, data = residuals_df, type = "Rec-CUSUM")

# Generate year labels
year_labels <- format(merged_data$Date, "%Y")

# Plot CUSUM chart with year labels
year_labels <- format(merged_data$Date, "%Y")
plot(cusum_test, xaxt = "n")  
abline(h = 0, col = "blue")
axis(1, at = seq(0, 1, length.out = length(year_labels)), labels = year_labels)

