# 
# Purpose: Lowpass function to remove noise from sensor data
# Author: Jake Diamond
# Date: July 1, 2020
# 

# Cutoff frequency (1/sample rate) is that frequency where the magnitude 
# response of the filter is sqrt(1/2)
lowpass_fun <- function(data, 
                        cutoff_frequency = 0.12) {
  require(signal)
  require(imputeTS)
  # Re-interpolate all NAs so that there are none with Stineman method
  data$value_an_int <- imputeTS::na_interpolation(data$value, option = "stine")
  # Order the data, just in case
  data <- data[with(data, order(time)),]
  # Sampling rate [s^-1]
  sr <- 1 / (as.numeric(difftime(data$time[2], data$time[1], units = "secs")))
  # Nyquist frequency = half the sampling rate
  nyq <- sr / 2
  # Cutoff frequency (s^-1)
  cutoff <- cutoff_frequency * sr
  # Normalized cutoff frequency for Butterworth filter
  W <- cutoff / nyq
  # Butterworth low-pass filter, digital, 2nd order
  myfilter <- signal::butter(2, W, type = 'low', plane = 'z')
  # Forward-reverse filter to remove phase-shift 
  # associated with Butterworth filter (must be in vector-form)
  vec <- as.vector(data$value_an_int)
  filtered <- signal::filtfilt(myfilter, vec)
  # Filtered data
  data$filtered <- filtered
  data <- data[with(data, order(time)), ]
  rem <- round(sr / cutoff, 0)
  data <- data[-c(1:rem, (nrow(data) - rem):nrow(data)),]
}