
# Pre-work ---------------------------------------------------------------------


library(tidyverse); library(tsibble); library(forecast)
library(lubridate); library(zoo); library(gginnards)
library(tseries); library(imputeTS); library(scales)


# Set working directory.
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()


# Set seed.
set.seed(856)


# Custom theme.
mytheme <- theme_bw() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.margin = margin(c(0,0,-5,0)))


# Custom "not-in" operator.
"%ni%" <- Negate("%in%")


# Explore time series data -----------------------------------------------------

somass <- read.csv("Spot and Mean Daily Water Temp Data At Depth 21.09.21.csv") %>% 
  filter(Location == "Somass River [PaperMill 1990s-2000s]") %>% 
  dplyr::select(c(5,7, 11)) %>%                       # Remove unnecessary columns.
  mutate(date = dmy(Date),                            # Reformat date column.
         doy = yday(date),                            # Day of the year.
         week = week(ymd(date))) %>%                  # Week - because data are weekly (roughly).
  dplyr::select(-Date)                                # Remove original dates.

win <- seq(as.Date("2012-01-01"),                     # Isolate study window in variable.
           as.Date("2021-09-03"),                     # Almost 9 full years
           by = "days")                               # of daily info.

somass <- somass[somass$date %in% win,]               # Subset to above window.

alldates <- as.data.frame(seq(min(win),      # Dataframe of all dates 
                              max(win),      # in study window.
                              by = "days")) %>%       
  `colnames<-`(., c("date")) %>%                      # Rename column,
  mutate(week = week(ymd(date)),                      # Reformat dates.
         Year = year(date)) %>% 
  merge(., somass, by = c("date", "week", "Year"),    # Re-add spot check temp data.
        all.x = TRUE) %>%                             # Even missing data.
  filter(date %in% win)                               # Subset.

weekly <- alldates %>%                          
  group_by(Year, week) %>%                  # For each year and week... 
  summarise(SomT = mean(MeanWaterT,         # Calculate mean spot check temperature.
                        na.rm = TRUE), 
            date = min(date))               # And select first day in that 7 day window.
weekly$SomT[is.nan(weekly$SomT)] <- NA      # Assign missing data to NA (instead of NaN).

# Visualize NA distribution.
ggplot_na_distribution(x = weekly$SomT,
                       x_axis_labels = weekly$date)

# Save NA distribution plot to directory.
ggsave("plots/somass_spotcheck.png", units = "px", 
       width = 2500, height = 1250)

# Impute weekly data using linear interpolation.
weekimp <- na_interpolation(x = weekly$SomT,
                            option = "linear")

# Visualize imputations.
ggplot_na_imputations(weekly$SomT, weekimp)

# Assign to new object, specify if data are imputed or observed.
impDF <- data.frame(wSom = as.numeric(weekimp),
                    date = ymd(weekly$date),
                    year = year(ymd(weekly$date))) %>% 
  filter(date %in% win) %>% 
  mutate(type = case_when(
    date %in% somass$date ~ "Observed",
    date %ni% somass$date ~ "Imputed"
  ))

STS <- ts(as.numeric(impDF$wSom), # Set Somass temperatures as a time series object.
          frequency = 52)       # Weekly averages with annual seasonality.
ns <- decompose(STS); plot(ns)  # View decomposition of time series data.
plot(ns$seasonal)               # Clearly strong seasonal component.


# Air temperature data ---------------------------------------------------------

# Read in daily air temperature data.
airtemp <- read.csv("alberni_temps.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-rTemp)

fullair <- as.data.frame(seq(min(airtemp$date),   # Make an object with all dates
                             max(airtemp$date),   # between the max and min of the
                             by = "days")) %>%    # air temperature data available.
  `colnames<-`(., c("date")) %>%                  # Rename column.
  merge(., airtemp, by = "date",                  # Merge with original air temp data.
        all = TRUE) %>%                           # Include rows with missing temp data.
  mutate(year = year(date),                       # Add year as grouping factor.
         doy = yday(date))                        # Add day of the year.

# Plot distribution of NAs.
ggplot_na_distribution(fullair$MaxT,
                       x_axis_labels = fullair$date)

# Use linear interpolation to impute missing data.
airimp <- na_interpolation(x = fullair$MaxT,
                           option = "linear")

# Visualize imputed data.
ggplot_na_imputations(fullair$MaxT, airimp,
                      x_axis_labels = fullair$date)

air.impall <- data.frame(tImp = as.numeric(airimp),       # Store imputed data in DF.
                         date = fullair$date,             # Add date info.
                         year = fullair$year) %>%         # Add year.
  mutate(rAir = rollapply(tImp, 7, mean, partial = TRUE)) # Rolling average of air temp.


# Combine all spot check data and air temp data.
dat <- merge(impDF, air.impall, by = c("date", "year")) %>% 
  select(c(1:3, "rAir"))

# visualize relationship - both highly seasonal.
ggplot(data = dat %>% 
         pivot_longer(cols = c("wSom", "rAir")),
       aes(x = date, y = value, colour = name)) +
  geom_line(size = 4/5, alpha = 3/4) + mytheme +
  xlab("") + ylab("Temperature (°C)")

# Save time series plot.
ggsave("plots/temps_TS.png", units = "px",
       width = 2000, height = 1200)

# Air temperature relationship by year.
(tempR <- ggplot(data = dat %>% 
                   mutate(year = as.factor(year)), 
                 aes(x = rAir, 
                     y = wSom,
                     colour = year)) +
    geom_point() + mytheme)

# Assessing various model fits.
(fit <- lm(data = dat, wSom ~ rAir)); summary(fit)
(fit2 <- lm(data = dat, wSom ~ poly(rAir, 2, raw = TRUE))); summary(fit2)
(fit3 <- lm(data = dat, wSom ~ poly(rAir, 3, raw = TRUE))); summary(fit3)
anova(fit, fit2, fit3) # Cubic appears to be best, but not by a huge amount.

# Cubic looks appropriate for most years.
# Some better than others (great = 2015, less great = 2019).
tempR + geom_smooth(colour = "black") + 
  facet_wrap(~year, scales = "free") +
  labs(x = "Air temperature (°C)",
       y = "Somass temperature (°C)") +
  theme(legend.position = "none")

ggsave("plots/temp_dists.png", units = "px",
       width = 2000, height = 1500)

# -------------------------------------------------------------------------

# Set up forecast horizon, here h = 26 weeks (~ 6 months).
fh <- 26

# Set up list to store output values.
bestfit <- list(aicc = Inf) 

for(i in 1:50) {                        # For fourier terms 1 - 50.
  fit <- auto.arima(STS,                # Conduct automatic ARIMA models.
                    xreg = fourier(STS, K = i), # Use Fourier on timeseries data with varying number of terms.
                    seasonal = FALSE)   # Fourier to encompass seasonality so exclude from base model.
  
  if(fit$aicc < bestfit$aicc)        # If AIC is lower than subsequent model.
    bestfit <- fit                   # Choose that model.
  else break;                        # Otherwise, exist loop (saves a lot of memory).
}

(bf <- length(bestfit))              # Optimal number of Fourier terms.
bestfit$arma; bestfit                # Optimal model and error distribution (3,1,3).
summary(bestfit)                     # Prints favourable summary stats.

harmonics <- fourier(STS,            # Fit optimal Fourier model to observed data.
                     K = bf)         # Using lowest AIC selection.
nrow(harmonics) == length(STS)       # Both 513 rows.

h1 <- Arima(y = as.numeric(STS),     # Initial time series of temperatures (w/ imputations).
            order = c(1,0,1),        # Fit ARIMA model to temperature data.
            seasonal = FALSE,        # 
            xreg = harmonics)        # Let modelled Fourier account for seasonality.

png(file = "plots/residuals.png", 
    units = 'px',
    width = 1000, height = 400)      # For saving residual plot.
h1 %>% checkresiduals(); dev.off()   # Inspect risiduals. Some issues but not abysmal.

newharmonics <- fourier(STS,         # Extend fourier term into the future.
                K = bf, h = fh)      # Forecast horizon = t + 6mon.

f2 <- forecast(h1,xreg=newharmonics) # Forecast using above fit.
plot(f2)                             # Plot - entire TS.
plot(f2, xlim = c(490, 540))         # Plot - just forecasted region

# Above fit seems OK but I think adding an air temperature term would help.

# Isolate air variables.
airvars <- dat[,c(1,4)] %>% 
  mutate(air2 = rAir^2,
         air3 = rAir^3) %>% 
  rename("air1" = "rAir")

# New ARIMA w/ air temp as a covariate.
h2 <- Arima(y = as.numeric(STS),
            order = c(1,0,1),
            seasonal = FALSE,
            xreg = as.matrix(cbind(harmonics, 
                                  airvars[,c(2:4)])))

# Inspect residuals and summary.
h2 %>% checkresiduals()
summary(h2)

# Project air temperature out one months using current year only.
# NOTE THAT THIS IS LIKELY INACCURATE AS IT DOES NOT ACCOUNT FOR
# ANTICIPATED WEATHER CHANGES (e.g., PRECIPITATION or COOLING).
airTS <- ts(airvars[airvars$date > "2020-01-01", "air1"], 
            frequency = 52)

# Forecast air temperatures out one month.
forecast(airTS, h = fh) %>% plot()

# Assign projected air temperature data to a DF.
prjAir <- as.data.frame(forecast(airTS, h = fh)) %>% 
  rename("air1" = "Point Forecast") %>% 
  mutate(air2 = air1^2, air3 = air1^3) %>% 
  dplyr::select(starts_with("air"))

# Bind projected air temperatures to Fourier terms.
h2.newvars <- as.data.frame(newharmonics) %>% 
  cbind(., prjAir)

# Forecast air temp + seasonality model using forecasted data.
h2f <- forecast(h2, xreg = as.matrix(h2.newvars)); head(h2f)
plot(h2f) # Trajectory and prediction intervals seem reasonable.
plot(h2f, xlim = c(480, 540))

# Coerce above data into a dataframe for ggplot.
df2 <- data.frame(meanT = as.numeric(h2f$mean),
                  lwr = as.numeric(h2f$lower[,1]),
                  upr = as.numeric(h2f$upper[,1]),
                  date = rep(max(impDF$date), 26) + seq(0, 7*25, 7),
                  type = "Forecasted") %>% 
  rbind(., impDF[,c(1:2)] %>% 
          mutate(upr = NA, lwr = NA, type = "Observed") %>% 
          rename("meanT" = "wSom")) 

# Plot full time series. 
(full2 <- ggplot(data = df2,
                 aes(x = date,
                     y = meanT)) +
    geom_hline(yintercept = c(18,19,20),
               colour = "red2",
               linetype = "dashed",
               alpha = c(1/8, 2/5, 1)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    fill = type),
                alpha = 2/10) +
    geom_line(linewidth = 1, aes(colour = type)) + 
    mytheme +
    labs(x = "", y = "Somass River Temperature (°C)") +
    theme(plot.margin = margin(5, 10, 0.1, 5, "pt")))

# Plot zoomed-in time series.
zi2 <- full2 +
    geom_point(aes(colour = type), size = 2.5,
               alpha = 1/2) +
    scale_x_date(limits = c(as.Date("2021-06-01"), 
                            as.Date("2021-11-31")),
                 date_breaks = "1 month",
                 date_labels = "%b") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(2, 25, 2),
                       limits = c(6.5, 23))
gginnards::move_layers(zi2, "GeomPoint", position = "top")

# Combine full and zoomed in time series plots.
(comb2 <- cowplot::plot_grid(full2, zi2, ncol = 1,
                             rel_heights = c(1, 0.8),
                             align = "V"))

ggsave("plots/ARIMA_wTemp.png", units = "px",
       width = 2500, height = 1500)
