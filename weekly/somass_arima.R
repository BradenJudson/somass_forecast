
# Pre-work ---------------------------------------------------------------------


library(tidyverse); library(tsibble); library(forecast)
library(lubridate); library(zoo); library(gginnards)
library(tseries); library(imputeTS); library(scales)
library(broom)


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

spot <- read.csv("Spot and Mean Daily Water Temp Data At Depth 21.09.21.csv") %>% 
  filter(Location == "Somass River [PaperMill 1990s-2000s]") %>% 
  dplyr::select(c(5,7, 11)) %>%                       # Remove unnecessary columns.
  mutate(date = dmy(Date),                            # Reformat date column.
         doy  = yday(date),                           # Day of the year.
         week = week(ymd(date))) %>%                  # Week - because data are weekly (roughly).
  dplyr::select(-Date)                                # Remove original dates.

max(spot$date)

win <- seq(as.Date("2017-01-01"),         # Isolate study window in variable.
           max(spot$date),                # Almost 9 full years
           by = "days")                   # of daily info.

somass <- spot[spot$date %in% win,]       # Subset to above window.

alldates <- as.data.frame(win) %>%       
  `colnames<-`(., c("date")) %>%                      # Rename column,
  mutate(week = week(ymd(date)),                      # Reformat dates.
         Year = year(date)) %>% 
  merge(., somass, 
        by    = c("date", "week", "Year"),            # Re-add spot check temp data.
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
          frequency = 52)         # Weekly averages with annual seasonality.
ns <- decompose(STS); plot(ns)    # View decomposition of time series data.
plot(ns$seasonal)                 # Clearly strong seasonal component.


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
         doy  = yday(date))                       # Add day of the year.

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
dat <- merge(impDF, air.impall, 
             by = c("date", "year")) %>% 
  select(c(1:3, "rAir"))

# visualize relationship - both highly seasonal.
ggplot(data = dat %>% 
         pivot_longer(cols = c("wSom", "rAir")),
       aes(x = date, y = value, colour = name)) +
  geom_line(size = 4/5, alpha = 3/4) + mytheme +
  labs(x = NULL, y = "Temperature (°C)")


# Save time series plot.
ggsave("plots/temps_TS.png", units = "px",
       width = 2000, height = 1200)


# Linear fit between Somass temp and air temp.
(fit <- lm(data = dat, wSom ~ rAir)); summary(fit)

# Looks like air temperature might be lagged behind water temperature a bit.
# To test this, I'll see if lagging the temperature improves anything.

# Make a list for the for-loop ouputs.
aics <- list()

# Testing 1-10 week lag effects.
for (i in 1:10) {
  dat2 <- dat %>% 
    mutate(rAirL = dplyr::lag(rAir, i))
  
  fitL <- lm(data = dat2, wSom ~ rAirL)
  aics[i] <- AIC(fitL)
}


# Store loop outputs here.
# Have to add lag = 0 using rbind.
(j <- as.data.frame(do.call(rbind, aics)) %>% 
  `colnames<-`(., c("AIC")) %>% 
  rownames_to_column(var   =  "lag") %>% 
  rbind(., data.frame(lag  =  0,
                      AIC  =  AIC(fit))) %>% 
  mutate(lag = as.numeric(lag)) %>% 
  arrange(lag))

# Lag = 1w is best fit.
plot(j)


# Add lagged air temperature data.
# And impute one missing point (keep Nobs consistent).
dat <- dat %>% 
  mutate(rAirL1 = dplyr::lag(rAir, 1)) %>% 
  tidyr::fill(rAirL1, .direction = 'up')


# Cubic looks appropriate for most years.
# Some better than others (great = 2015, less great = 2019).
tempR + geom_smooth(colour = "black") + 
  facet_wrap(~year, scales = "free") +
  labs(x = "Air temperature (°C)",
       y = "Somass temperature (°C)") +
  theme(legend.position = "none")

ggsave("plots/temp_dists.png", units = "px",
       width = 2000, height = 1500)

# Assessing various model fits.
#First, non-lagged models. 
summary(fit) # Non-lagged model identified above.
(fit2 <- lm(data = dat, wSom ~ poly(rAir, 2, raw = TRUE))); summary(fit2)
(fit3 <- lm(data = dat, wSom ~ poly(rAir, 3, raw = TRUE))); summary(fit3)


# Lagged models here.
(fitL <- lm(data  = dat, wSom ~ rAirL1)); summary(fitL)
(fitL2 <- lm(data = dat, wSom ~ poly(rAirL1, 2, raw = TRUE))); summary(fitL2)
(fitL3 <- lm(data = dat, wSom ~ poly(rAirL1, 3, raw = TRUE))); summary(fitL3)


# Extract model fit statistics and summarise here. 
(fits <- list(fit, fit2, fit3, fitL, fitL2, fitL3) %>% 
  lapply(., function(x) glance(x) %>% 
           mutate(call = as.character(x$call)[2])) %>% 
  do.call(rbind, .) %>% 
  relocate(call, 1) %>% 
  mutate(deltaAIC = round(AIC - min(AIC), 1)))

# Write to directory.
write.csv(fits, "lm_fits.csv", row.names = F)


# Cubic model is the best fit for both lagged and un-lagged air temperature data.
anova(fit, fit2, fit3)  
anova(fitL, fitL2, fitL3)

# Everything above indicates that cubic polynomial with one week lagged air temp is the better model. 

stamp <- read.csv("stamp_tempTS.csv")

# Yearly timeseries for lagged and non-lagged temperatures.
ggplot(data = dat[dat$year != "2021", ] %>% 
         mutate("Air temperature (C)"        = scale(rAir),
                "Lagged air temperature (C)" = scale(rAirL1),
                "Somass temperature (C)"     = scale(wSom)) %>% 
         pivot_longer(cols = c(6:8)),
       aes(x = date, y = value, colour = name)) +
  geom_line(alpha = 3/4, size = 3/4) +
  mytheme + labs(x = "", y = "Scaled variable") +
  facet_wrap(~year, scales = "free") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b")

ggsave("plots/temp_lag_relationships.png", units = "px",
       width = 2250, height = 2000)


# Seasonality ------------------------------------------------------------------


# Set up forecast horizon, here h = 6 weeks.
fh <- 6


# Set up list to store output values.
bestfit <- list(aicc = Inf) 


for(i in 1:25) {                        # For fourier terms 1 - 50.
  fit <- auto.arima(STS,                # Conduct automatic ARIMA models.
                    xreg = fourier(STS, K = i), # Use Fourier on timeseries data with varying number of terms.
                    seasonal = FALSE)   # Fourier to encompass seasonality so exclude from base model.
  
  if(fit$aicc < bestfit$aicc)        # If AIC is lower than subsequent model.
    bestfit <- fit                   # Choose that model.
  else break;                        # Otherwise, exist loop (saves a lot of memory).
}


(bf <- ncol(bestfit$xreg)/2)         # Optimal number of Fourier terms.
bestfit$arma; bestfit                # Optimal model and error distribution.
summary(bestfit)                     # Prints favourable summary stats.


harmonics <- fourier(STS,            # Fit optimal Fourier model to observed data.
                     K = bf)         # Using lowest AIC selection.
nrow(harmonics) == length(STS)       # Both 513 rows.


h1 <- Arima(y = as.numeric(STS),     # Initial time series of temperatures (w/ imputations).
            order = c(1,0,0),        # Fit ARIMA model to temperature data.
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
plot(f2, xlim = c(230, 260))         # Plot - just forecasted region


# Above fit seems OK but I think adding an air temperature term would help.


# Isolate air variables.
airvars <- dat[,c("date", "rAirL1")] %>% 
  mutate(airL2 = rAirL1^2,
         airL3 = rAirL1^3) %>% 
  rename("airL1" = "rAirL1") 


# New ARIMA w/ air temp as a covariate.
h2 <- Arima(y = as.numeric(STS),
            order = c(1,0,0),
            seasonal = FALSE,
            xreg = as.matrix(cbind(harmonics, 
                   airvars[,c(2:4)])))


# Check diagnostics.
h2 %>% checkresiduals()
summary(h2)


# See how harmonics coincide with air temperature data.
exvar = as.data.frame(cbind(harmonics, airvars)) %>% 
  merge(., impDF, by = "date") %>% 
  mutate(f2 = `S1-52` + `C1-52`,
         fp2 = as.numeric(scale(1-f2)),
         airsc = as.numeric(scale(airL1)),
         Somass = as.numeric(scale(wSom))) %>% 
  rename("Fourier" = "fp2",
         "Lagged (1-w) air temperature" = "airsc") %>% 
  pivot_longer(cols = c("Fourier", "Lagged (1-w) air temperature", "Somass")) 


(total <- ggplot() + 
  geom_line(data = exvar,
            aes(x = date, 
                y = value, 
                colour = name),
            size = 1, alpha = 4/5) + 
  mytheme + 
    theme(legend.position = "top") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  ylab("Scaled values") + xlab(NULL))


(yr.fac <- ggplot() + 
    geom_line(data = exvar[exvar$year != 2021,],
              aes(x = date, 
                  y = value, 
                  colour = name),
              size = 1, alpha = 4/5) + 
    mytheme + theme(legend.position = "none") +
    ylab("Scaled values") + xlab(NULL) + 
    facet_wrap(~year, scales = "free", ncol = 2) +
    scale_x_date(date_breaks = "2 month",
                 date_labels = "%b"))


cowplot::plot_grid(total, yr.fac, ncol = 1,
                   rel_heights = c(0.4, 1),
                   align = "V")


ggsave("plots/fourierK2.png", units = "px",
       width = 2500, height = 3000)


# Project air temperature out one months using current year only.
# NOTE THAT THIS IS LIKELY INACCURATE AS IT DOES NOT ACCOUNT FOR
# ANTICIPATED WEATHER CHANGES (e.g., PRECIPITATION or COOLING).
airTS <- ts(airvars[airvars$date > "2020-01-01", "airL1"], 
            frequency = 52)

# Forecast air temperatures out one month.
forecast(airTS, h = fh) %>% plot()

# Assign projected air temperature data to a DF.
# Below, I increase the projected air temperature variables by 1/4 such that they 
# match the observed temperature. This is just for exploration purposes. Also the 
# forecast of air temperature is highly imprecise and the trend appears to decline 
# more steeply than what would be expected in recent years.
# Use independent meteorological 14-day forecasts in future. 

prjAir <- as.data.frame(forecast(airTS, h = fh)) %>% 
  rename("airL1" = "Point Forecast") %>% 
  mutate(airL1 = airL1,
         airL2 = airL1^2,
         airL3 = airL1^3) %>% 
  dplyr::select(starts_with("air"))


# For experimentation only.
# prjAir[1:16,1] <- 30
# prjAir[1:16,2] <- 30^2
# prjAir[1:16,3] <- 30^3


# Bind projected air temperatures to Fourier terms.
h2.newvars <- as.data.frame(newharmonics) %>% 
  cbind(., prjAir)


# Forecast air temp + seasonality model using forecasted data.
h2f <- forecast(h2, xreg = as.matrix(h2.newvars)); head(h2f)
plot(h2f) # Trajectory and prediction intervals seem reasonable.
plot(h2f, xlim = c(230, 255))
accuracy(h2f)

# Coerce above data into a dataframe for ggplot.
df2 <- data.frame(meanT = as.numeric(h2f$mean),
                  lwr80 = as.numeric(h2f$lower[,1]),
                  lwr95 = as.numeric(h2f$lower[,2]),
                  upr80 = as.numeric(h2f$upper[,1]),
                  upr95 = as.numeric(h2f$upper[,2]),
                  date = rep(max(impDF$date), fh) + seq(0, 7*(fh-1), 7),
                  type = "Forecasted") %>% 
  rbind(., impDF[,c(1:2)] %>% 
          mutate(upr80 = NA, lwr80 = NA, 
                 upr95 = NA, lwr95 = NA,
                 type = "Observed") %>% 
          rename("meanT" = "wSom")) 

# Plot full time series. 
(full2 <- ggplot(data = df2,
                 aes(x = date,
                     y = meanT)) +
    geom_hline(yintercept = c(18,19,20),
               colour = "red2",
               linetype = "dashed",
               alpha = c(1/8, 2/5, 1)) +
    geom_ribbon(aes(ymin = lwr95, ymax = upr95,
                    fill = type),
                alpha = 1/5) +
    geom_ribbon(aes(ymin = lwr80, ymax = upr80,
                    fill = type),
                alpha = 2/5) + 
    geom_line(linewidth = 1, aes(colour = type)) + 
    mytheme +
    labs(x = "", y = "Somass River Temperature (°C)") +
    theme(plot.margin = margin(5, 10, 0.1, 5, "pt")) +
    guides(colour = "none"))

# Plot zoomed-in time series.
zi2 <- full2 +
    geom_point(aes(colour = type), size = 2.5,
               alpha = 1/2) +
    scale_x_date(limits = c(as.Date("2021-6-01"), 
                            as.Date("2021-10-20")),
                 date_breaks = "1 month",
                 date_labels = "%b") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(2, 25, 4),
                       limits = c(4, 23))
gginnards::move_layers(zi2, "GeomPoint", position = "top")

# Combine full and zoomed in time series plots.
(comb2 <- cowplot::plot_grid(full2, zi2, ncol = 1,
                             rel_heights = c(1, 0.8),
                             align = "V"))

ggsave("plots/ARIMA_wTemp.png", units = "px",
       width = 2500, height = 2000)


fmDF <- data.frame(modelled = h2$fitted,
                   observed = h2$x,
                   air = h2$xreg[,3],
                   date = dat$date,
                   year = dat$year) %>% 
  pivot_longer(cols = c(modelled, observed, air))


ggplot(fmDF, 
       aes(x = date,
           y = value,
           colour = name)) +
  geom_line(size = 1, alpha = 3/4) + mytheme +
  labs(y = "Somass temperature (°C)", x = NULL) +
  scale_y_continuous(breaks = seq(3, 50, 4)) +
  facet_wrap(~year, scales = "free", nrow = 2) +
  scale_x_date(date_labels = "%b")

ggsave("plots/fit_obs.png", units = "px",
       width = 3000, height = 1700)


