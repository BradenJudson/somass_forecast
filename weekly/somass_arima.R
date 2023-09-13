

# Pre-work ---------------------------------------------------------------------


library(tidyverse); library(tsibble); library(forecast)
library(lubridate); library(zoo); library(gginnards)
library(tseries); library(imputeTS); library(scales)
library(broom); library(svMisc); library(tidyhydat)


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
           max(spot$date),                # Almost 5 full years.
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
ggplot_na_imputations(weekly$SomT, weekimp) +
  ylab("Temperature (ºC)") + ggtitle(NULL,subtitle = NULL) +
  mytheme + xlab("Week")

ggsave("plots/imputed_somass.png", units = "px",
       widt = 2000, height = 1200)

# Assign to new object, specify if data are imputed or observed.
impDF <- data.frame(wSom = as.numeric(weekimp),
                    date = ymd(weekly$date),
                    year = year(ymd(weekly$date))) %>% 
  filter(date %in% win) %>% 
  mutate(type = case_when(
    date %in% somass$date ~ "Observed",
    date %ni% somass$date ~ "Imputed"
  ))

# write.csv(impDF, "somass_weekly.csv", row.names = F)

STS <- ts(as.numeric(impDF$wSom), # Set Somass temperatures as a time series object.
          frequency = 365.25/7)         # Weekly averages with annual seasonality.

# test <- as_tsibble(STS)
ns <- decompose(STS); plot(ns)    # View decomposition of time series data.
plot(ns$seasonal)                 # Clearly strong seasonal component.

(ffreq <- forecast::findfrequency(STS))
all.equal(ffreq, 52)

# Seasonality ------------------------------------------------------------------

# Set up forecast horizon, here h = 4 weeks.
fh <- 4

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
(order <- bestfit$arma[1:3])
bestfit %>% checkresiduals()

harmonics <- fourier(STS,            # Fit optimal Fourier model to observed data.
                     K = bf)         # Using lowest AIC selection.
nrow(harmonics) == length(STS)       # Both 249 rows.

# Air temperature data ---------------------------------------------------------

# Read in daily air temperature data.
airtemp <- read.csv("alberni_temps.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-rTemp)

fullair <- as.data.frame(win) %>%    # air temperature data available.
  `colnames<-`(., c("date")) %>%                  # Rename column.
  merge(., airtemp, by = "date",                  # Merge with original air temp data.
        all.x   = TRUE) %>%                         # Include rows with missing temp data.
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
                      x_axis_labels = fullair$date) +
  ylab("Temperature (ºC)") + ggtitle(NULL,subtitle = NULL) +
  mytheme + xlab(NULL)

ggsave("plots/air_temp_imp.png", units = "px",
       width = 2000, height = 1200)

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
  labs(x = NULL, y = "Temperature (°C)") +
  facet_wrap(~year, scales = "free") +
  scale_x_date(date_labels = "%b")


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
ggplot(data = dat %>% 
         mutate(year = as.factor(year)),
       aes(x = rAirL1,
           y = wSom,
           colour = year)) +
  geom_point() + mytheme +
  geom_smooth(colour = "black") + 
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

# Isolate air variables.
airvars <- dat[,c("date", "rAirL1")] %>% 
  mutate(airL2 = rAirL1^2,
         airL3 = rAirL1^3) %>% 
  rename("airL1" = "rAirL1") 

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



# Flow effects -----------------------------------------------------------------

# Ash river flows - m3/s.
ash <- hy_daily_flows(station_number = "08HB023",
                      start_date = min(win)) %>%
  mutate(year = as.factor(year(Date)),
         rAsh = rollapply(Value, 14, mean,
                             partial = TRUE)) %>%
  dplyr::select(c("Date", "rAsh", "year")) %>%
  `colnames<-`(., tolower(c(colnames(.)))) 

# Isolate Ash flows. 
ashFlow <- ash[ash$date %in% impDF$date, c("date", "rash")]


# Sproat river flows - m3/s.
sproat <- hy_daily_flows(station_number = "08HB008",
                         start_date = min(win)) %>%
  mutate(year = as.factor(year(Date)),
         rSproat = rollapply(Value, 14, mean,
                             partial = TRUE)) %>%
  dplyr::select(c("Date", "rSproat", "year")) %>%
  `colnames<-`(., tolower(c(colnames(.)))) 

# Isolate Sproat flows. 
sproatFlow <- sproat[sproat$date %in% impDF$date, c("date", "rsproat")]
 
# Make sure all dates are accounted for. 
nrow(ashFlow) == nrow(sproatFlow) & nrow(impDF)

flows <- merge(ashFlow, sproatFlow, by = "date") %>% 
  pivot_longer(cols = c("rash", "rsproat")) %>% 
  mutate(name = tools::toTitleCase(sub('.', '', name)))

(flowTS <- ggplot(data = flows, aes(x = date, y = value, colour = name)) +
    geom_line(size = 1) + mytheme +
    labs(x = NULL, y = "Discharge (cms)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "none"))

(sumTS <- ggplot(data = flows %>% 
                  mutate(year = year(date),
                         month = month(date)) %>% 
                  filter(month %in% 5:9),
                aes(x = date, y = value, colour = name)) +
    geom_line(size = 1) +
    facet_wrap(~year, scales = "free_x") +
    mytheme +
    labs(x = NULL, y = "Discharge (cms)") +
    theme(legend.position = c(0.9,0.2),
          legend.justification = c(1,0)))

cowplot::plot_grid(flowTS, sumTS, ncol = 1)
ggsave("plots/flows.png", units = "px",
       width = 2000, height = 2500)


# TS Model selection -----------------------------------------------------------

# Define a funcion to test ARIMA with various covariate terms.
arm.xreg <- function(xreg) {
  # y = Somass temperatures.
  Arima(y = as.numeric(STS),
        # Optimal error structure defined earlier.
        order = order,
        seasonal = FALSE,
        # Covariates = Fourier terms and whatever else is defined.
        xreg = as.matrix(cbind(harmonics, xreg)))
}

# Set up a list so the computation is vectorized. 
# All covariates and select combinations of covariates. 
xregs <- list(air = airvars[,2:4], sproat = sproatFlow[,2],
              ash = ashFlow[,2],   harmonics = NULL,
              bothFlow = cbind(sproatFlow[,2], ashFlow[,2]),
              airFlow = cbind(airvars[,2:4], ashFlow[,2], sproatFlow[,2]))

# Apply to all list elements simultaneously. 
(xreg_mods <- lapply(xregs, arm.xreg))

# Extract list values and reorganize into a data.frame.
(mod_vals  <- as.data.frame(do.call(rbind, lapply(xreg_mods, accuracy))) %>% 
    mutate(AIC  = c(do.call(rbind, lapply(xreg_mods, AIC))),
           xreg = names(xregs),
           deltaAIC = round(AIC - min(AIC), 2)) %>% 
    relocate("xreg", "AIC", "deltaAIC") %>% 
    # Present lowest relative AIC first.
    arrange(deltaAIC)) 

# Isolate the model with lowest relative AIC.
(optMod <- xreg_mods[[mod_vals[mod_vals$deltaAIC == 0, 1]]])


arimaDF <- data.frame(obs  = impDF$wSom,
                      date = impDF$date,
                      h1   = fitted(optMod, h = 1),
                      h2   = fitted(optMod, h = 2),
                      h3   = fitted(optMod, h = 3),
                      h4   = fitted(optMod, h = 4)) %>% 
  mutate(month = month(date),
         year = year(date),
         MAE  = abs(obs - h2)) %>% 
  filter(month %in% c(4:9)) 

armLF <- arimaDF %>% 
  pivot_longer(cols = starts_with("h"), names_to = "h") %>% 
  mutate(MAE = abs(obs - value))


ggplot(data = armLF, 
       aes(x = date, y = obs)) +
  mytheme +
  geom_line(aes(x = date, y = value, colour = h),
            size = 2/3, alpha = 1, linetype=  2) +
  geom_point(size = 2) +
  facet_wrap(~year, scales = "free_x") +
  labs(x = NULL, y = "Somass temperature (C)") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")

ggplot(data = arimaDF) +
  geom_point(aes(x = date, y = obs), color = "red") +
  geom_line(aes(x = date, y = h2),   color = "blue", alpha = 1/3) +
  geom_point(aes(x = date, y = h2),  color = "blue", alpha = 1/3) +
  facet_wrap(~year, scales=  "free_x") + mytheme +
  labs(x = NULL, y = "Somass temperature (C)") +
  ggtitle("Blue = modelled (h = 2), Red = Observed") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")


#Performance -------------------------------------------------------------------

# Define forecasting function for cross-validation.
fc <- function(y, h, xreg, newxreg) {
  
  # Parameters from original ARIMA models.
  # No seasonality, use Fourier terms from earlier instead.
  fit <- Arima(y, order =  order,
               seasonal =  FALSE,
               xreg     =  xreg)
  
  # Input values above. 
  # Splitting into train and test data is automatic.
  forecast(fit, xreg = newxreg, h = h) 
  
}

# Perform cross-validation. 
# Weekly Somass temperatures as time series, Fourier terms as covariates. 
# Looking ahead h = 1:6 weeks.
forcv <- tsCV(as.numeric(STS), fc,
          h = fh, xreg = as.matrix(cbind(harmonics, 
                                  airvars[,c(2:4)])))
head(forcv,  15) # Check formatting.

# For formatting subsequent base plots.
par(mfrow = c(3,1), mar = c(5,5,2,2))

# Calculate RMSE for h = 1:6 and plot.
(cv_rmse <- apply(forcv, 2, FUN = function(x) sqrt(mean(x^2, na.rm = TRUE)))) 
plot(cv_rmse, xlab = "Forecast horizon (weeks)", ylab = "RMSE", type = "b")

# Calculate absolute error for  h = 1:6 and plot.
(cv_ae <- apply(forcv, 2, FUN = function(x) mean(abs(x), na.rm = TRUE)))
plot(cv_ae, xlab = "Forecast horizon (weeks)", ylab = "MAE", type = "b")


### NEEDS FIXING BELOW ### cvae


# Calculate mean absolute percent error for  h = 1:6 and plot.
(cv_ae <- apply(forcv, 2, FUN = function(x) 100*mean(abs(x), na.rm = TRUE)))/as.numeric(h2f$mean)
plot(cv_ae, xlab = "Forecast horizon (weeks)", ylab = "MAPE", type = "b")


# Prediction intervals at thresholds -------------------------------------------


nsim <- 1000L # 10k simulations. 
fh            # = 4 weeks. Defined earlier.

# Empty matrix to populate w/ for-loop.
future <- matrix(NA, nrow = fh, ncol = nsim) 

# For each of 1000 simulations, do the following:
for(i in seq(nsim)) {

  # Simulate the best-performing model. 
  future[,i] <- simulate(Arima(y = as.numeric(STS),
                               order = order,
                               seasonal = FALSE,
                               xreg = harmonics),
                         # 4 weeks of forecasted harmonics (~ newxreg).
                         xreg   = fourier(STS,
                                          K = bf,
                                          h = fh), 
                         # Goal is future, bootstrapped values.
                         future = TRUE,
                         bootstrap = TRUE)
  
  # Print progress bar for each iteration. 
  progress(i, nsim) 
}

# Isolate simulated data in dataframe. 
(future_sims <- as.data.frame(future) %>% 
  # h = forecast horizon (in weeks).
  mutate(h = as.factor(paste(1:fh, 
                             "weeks"))) %>% 
  # Pivot to long-form for easier grouping. 
  pivot_longer(cols = -c(h),
               names_to  = "sim", 
               values_to = "temp") %>% 
  mutate(d = as.Date(max(impDF$date) + 7*as.numeric(substr(x = h, 1, 1)))))
  
(simhist <- ggplot(data = future_sims, aes(x = temp)) +
  geom_histogram(fill  = "gray95",
                 color = "gray30",
                 bins  = 50) +
    facet_wrap(. ~ h, scales = "free_x") +
    mytheme +
    labs(x = "Temperature (C)", y = NULL))

ggsave("plots/simulated_tempdists.png", units = "px",
       width = 2000, height = 1500)

(sim_summ <- future_sims %>% 
  group_by(h) %>% 
  # For each forecasted week, get mean temperature,
  # and % of days (from simulation) above 18, 19, 20C. 
  summarise(mean = mean(temp, na.rm = TRUE),
            p18  = round(sum(temp > 18, na.rm = TRUE)/nsim * 100, 1),
            p19  = round(sum(temp > 19, na.rm = TRUE)/nsim * 100, 1),
            p20  = round(sum(temp > 20, na.rm = TRUE)/nsim * 100, 1)) %>% 
  mutate(date = max(impDF$date) + seq(7, 7*fh, 7)))

(forecast_probs <- ggplot(data = impDF,
       aes(x = date,
           y = wSom)) +
  geom_hline(yintercept = c(18,19,20),
                          colour = "red2",
                          linetype = "dashed",
                          alpha = c(1/8, 2/5, 1)) +
  geom_line(size = 1, colour = "black") +
  geom_point(size = 2, shape = 21,
             colour = "white",
             fill = "black",
             stroke = 2) + mytheme +
  labs(x = NULL, y = "Somass temperature (C)") +
  coord_cartesian(xlim = c(max(impDF$date) - 100,
                           max(impDF$date) + 8*fh)) +
  geom_boxplot(data = future_sims,
               aes(x  = d, y = temp,
                 group = h, width  = 8),
              fill = "gray95",
              alpha = 1/2,outlier.alpha = 0) +
  scale_y_continuous(limits = c(min(future_sims$temp),
                                max(impDF$wSom)+1),
                     breaks = seq(1, 50, 3)) +
  geom_text(data = sim_summ,
            aes(x = date-3/2, y = 21,
                label = sprintf("%0.1f", p18)),
            size = 3, hjust = 0) +
  geom_text(data = sim_summ,
            aes(x = date-3/2, y = 22,
                label = sprintf("%0.1f", p19)),
            size = 3, hjust = 0) +
  geom_text(data = sim_summ,
            aes(x = date-3/2, y = 23,
                label = sprintf("%0.1f", p20)),
            size = 3, hjust = 0) +
  annotate("text", y = c(21, 22, 23), size = 3,
           label = c("p18 (%) = ",
                     "p19 (%) = ",
                     "p20 (%) = "),
           x = max(impDF$date)-1))


ggsave("plots/forecast_wProbs.png", units = "px",
       width = 2200, height = 1200)

  
# -------------------------------------------------------------------------
