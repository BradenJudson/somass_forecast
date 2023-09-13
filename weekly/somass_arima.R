

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


# Fix Somass TS ----------------------------------------------------------------
# 
 sproatT <- read.csv("sproat_temps.csv") %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year > 2016 & year %ni% c("2023", "2022")) %>% 
  filter(month %in% 5:9) %>% 
  merge(., impDF, all.x = TRUE, by = c("date")) %>% 
  mutate(date = as.Date(date),
         rSproat = rollapply(sproat, 14, mean, partial= T),
         sproatL = dplyr::lead(rSproat, 4))  %>% 
  tidyr::fill(sproatL, .direction = 'up')

ggplot(data = sproatT) +
  geom_line(aes(x = date, y = sproatL, group = 1)) +
  geom_point(aes(x = date, y = wSom), color = "red") +
  geom_line(aes(x = date, y = rSproat, group = 1), 
            colour = "blue") +
  geom_line(data = sproatT %>% 
              filter(!is.na(wSom)),
            aes(x = date, y = wSom, group = 1),
            color = "red", size = 1/3) +
  facet_wrap(~year.x, scales = "free") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  mytheme + labs(x = NULL, y = "Temperature (C)") +
  scale_y_continuous(breaks = seq(0, 30, 3))

aiclist <- list()

for (i in 1:10) {
  dat2 <- sproatT %>% 
    mutate(lagSproat = dplyr::lead(rSproat, i)) %>% 
    tidyr::fill(lagSproat, .direction = "up")
  
  fitL <- lm(data = dat2, wSom ~ lagSproat)
  aiclist[i] <- AIC(fitL)
}

ggplot(data = sproatT,
       aes(x = rSproat, y = wSom)) +
  geom_point()

# stampT  <- read.csv("stamp_temps.csv")
# 
# all_temps <- merge(sproatT, stampT, by = "date") %>% 
#   mutate(month = month(date))
# 
# (stampFlow <- ggplot(data = stamp %>% 
#          mutate(date = as.Date(date),
#                 month = month(date),
#                 year  = year(date)) %>% 
#          filter(year %in% c(2017:2021)) %>% 
#          filter(month %in% c(5:8)), 
#        aes(x = date, 
#            y = rStampSensor,
#            group = 1)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~year, scales = "free", nrow = 1) + 
#   scale_x_date(date_breaks = "1 month",
#                date_labels = "%b %d") +
#   labs(x = NULL, y = "Stamp Flow") + mytheme)
# 
# 
# (somassT <- ggplot(data = somass %>% 
#                      mutate(month = month(date),
#                             year  = year(date)) %>% 
#                      filter(month %in% 5:8), 
#                   aes(x = date,
#                       y = MeanWaterT,
#                       group = 1)) +
#     geom_line() +
#   geom_point() +
#     facet_wrap(~year, scales = "free", nrow = 1) +
#     labs(x = NULL, y = "Somass temp") + mytheme)
# 
# 
# (airplot <- ggplot(data= airtemp %>% 
#                      mutate(date = as.Date(date),
#                             year = year(date),
#                             month = month(date),
#                             rAir = rollapply(mAirT, 10, mean, partial = TRUE)) %>% 
#                      filter(year %in% c(2017:2021)) %>% 
#                      filter(month %in% c(5:8)), 
#                   aes(x = date,
#                       y = rAir)) +
#     geom_line() +
#     geom_point() +
#     facet_wrap(~year, scales = "free", nrow = 1) +
#     mytheme)
# 
# 
# (stampTemp <- ggplot(stampT %>% 
#                        mutate(date = as.Date(date),
#                               month = month(date),
#                               rTemp = rollapply(stamp, 7, mean, partial = TRUE)) %>% 
#                        filter(year %in% c(2017:2021)) %>% 
#                        filter(month %in% c(5:8)), 
#                      aes(x = date, y = rTemp)) +
#     geom_point() + 
#     facet_wrap(~year, scales = "free", nrow = 1) +
#     mytheme +
#     labs(x = NULL, y= "Stamp temp") +
#     geom_line() +
#     scale_x_date(date_breaks = "1 month",
#                  date_labels = "%b %d"))
# 
# 
# 
# cowplot::plot_grid(somassT, stampFlow, airplot, stampTemp, ncol = 1)
# 
# 



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

# # Sproat river flows - m3/s.
sproat <- hy_daily_flows(station_number = "08HB008",
                         start_date = min(win)) %>%
  mutate(year = as.factor(year(Date)),
         rSproat = rollapply(Value, 14, mean,
                             partial = TRUE)) %>%
  dplyr::select(c("Date", "rSproat", "year")) %>%
  `colnames<-`(., tolower(c(colnames(.)))) 
 
# All dates are accounted for. 
nrow(sproat[sproat$date %in% impDF$date,]) == length(impDF$wSom)

(sproatTS <- ggplot(data = sproat, aes(x = date, y = rsproat)) +
  geom_line(size = 1) + mytheme +
  labs(x = NULL, y = "Sproat discharge (cms)") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y"))

(sproatWr <- ggplot(data = sproat %>% 
                      mutate(year = year(date),
                             month = month(date)) %>% 
                      filter(month %in% 5:9),
                    aes(x = date, y = rsproat)) +
    geom_point(alpha = 1/3, size = 1) + facet_wrap(~year, scales = "free_x") +
    mytheme + labs(x = NULL, y = "Sproat discharge (cms)"))

SomSpr <- merge(impDF, sproat, by = c("date", "year"), 
                all.x = TRUE) %>% 
  mutate(Lflow1 = dplyr::lag(rsproat, 1),
         Leadflow = dplyr::lead(rsproat, 4),
         month = month(date)) %>% 
  tidyr::fill(c(Lflow1, Leadflow), .direction = "up")

cowplot::plot_grid(sproatTS, sproatWr, ncol = 1)
ggsave("plots/sproat_flows.png", units = "px",
       width = 2000, height = 2500)

ggplot(data = SomSpr %>% 
         mutate(month = month(date)) %>% 
         filter(month %in% 6:7),
       aes(x = wSom, y = Leadflow)) +
  geom_smooth(method = "lm") +
  geom_point() + mytheme +
  labs(x = "Somass temperature (C)",
       y = "Stamp discharge (cms)") +
  scale_x_continuous(breaks = seq(0, 25, 2)) +
  scale_y_continuous(breaks = seq(0, 5, 1))

(SprFit1 <- lm(data = SomSpr, wSom ~ rsproat)); summary(SprFit1)
(SprFitL <- lm(data = SomSpr, wSom ~ Lflow1)); summary(SprFitL)
(SprFitL2 <- lm(data = SomSpr, wSom ~ Lflow2)); summary(SprFitL2)
(SprFit2 <- lm(data = SomSpr, wSom ~ I(log(rsproat)))); summary(SprFit2)
anova(SprFit1, SprFitL, SprFitL2, SprFit2)

flow <- SomSpr %>% 
  mutate(log.Sproat = log(rsproat)) %>% 
  select(c("date", "Leadflow"))

# Model selection --------------------------------------------------------------


arm.xreg <- function(xreg) {
  Arima(y = as.numeric(STS),
        order = order,
        seasonal = FALSE,
        xreg = as.matrix(cbind(harmonics, xreg)))
}

(airReg <- arm.xreg(xreg = airvars[,c(2:4)]))
(sprReg <- arm.xreg(xreg = flow[,2]))
(botReg <- arm.xreg(xreg = cbind(airvars[,2:4], flow[,2])))
(defReg <- arm.xreg(xreg = NULL))

(modTests <- rbind(as.data.frame(accuracy(airReg)) %>% 
                    mutate(AIC = AIC(airReg), 
                           xreg = "Air"),
                  as.data.frame(accuracy(sprReg)) %>% 
                    mutate(AIC = AIC(sprReg),
                           xreg = "Sproat"),
                  as.data.frame(accuracy(botReg)) %>% 
                    mutate(AIC = AIC(botReg),
                           xreg = "Air and Flow"),
                  as.data.frame(accuracy(defReg)) %>% 
                    mutate(AIC = AIC(defReg),
                           xreg = "Harmonics")) %>% 
  relocate("xreg") %>% 
  mutate(deltaAIC = round(AIC - min(AIC), 3))) 


defReg %>% checkresiduals()
sprReg %>% checkresiduals()
botReg %>% checkresiduals()
airReg %>% checkresiduals()

arimaDF <- data.frame(obs  = as.numeric(STS),
                      date = impDF$date,
                      h2   = fitted(sprReg, h = 2L)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(month %in% c(4:7))

ggplot(data = arimaDF) +
  geom_point(aes(x = date, y = obs), color = "red") +
  geom_line(aes(x = date, y = h2), color = "blue",  alpha = 1/3) +
  geom_point(aes(x = date, y = h2), color = "blue", alpha = 1/3) +
  facet_wrap(~year, scales=  "free") + mytheme +
  labs(x = NULL, y = "Somass temperature (C)") +
  ggtitle("Blue = modeled, Red = Observed")



# Air temperature ---------------------------------------------------------

# 
# # New ARIMA w/ air temp as a covariate.
# h2 <- Arima(y = as.numeric(STS),
#             order = order,
#             seasonal = FALSE,
#             xreg = as.matrix(cbind(harmonics, 
#                    airvars[,c(2:4)])))
# 
# 
# # Check diagnostics.
# h2 %>% checkresiduals()
# summary(h2)
# 
# 
# # Project air temperature out one months using current year only.
# # NOTE THAT THIS IS LIKELY INACCURATE AS IT DOES NOT ACCOUNT FOR
# # ANTICIPATED WEATHER CHANGES (e.g., PRECIPITATION or COOLING).
# airTS <- ts(airvars[airvars$date > "2020-01-01", "airL1"], 
#             frequency = 52)
# 
# # Forecast air temperatures out one month.
# forecast(airTS, h = fh) %>% plot()
# 
# # Assign projected air temperature data to a DF.
# # Below, I increase the projected air temperature variables by 1/4 such that they 
# # match the observed temperature. This is just for exploration purposes. Also the 
# # forecast of air temperature is highly imprecise and the trend appears to decline 
# # more steeply than what would be expected in recent years.
# # Use independent meteorological 14-day forecasts in future. 
# 
# prjAir <- as.data.frame(forecast(airTS, h = fh)) %>% 
#   rename("airL1" = "Point Forecast") %>% 
#   mutate(airL1 = airL1,
#          airL2 = airL1^2,
#          airL3 = airL1^3) %>% 
#   dplyr::select(starts_with("air"))
# 
# 
# # For experimentation only.
# # prjAir[1:16,1] <- 30
# # prjAir[1:16,2] <- 30^2
# # prjAir[1:16,3] <- 30^3
# 
# 
# # Bind projected air temperatures to Fourier terms.
# h2.newvars <- as.data.frame(newharmonics) %>% 
#   cbind(., prjAir)
# 
# 
# # Forecast air temp + seasonality model using forecasted data.
# h2f <- forecast(h2, xreg = as.matrix(h2.newvars)); head(h2f)
# plot(h2f) # Trajectory and prediction intervals seem reasonable.
# plot(h2f, xlim = c(230, 255))
# 
# # Plot full time series. 
# (full2 <- ggplot(data  = df2,
#                  aes(x = date,
#                      y = meanT)) +
#     geom_hline(yintercept = c(18,19,20),
#                colour = "red2",
#                linetype = "dashed",
#                alpha = c(1/8, 2/5, 1)) +
#     geom_ribbon(aes(ymin = lwr95, ymax = upr95,
#                     fill = type),
#                    alpha = 1/5) + 
#     geom_line(linewidth  = 1, 
#               aes(colour = type),
#               alpha = 4/5) + 
#     mytheme +
#     labs(x = NULL, y = "Somass River Temperature (°C)") +
#     theme(plot.margin = margin(5, 10, 0.1, 5, "pt")) +
#     guides(colour = "none") +
#   scale_x_date(date_breaks = "1 year",
#                date_labels = "%Y"))
# 
# # Plot zoomed-in time series.
# zi2 <- full2 +
#     geom_point(aes(colour = type), size = 2.5,
#                alpha = 1/2) +
#     scale_x_date(limits = c(max(df2$date) - 140, 
#                             as.Date(max(df2$date))),
#                  date_breaks = "1 month",
#                  date_labels = "%b") +
#     theme(legend.position = "none") +
#     scale_y_continuous(breaks = seq(2, 25, 4),
#                        limits = c(9, 23))
# gginnards::move_layers(zi2, "GeomPoint", position = "top")
# 
# # Combine full and zoomed in time series plots.
# (comb2 <- cowplot::plot_grid(full2, zi2, ncol = 1,
#                              rel_heights = c(1, 0.8),
#                              align = "V"))
# 
# ggsave("plots/ARIMA_wTemp.png", units = "px",
#        width = 2500, height = 2000)


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
  forecast(fit, xreg = newxreg, h = h) }

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
                               order = c(1,0,0),
                               seasonal = FALSE,
                               xreg = harmonics),
                         # 6 weeks of forecasted harmonics (~newxreg).
                         xreg   = newharmonics, 
                         # Goal is future values.
                         future = TRUE,
                         bootstrap = TRUE)
  
  # Print progress bar for each iteration. 
  progress(i, nsim) 
}

# Isolate simulated data in dataframe. 
(future_sims <- as.data.frame(future) %>% 
  # h = forecast horizon (in weeks).
  mutate(h = as.factor(paste(seq(1, 6, 1), 
                             "weeks"))) %>% 
  # Pivot to long-form for easier grouping. 
  pivot_longer(cols = -c(h),
               names_to  = "sim", 
               values_to = "temp") %>% 
  ggplot(data = ., aes(x = temp)) +
  geom_histogram(fill = "gray90",
                 color = "black") +
    facet_wrap(. ~ h, scales = "free_x") +
    mytheme +
    labs(x = "Temperature (C)", y = NULL))

ggsave("plots/simulated_tempdists.png", units = "px",
       width = 2000, height = 1500)

sim_summ <- future_sims %>% 
  group_by(h) %>% 
  # For each forecasted week, get mean temperature,
  # and % of days (from simulation) above 18, 19, 20C. 
  summarise(mean = mean(temp, na.rm = TRUE),
            p18  = round(sum(temp > 18, na.rm = TRUE)/nsim * 100, 1),
            p19  = round(sum(temp > 19, na.rm = TRUE)/nsim * 100, 1),
            p20  = round(sum(temp > 20, na.rm = TRUE)/nsim * 100, 1)) %>% 
  mutate(date = max(impDF$date) + seq(7, 7*fh, 7)) 


zi2 +
  geom_text(data = sim_summ,
            aes(x = date, y = 21,
                label = sprintf("%0.1f", p18)),
            size = 3, hjust = 0) +
  geom_text(data = sim_summ,
            aes(x = date, y = 22,
                label = sprintf("%0.1f", p19)),
            size = 3, hjust = 0) +
  geom_text(data = sim_summ,
            aes(x = date, y = 23,
                label = sprintf("%0.1f", p20)),
            size = 3, hjust = 0) +
  scale_y_continuous(limits = c(9, 24)) +
  annotate("text", y = c(21, 22, 23), size = 3, 
           label = c("p18 (%) = ", 
                     "p19 (%) = ",
                     "p20 (%) = "),
           x = max(impDF$date)-1) 
  

ggsave("plots/forecast_wProbs.png", units = "px",
       width = 2200, height = 1200)

  
# -------------------------------------------------------------------------
