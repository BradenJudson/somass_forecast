
# Libraries.
library(tidyverse); library(ggplot2); library(ggrepel)
library(ggpmisc); library(lubridate)


# Set working directory.
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Custom theme.
mytheme <- theme_bw() +
  theme(legend.position = "top", 
        legend.title  = element_blank(),
        legend.margin = margin(c(0,0,-5,0)),
        plot.margin   = margin(c(2,10,2,2)))


# Snow pack --------------------------------------------------------------------

# Mount Cokely snow pack data
# https://aqrt.nrs.gov.bc.ca/Data/Location/Summary/Location/3B02A/Interval/Latest

snow <- read.csv("DataSetExport-SD.Field Visits@3B02A-20230825160412.csv",
                 skip = 2) %>% 
  select(c(1,3)) %>% 
  `colnames<-`(., c("date", "snow_cm")) %>% 
  mutate(date  = mdy(sub(" .*", "", date)),
         Year  = year(date),
         month = month(date)) %>%
  filter(month %in% c(1:4)) %>% 
  group_by(Year) %>% 
  summarise(mSnow = mean(snow_cm, na.rm = TRUE),
            sSnow = sd(snow_cm, na.rm = TRUE),
            nSnow = n())

# Visualize snow pack time series.
# Note missing data in 2019-2022.
ggplot(data = snow, aes(x = Year, 
                        y = mSnow,
                        group = 1)) +
  geom_hline(yintercept = 0,
             colour = "blue") +
  geom_errorbar(aes(ymin = mSnow - sSnow,
                    ymax = mSnow + sSnow),
                alpha = 1/2) +
  geom_line(alpha = 1/2) +
  geom_point(size  = 2, colour = "black",
             shape = 21, fill  = "gray") + 
  mytheme +
  scale_x_continuous(breaks = seq(1900, 2100, 3)) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  labs(x = NULL, y = "Mount Cokely snow pack (cm)")

ggsave("plots/cokely_snow.png", units = "px",
       width = 2000, height = 1000)

# -------------------------------------------------------------------------

# Checking for relationships with large-scale oceanographic changes and annual 
# patterns in Somass River temperature. 

'%ni%' <- Negate("%in%")


# Current issue: no data for 2011 at all
# Data for 2007 exists, but not at Paper Mill Dam.


annual <- read.csv("somass_weekly.csv") %>% 
  group_by(year) %>% 
  mutate(doy = yday(date)) %>% 
  filter(doy > 121 & doy < 274) %>%
  filter(year %ni% c("2007", "2011")) %>%      # Exclude missing years here.
  mutate(tDif = doy - lag(doy),
         tATU = tDif * wSom) %>% 
  summarise(ATUs  = sum(tATU, 
            na.rm = TRUE),
            nSur  = n()) 

# Above suggests 2021 missing 2 data points. Should be slightly higher.

ggplot(data = annual, 
       aes(x = year,
           y = ATUs)) +
  geom_line(size = 1, alpha = 1/3) +
  geom_point(size = 2) + mytheme +
  labs(x = NULL, y = "ATUs") +
  scale_x_continuous(breaks = seq(200, 2022, 2))


ggsave("plots/atu_ts.png", units = "px",
       width = 2000, height = 1000)


# # Read in entirety of spot check data.
# annual <- spot %>% 
#   filter(doy > 121 & doy < 274) %>% 
#   group_by(Year) %>%
#   summarise(t18 = sum(MeanWaterT > 18, na.rm = TRUE) / n(),
#             t19 = length(MeanWaterT > 19),
#             max = max(MeanWaterT, na.rm = TRUE),
#             mean = mean(MeanWaterT, na.rm = TRUE))

# NOAA ocean index data.
noaa <- read_table(file = "cpc.ncep.noaa.gov_data_indices_oni.ascii.txt") %>% 
  filter(SEAS %in% c("JFM", "FMA", "MAM")) %>% 
  group_by(YR) %>% 
  summarise(anomM = mean(ANOM), anomS = sd(ANOM),
            tempM = mean(TOTAL), tempS = sd(TOTAL)) %>% 
  filter(YR %in% annual$year)

pni <- read.csv("PNW_aPNI.csv", skip = 1, check.names = F) %>% 
  select(c(Year, 'Annual aPNI')) %>% 
  rename('aPNI' = 'Annual aPNI')

ggplot(data = pni, aes(x = Year, y = aPNI)) +
  geom_point() + geom_line(alpha = 1/2) + 
  theme_bw() + labs(x = NULL, y = 'aPNI') +
  scale_x_continuous(breaks = seq(0, 2100, 10))


# Join above two DFs for comparisons.
temps <- merge(annual, noaa, 
               by.x = "year",
               by.y = "YR") %>% 
  rename("Year" = "year") %>% 
  merge(., pni) %>% 
  merge(., snow[c(1:2)])  %>% 
  mutate(lab = substr(Year, 3, 4))


tempsPiv <- temps %>% 
  pivot_longer(cols = c("ATUs"), 
               names_to = "SomVar", values_to = "SomVal") %>% 
  pivot_longer(cols = c("anomM", "tempM", "aPNI", "mSnow"), 
               names_to = "ClimVar", values_to = "ClimVal")

# Labelling vector for facet_wraps.
plotlabs <- as_labeller(c('anomM' = "SST Anomaly Index",
                          'tempM' = "Mean Pacific SST (°C)",
                          "max"   = "Maximum temperature (°C)",
                          "mean"  = "Mean temperature (°C)",
                          "t18"   = "Days above 18°C",
                          "t19"   = "Days above 19°C",
                          "aPNI"  = "Pacific Northwest Index",
                          "mSnow" = "Mount Cokely snow pack (cm)"))

# Plot pairwise relationships.
ggplot(data = tempsPiv,
       aes(x = ClimVal, y = SomVal)) +
  geom_smooth(method   = "lm", 
              colour   = "black",
              linetype = "dashed",
              alpha = 1/10) +
  geom_point(shape  = 21, fill = "gray70",
             size   = 2, alpha = 3/4) + 
  facet_grid(rows     =  vars(SomVar), 
             cols     =  vars(ClimVar),
             scales   =  "free", 
             switch   =  "y",
             labeller =  plotlabs) +
  mytheme +
  labs(x = NULL, y = "Somass River Values",) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.3))) +
  stat_poly_eq(use_label(c("R2", "p")),
               label.x = "left",
               label.y = "top")

# Most relationships above appear very weak except for annual Somass temperature
# which appears to be linearly correlated with SST Anomalies and mean Pacific SST.

ggsave("plots/sst_somass.png", units = "px",
       width = 2500, height = 2250)

hist(snow$mSnow); hist(log(snow$sSnow))


# lms --------------------------------------------------------------------------

# cor(temps[,colnames(temps) %in% c("mSnow", "aPNI", "anomM", "tempM")])
# 
# 
# library(broom)
# 
# lms <- tempsPiv[,c(1,4:7)] %>% 
#   filter(SomVar == "mean") %>% 
#   nest(data = -c(ClimVar))       %>% 
#   mutate(fit = map(data, ~lm(SomVal ~ ClimVal, data = .x)),
#          tidied = map(fit, glance)) %>%
#   unnest(tidied)
# 
# colnames(temps)
# 
# test <- lm(data = temps,
#            mean ~ mSnow)
# summary(test); AIC(test)


# exp index --------------------------------------------------------------------

# Courtesy of Nicholas Brown, DFO.
pass <- read.csv("somass_sockeye.csv") %>% 
  mutate(date = ymd(date)) %>% 
  select(c(3,4,5,7)) %>% 
  mutate(river = case_when(
    system == "Sproat Lake" ~ "Sproat River",
    system == "Great Central Lake" ~ "Stamp River"
  ))

ggplot(data = pass[pass$year > 1999,], 
       aes(x = date, 
           y = adj_adults)) +
  geom_point(size = 1/2) + 
  facet_wrap(~year, scales = "free") +
  scale_x_date(date_labels = "%b",
               date_breaks = "2 months") +
  mytheme +
  labs(x = NULL, y = "Sockeye")

stampT  <- read.csv("stamp_temps.csv") %>% 
  mutate(river = "Stamp River") %>% 
  rename("temp" = "stamp")
sproatT <- read.csv("sproat_temps.csv") %>% 
  mutate(river = "Sproat River") %>% 
  rename("temp" = "sproat")

rivs <- rbind(stampT, sproatT)

com <- merge(pass, rivs, 
             by = c("year", "date", "river")) %>% 
  group_by(year, river) %>% 
  mutate(exp = temp*(adj_adults/sum(adj_adults))) %>% 
  summarise(expInd = sum(exp, na.rm = TRUE))


ggplot(data = com,
       aes(x = year,
           y = expInd,
           color = river)) +
  geom_line(size  = 1) +
  geom_point(size = 3) +
  mytheme +
  ylab("Temperature Exposure Index")


cor(com[com$river == "Sproat River", "expInd"],
    com[com$river == "Stamp River",  "expInd"],
    method = "spearman")


# Reconfigure above ATU calculations to match the expInd calcs. 
# If I can get ATUs AND Exposure Indices for both stamp and sproat the
# analysis would be much stronger. 


# -------------------------------------------------------------------------




