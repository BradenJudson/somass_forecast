
# Libraries.
library(tidyverse); library(ggplot2); library(ggrepel)
library(ggpmisc)


# Set working directory.
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Custom theme.
mytheme <- theme_bw() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.margin = margin(c(0,0,-5,0)))

# ------------------------------------------------------------------------------

# Checking for relationships with large-scale oceanographic changes and annual 
# patterns in Somass River temperature. 

# Read in entirety of spot check data.
annual <- spot %>% 
  filter(doy > 121 & doy < 274) %>% 
  group_by(Year) %>% 
  summarise(t18 = length(MeanWaterT > 18),
            t19 = length(MeanWaterT > 19),
            max = max(MeanWaterT, na.rm = TRUE),
            mean = mean(MeanWaterT, na.rm = TRUE))

# NOAA ocean index data.
noaa <- read_table(file = "cpc.ncep.noaa.gov_data_indices_oni.ascii.txt") %>% 
  filter(SEAS %in% c("DJF", "JFM", "FMA", "MAM")) %>% 
  group_by(YR) %>% 
  summarise(anomM = mean(ANOM), anomS = sd(ANOM),
            tempM = mean(TOTAL), tempS = sd(TOTAL)) %>% 
  filter(YR %in% annual$Year)

# Join above two DFs for comparisons.
temps <- merge(annual, noaa, by.x = "Year", by.y = "YR") %>% 
  pivot_longer(cols = c("t18", "t19", "max", "mean"), 
               names_to = "SomVar", values_to = "SomVal") %>% 
  pivot_longer(cols = c("anomM", "tempM"), 
               names_to = "ONIvar", values_to = "ONIvals")

# Labelling vector for facet_wraps.
plotlabs <- as_labeller(c('anomM' = "SST Anomaly Index",
                          'tempM' = "Mean Pacific SST (°C)",
                          "max" = "Maximum temperature (°C)",
                          "mean" = "Mean temperature (°C)",
                          "t18" = "Days above 18°C",
                          "t19" = "Days above 19°C"))

# Plot pairwise relationships.
ggplot(data = temps,
       aes(x = ONIvals, y = SomVal)) +
  geom_smooth(method = "lm", colour = "black",
              linetype = "dashed",
              alpha = 1/10) +
  geom_point(shape = 21, fill = "gray70",
             size = 2, alpha = 3/4) + 
  facet_grid(rows = vars(SomVar), cols = vars(ONIvar),
             scales = "free", switch = "y",labeller = plotlabs) +
  mytheme +
  labs(x = "", y = "Somass River Values")

# Most relationships above appear very weak except for annual Somass temperature
# which appears to be linearly correlated with SST Anomalies and mean Pacific SST.

ggsave("plots/sst_somass.png", units = "px",
       width = 2000, height = 2250)

# More closely inspect significant relationships.
ggplot(data = temps[temps$SomVar == "mean",] %>% 
         mutate(yr = str_sub(3,4, string = Year)),
       aes(x = ONIvals, y = SomVal)) + 
  geom_smooth(method = "lm", colour = "black",
              alpha = 1/5, linetype = "dashed",
              linewidth = 1/2) +
  geom_point() +
  geom_label_repel(aes(label = yr), box.padding = 0, 
                   point.padding = 0, min.segment.length = 0) +
  facet_wrap(~ONIvar, scales = "free", 
             labeller = plotlabs) +
  mytheme + 
  labs(x = "", y = "Average Somass River temperature (°C)") +
  stat_poly_eq(use_label(c("R2", "p")),
               label.x = "right",
               label.y = "bottom")

ggsave("plots/meanSomass_sst.png", units = "px",
       width = 3000, height = 2000)

