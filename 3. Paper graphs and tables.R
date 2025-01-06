# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(viridis)
library(ggpmisc)
library(cowplot)
library(grid)
library(patchwork)
library(readr)

# Load AfConc simulated data
dssat_data <- read_csv("Dssat_simulation_original_AfConc.csv")

# Cleaning and modifying data
final_data <- dssat_data %>%
  separate(EXPERIMENT, into = c("Year", "station"), sep = "_") %>% 
  filter(station %in% c("AAAF", "AAAV", "AAAC", "AAAL", "AAAR", "AABB", "AABC",
                     "AAAI", "AAAK", "AAAM", "AAAT", "AAAA"))%>%
  mutate(county = recode(station,
                          "AAAF" = "Appling", # county number: 1, 5, 19, 69, 71, 155, 175, 205, 271, 275, 277, 305
                          "AAAV" = "Wayne",
                          "AAAC" = "Bacon",
                          "AAAL" = "Coffee",
                          "AAAR" = "Telfair",
                          "AABB" = "Tift",
                          "AABC" = "Irwin",
                          "AAAI" = "Mitchell",
                          "AAAK" = "Thomas",
                          "AAAM" = "Laurens",
                          "AAAT" = "Colquitt",
                          "AAAA" = "Berrien"))%>%
  mutate(planting_date = case_when(
    RUN >= 1 & RUN <= 10 ~ "May 5",
    RUN >= 11 & RUN <= 20 ~ "May 12",
    RUN >= 21 & RUN <= 30 ~ "May 19",
    RUN >= 31 & RUN <= 40 ~ "May 26",
    TRUE ~ NA_character_  
    )
  )%>%
  mutate(
    last_digit = RUN %% 10,  # This will calculate the last digit (0 will be considered as 10)
    treatment = case_when(
      last_digit == 1 ~ "Rainfed",
      last_digit == 2 ~ "DE30IT30",
      last_digit == 3 ~ "DE30IT50",
      last_digit == 4 ~ "DE30IT70",
      last_digit == 5 ~ "DE50IT30",
      last_digit == 6 ~ "DE50IT50",
      last_digit == 7 ~ "DE50IT70",
      last_digit == 8 ~ "DE70IT30",
      last_digit == 9 ~ "DE70IT50",
      last_digit == 0 ~ "DE70IT70",  
      TRUE ~ NA_character_  
    )
  ) %>%
  select(-last_digit)

## Figure 1. Simulated aflatoxin contamination levels under various irrigation treatments

# Reorganize levels of 'treatment' for custom x-axis order
final_data$treatment <- factor(final_data$treatment, 
                               levels = c("Rainfed", 
                                          "DE30IT30", "DE30IT50", "DE30IT70",
                                          "DE50IT30", "DE50IT50", "DE50IT70",
                                          "DE70IT30", "DE70IT50", "DE70IT70"))

table2 <- ggplot(final_data, aes(x = treatment, y = log10(AFCON + 1))) +
  geom_boxplot() +
  scale_y_continuous(labels = function(x) 10^x) + 
  labs(x = "Irrigation Treatment",
       y = "Simulated Aflatoxin Concentration (µg/kg, ppb)") +
  theme_bw() +
  theme(axis.title = element_text(family = "Arial",  size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12, angle = 45, hjust = 1))

table2

ggsave("paper_table2.png", plot = table2,
       width = 170, height = 140, units = 'mm')

## Figure 2. Impact of soil type and irrigation on simulated aflatoxin concentrations. 
soil <- ggplot(final_data, aes(x = treatment, y = log10(AFCON + 1), fill = soil)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  # Setting y-axis labels to show the original AFCON values
  scale_y_continuous(labels = function(x) 10^x ) + 
  labs(x = "Irrigation Treatment",
       y = "Simulated aflatoxin concentration (µg/kg, ppb)") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12, angle = 45, hjust = 1)) +
  guides(fill=guide_legend(title="Soil Type"))

soil

ggsave("paper_figure1_different_soil.png", plot = soil,
       width = 170, height = 140, units = 'mm')

# Figure 3. Impact of planting date on simulated mean aflatoxin (Rainfed vs. Irrigation)

# Add irrigation column to the dataset
simulateddata <- final_data %>%
  mutate(irrigation = ifelse(treatment == "Rainfed", "Rainfed", "Irrigated"))

# Summarize the data for yearly mean, soil types, and irrigation
yearly_data <- simulateddata %>%
  group_by(year, planting_date, irrigation) %>%
  summarise(mean_AFCON = mean(AFCON, na.rm = TRUE),
            med_AFCON = median(AFCON, na.rm = TRUE)) %>%
  ungroup() 

# Convert year to a factor for proper grouping on the x-axis
yearly_data$year <- as.factor(yearly_data$year)


# Reorder year and planting_date
yearly_data$year <- factor(yearly_data$year, levels = rev(sort(unique(yearly_data$year))))
yearly_data$irrigation <- factor(yearly_data$irrigation, levels = c("Rainfed", "Irrigated"))
yearly_data$planting_date <- factor(yearly_data$planting_date, levels = c("May 26", "May 19", "May 12", "May 5"))


# plot
sowingdate <- ggplot(yearly_data, aes(y = year, x = mean_AFCON, fill = planting_date)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black",
           size = 0.5, width = 0.8) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.8),color = "black") +
  facet_wrap(~ irrigation, scales = "free_x", ncol = 2) + # Rainfed on the left, Irrigated on the right
  labs(y = "Year",
       x = "Simulated Mean Aflatoxin Concentration (µg/kg, ppb)",
       fill = "Planting Date") +
  #scale_fill_manual(values = c("May 5" = "#8dd3c7", "May 12" = "#ffffb3", "May 19" = "#bebada", "May 26" = "#fb8072"),
  #                  breaks = c("May 5", "May 12", "May 19", "May 26")) +
  scale_fill_brewer(palette = "Set2",  breaks = c("May 5", "May 12", "May 19", "May 26")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12))

sowingdate

ggsave("paper_figure2_different_sowingdate.png", plot = sowingdate,
       width = 150, height = 170, units = 'mm')


# Load SoilWat simulated data

data_soil <- read_csv("Dssat_simulation_original_SoilWat.csv")

data_soil <- data_soil %>%
  separate(EXPERIMENT, into = c("Year", "station"), sep = "_") %>% 
  filter(station %in% c("AAAF", "AAAV", "AAAC", "AAAL", "AAAR", "AABB", "AABC",
                        "AAAI", "AAAK", "AAAM", "AAAT", "AAAA"))%>%
  mutate(county = recode(station,
                         "AAAF" = "Appling", # 1, 5, 19, 69, 71, 155, 175, 205, 271, 275, 277, 305
                         "AAAV" = "Wayne",
                         "AAAC" = "Bacon",
                         "AAAL" = "Coffee",
                         "AAAR" = "Telfair",
                         "AABB" = "Tift",
                         "AABC" = "Irwin",
                         "AAAI" = "Mitchell",
                         "AAAK" = "Thomas",
                         "AAAM" = "Laurens",
                         "AAAT" = "Colquitt",
                         "AAAA" = "Berrien"))%>%
  mutate(planting_date = case_when(
    RUN >= 1 & RUN <= 10 ~ "May 5",
    RUN >= 11 & RUN <= 20 ~ "May 12",
    RUN >= 21 & RUN <= 30 ~ "May 19",
    RUN >= 31 & RUN <= 40 ~ "May 26",
    TRUE ~ NA_character_  # Handles cases outside these ranges
  )
  )%>%
  mutate(
    last_digit = RUN %% 10,  # This will calculate the last digit (0 will be considered as 10)
    treatment = case_when(
      last_digit == 1 ~ "Rainfed",
      last_digit == 2 ~ "DE30IT30",
      last_digit == 3 ~ "DE30IT50",
      last_digit == 4 ~ "DE30IT70",
      last_digit == 5 ~ "DE50IT30",
      last_digit == 6 ~ "DE50IT50",
      last_digit == 7 ~ "DE50IT70",
      last_digit == 8 ~ "DE70IT30",
      last_digit == 9 ~ "DE70IT50",
      last_digit == 0 ~ "DE70IT70",  
      TRUE ~ NA_character_  # for any unexpected values
    )
  ) %>%
  select(-last_digit)

# Figure 4. Yearly cumulative rainfall by planting dates 

average_simulated_data <- data_soil %>%
  group_by(county, treatment, soil, year, RUN) %>%
  filter(DATE == max(DATE)) %>% 
  #arrange(RUN) %>%
  #group_by(soil, year, RUN) %>%
  select(DATE, RUN,year, soil, PREC,  `IR#C`,  IRRC, county, planting_date, treatment)%>%
  filter(treatment == "Rainfed") 


average_simulated_data$planting_date <- factor(
  average_simulated_data$planting_date, 
  levels = c("May 5", "May 12", "May 19", "May 26"))

sowingdate_rainfall <- ggplot(average_simulated_data, aes(x = as.factor(year), y= PREC, fill = planting_date)) +
  geom_boxplot(outlier.size = 1, outlier.shape = 21, color = "black") +
  scale_fill_manual(values = c("May 5" = "#E78AC3", 
                               "May 12" = "#8DA0CB", 
                               "May 19" = "#FC8D62", 
                               "May 26" =  "#66C2A5" )) +
  labs(x = "Year", y = "Cumulative Rainfall (mm)", fill = "Planting Date") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12))

sowingdate_rainfall

ggsave("paper_figure_cumulative_rainfall_sowingdate.png", plot = sowingdate_rainfall,
       width = 170, height = 150, units = 'mm')






# Figure 5. Comparison of water availability (higher vs. lower aflatoxin scenarios)

new <- data_soil  %>%
  mutate(WAP = ceiling((DAS+1)/7))%>%
  arrange(year, county, planting_date, soil, treatment, RUN, WAP) %>%
  group_by(year, county, planting_date, soil, treatment, RUN, WAP) %>%
  summarize(max_prec = max(PREC, na.rm = TRUE),
            max_irrc = max(IRRC, na.rm = TRUE),
            max_ir_events = max(`IR#C`, na.rm = TRUE)) %>%
  mutate(weekly_prec = max_prec - lag(max_prec, default = 0),
         weekly_irrc = max_irrc - lag(max_irrc, default = 0),
         weekly_ir_events = max_ir_events - lag(max_ir_events, default = 0),
         total_water = weekly_prec + weekly_irrc)



irrigation_guide <- data.frame(
  WAP = 1:22,
  Inches_Per_Week = c(
    0.08, 0.26, 0.39, 0.55, 0.76, 0.95, 1.08, 1.29, 1.49, 1.59,
    1.58, 1.49, 1.47, 1.30, 1.16, 0.97, 0.83, 0.67, 0.49, 0.30,
    0.14, 0.01)) %>%
  mutate(Requirements_mm = Inches_Per_Week * 25.4)

comparison_cumulative <- new %>%
  left_join(irrigation_guide, by = "WAP") %>%
  mutate(Water_Difference = total_water - Requirements_mm)

# write_csv(comparison_cumulative, "dssatdata_whole_20241125.csv")
# comparison_cumulative <- read_csv("dssatdata_whole_20241125.csv")

# Plot water differences
plot_A <- ggplot(comparison_cumulative %>% 
                   filter(treatment == "Rainfed" & soil == "Leefield Soil" & 
                            year %in% c(2019) & planting_date %in% c("May 26")), 
                 aes(x = WAP, y = Water_Difference)) +
  geom_bar(stat = "identity", aes(fill = Water_Difference > 0), alpha = 0.8, color = "black") +
  facet_wrap(~ county ,  ncol = 4) +
  scale_fill_manual(
    values = c("TRUE" = "gray60", "FALSE" = "white"),
    labels = c("Deficit", "Sufficient")) +
  labs(x = "Weeks After Planting (WAP)",
       y = "Water Difference (mm)",
       fill = "Water Status", 
       title = "(A) Planting Date: 2019/05/26") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        #strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

plot_A

plot_B <- ggplot(comparison_cumulative %>% 
                   filter(treatment == "DE30IT30" & soil == "Leefield Soil" & 
                            year %in% c(2021) & planting_date %in% c("May 12")), 
                 aes(x = WAP, y = Water_Difference)) +
  geom_bar(stat = "identity", aes(fill = Water_Difference > 0), alpha = 0.8, color = "black") +
  facet_wrap(~ county ,  ncol = 4) +
  scale_fill_manual(values = c("TRUE" = "gray60", "FALSE" = "white"), labels = c("Deficit", "Sufficient")) +
  labs(x = "Weeks After Planting (WAP)",
       y = "Water Difference (mm)",
       fill = "Water Status", 
       title = "(B) Planting Date: 2021/05/19") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none", 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

plot_B


combined <- plot_A / plot_B + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined

ggsave("figure5_20192021comparison_waterstatus.png", plot = combined,
       width = 170, height = 220, units = 'mm') 






# Figure 6. observed aflatoxin vs simulated aflatoxin 
# simulated
simulated <- final_data %>% 
  select(year, county, AFCON ) %>% 
  mutate(value = "Simulated")

# observed
at_data <- read_csv("tidy_data_2016_2023.csv")
at_data$county <- as.numeric(at_data$county)

#unique(at_data$county)

observed <- at_data %>% 
  filter(county %in% c(1, 5, 19, 69, 71, 155, 175, 205, 271, 275, 277, 305))%>% 
  select(crop_year, county, aflatoxin)

observed$county <- as.character(observed$county)

county_mapping <- c("1" = "Appling", "5" = "Bacon", "19" = "Berrien", 
                    "69" = "Coffee", "71" = "Colquitt", "155" = "Irwin", 
                    "175" = "Laurens", "205" = "Mitchell", "271" = "Telfair",
                    "275" = "Thomas", "277" = "Tift", "305" = "Wayne")

# unique(observed$county)
observed$county <- county_mapping[observed$county]
observed$value <- "Observed"

observed <- observed %>% 
  select(crop_year,county,aflatoxin,value)

colnames(simulated) <- c("year","county", "aflatoxin", "value")
colnames(observed) <- c("year", "county", "aflatoxin", "value")

combined <- rbind(observed,simulated)

# write_csv(combined, "dssat_Combined.csv")
# combined <- read_csv("dssat_Combined.csv")

average_aflatoxin <- combined %>%
  group_by(year, value) %>%
  summarize(avg_aflatoxin = mean(aflatoxin, na.rm = TRUE),
            sd= sd(aflatoxin, na.rm = TRUE),
            max = max(aflatoxin, na.rm = TRUE),
            min = min(aflatoxin, na.rm = TRUE), 
            med = median(aflatoxin, na.rm = TRUE), .groups = "drop")

figure6 <- ggplot(combined, aes(x = as.factor(year), 
                                y = aflatoxin, 
                                #y = log10(aflatoxin + 1),
                                fill = value)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_y_log10() +
  labs(x = "",
       y = "Aflatoxin concentration (µg/kg, ppb)") +
  scale_fill_brewer(palette = "Set3") + 
  #scale_y_continuous(labels = function(x) 10^x ) + 
  #scale_y_continuous(trans = 'log10',limits = c(0.1, 1000)) +  # Apply log scale with custom breaks and limits starting from 0.1
  theme_bw() +
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.5, 'cm'), 
        axis.title = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12)) +
  guides(fill=guide_legend(title=""))

figure6

ggsave("figure6_year_obs_sim_comparison.png", plot = figure6,
       width = 170, height = 140, units = 'mm') 




# Real world scenarios
# Figure 7. resampled simulated vs observed aflatoxin concentration
library(lhs)


# Create a scenarios dataframe using LHS
n_scenarios <- 500  # Number of scenarios
lhs_samples <- randomLHS(n_scenarios, 2)  # 2 variables: irrigation and year

# Scale LHS samples to match the range of `irrigation` and `year`
scenarios <- data.frame(
  irrigation = ifelse(lhs_samples[, 1] < 0.5, "Rainfed", "Irrigated"),  # Binary split for irrigation
  year = round(lhs_samples[, 2] * (2023 - 2016) + 2016),                # Scale to years 2016-2023
  stringsAsFactors = FALSE
)

monte_carlo <- function(simulateddata, scenarios) {
  sample_data <- scenarios %>%
    rowwise() %>%
    mutate(
      aflatoxin = {
        # Filter simulateddata to match current row's irrigation and year
        matched_data <- simulateddata %>%
          filter(
            irrigation == irrigation,
            year == year
          ) %>%
          pull(AFCON)
        
        # Sample from matched_data if not empty, otherwise return NA
        if (length(matched_data) > 0) {
          sample(matched_data, size = 1, replace = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  
  # Calculate summary statistics for the sampled aflatoxin levels
  summary_stats <- sample_data %>%
    group_by(year) %>%
    summarise(
      mean_aflatoxin = mean(aflatoxin, na.rm = TRUE),
      median_aflatoxin = median(aflatoxin, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(summary_stats)
}

set.seed(123)
n_iterations <- 1000

# Perform Monte Carlo simulations with LHS scenarios
monte_carlo_results <- replicate(
  n_iterations,
  monte_carlo(simulateddata, scenarios),
  simplify = FALSE
)

combined_results <- do.call(rbind, lapply(monte_carlo_results, as.data.frame))
write.csv(combined_results, "monte_carlo_results.csv", row.names = FALSE)


# Boxplot to show variability across iterations
combined_results_long <- do.call(rbind, monte_carlo_results) %>%
  pivot_longer(cols = c(mean_aflatoxin, median_aflatoxin), names_to = "metric", values_to = "value") %>% 
  filter(metric == "mean_aflatoxin")

results_selected <- combined %>%
  filter(value == "Observed") %>% 
  mutate(metric = "Observed",
         value = aflatoxin) %>% 
  select(year, value, metric)

figure7data <- rbind(combined_results_long, results_selected)

figure7 <- ggplot(figure7data, aes(x = factor(year), y = value, fill = metric)) +
  geom_boxplot() +
  labs(x = "Year", y = "Aflatoxin concentration (µg/kg, ppb)") +
  scale_fill_brewer(labels = c("Simulated (Resampled)", "Observed"), palette = "Set3") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, 'cm'), 
    axis.title = element_text(family = "Arial", size = 12),
    axis.text.y = element_text(family = "Arial", size = 12),
    axis.text.x = element_text(family = "Arial", size = 12)) +
  guides(fill=guide_legend(title=""))

ggsave("figure7_obs_Resim_comparison.png", plot = figure7,
       width = 170, height = 140, units = 'mm')  






# Table 8. total water difference vs. simulated aflatoxin concentrations 

water_deficit_summary <- comparison_cumulative %>%
  # filter(treatment == "Rainfed" & soil == "Leefield Soil" &            
  #          year %in% c(2019, 2016) & planting_date %in% c("May 26", "May 19")) %>%
  mutate(Water_Status = ifelse(Water_Difference >= 0, "Sufficient", "Deficit")) %>%
  group_by(county, planting_date, year, treatment, soil) %>%
  summarize(
    weeks_With_Deficit = sum(Water_Difference < 0),
    Total_Days = n()
  )


complete_dataset <- merge(final_data, water_deficit_summary)
# write_csv(complete_dataset, "dssatdata_20241125.csv")
# complete_dataset <- read_csv("dssatdata_20241125.csv")

critical_weeks <- comparison_cumulative %>%
  filter(WAP  >= 10 & WAP  <= 17) %>%
  select(county, planting_date, treatment, Water_Difference, WAP) %>%
  group_by(county, planting_date, treatment, soil, year) %>%
  summarize(total_water_difference = sum(Water_Difference, na.rm = TRUE)) 

total_water_difference <- critical_weeks %>% 
  ungroup() %>% 
  group_by(county, planting_date, treatment, year) %>% 
  summarize(mean_total_water_difference = mean(total_water_difference))

q <- merge(complete_dataset, critical_weeks)

water_year <- ggplot(q, aes(x = total_water_difference, y = AFCON, color = as.factor(year))) +
  geom_point(size = 2, alpha = 0.8) +  
  #geom_smooth(method = "lm", se = FALSE, 
  #            aes(group = soil), linetype = "dashed", size = 0.8) + 
  scale_color_brewer(palette = "Paired") +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    #title = "Relationship Between Total Water Difference and Aflatoxin Concentration",
    #subtitle = "Data grouped by soil type",
    x = "Total Water Difference (mm)",
    y = "Simulated Aflatoxin Concentration (µg/kg, ppb)",
    color = "Crop Year"
  ) +
  theme_bw() +  
  theme(
    #plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    #plot.subtitle = element_text(size = 12, hjust = 0.5), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.7,0.6),  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)) 
water_year



water_soil <- ggplot(q, aes(x = total_water_difference, y = AFCON, color = soil)) +
  geom_point(size = 2, alpha = 0.8) +  
  #geom_smooth(method = "lm", se = FALSE, 
  #            aes(group = soil), linetype = "dashed", size = 0.8) + 
  scale_color_manual(values = c("Dothan Soil" = "#A6D854", 
                                "Fuquay Soil" = "#FFD92F", 
                                "Leefield Soil" = "#E5C494", 
                                "Pelhem Soil" =  "#B3B3B3",
                                "Tifton Soil" = "#E78AC3")) +
  #scale_color_brewer(palette = "Dark2") +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    #title = "Relationship Between Total Water Difference and Aflatoxin Concentration",
    #subtitle = "Data grouped by soil type",
    x = "Total Water Difference (mm)",
    y = "Simulated Aflatoxin Concentration (¥ìg/kg, ppb)",
    color = "Soil Type"
  ) +
  theme_bw() +  
  theme(
    #plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    #plot.subtitle = element_text(size = 12, hjust = 0.5), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.7,0.6),  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)) 
water_soil

water_planting <- ggplot(q, aes(x = total_water_difference, y = AFCON, color = planting_date )) +
  geom_point(size = 2, alpha = 0.8) +  
  #geom_smooth(method = "lm", se = FALSE, 
  #            aes(group = soil), linetype = "dashed", size = 0.8) + 
  scale_color_manual(values = c("May 5" = "#E78AC3", 
                                "May 12" = "#8DA0CB", 
                                "May 19" = "#FC8D62", 
                                "May 26" =  "#66C2A5")) +
  #scale_color_brewer(palette = "Dark2") +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    #title = "Relationship Between Total Water Difference and Aflatoxin Concentration",
    #subtitle = "Data grouped by soil type",
    x = "Total Water Difference (mm)",
    y = "Simulated Aflatoxin Concentration (µg/kg, ppb)",
    color = "Planting Date"
  ) +
  theme_bw() +  
  theme(
    #plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    #plot.subtitle = element_text(size = 12, hjust = 0.5), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.7,0.6),  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)) 

water_planting

water_figure <- grid.arrange( water_soil, water_planting,water_year, ncol = 3)
water_figure

ggsave("figure8_discussion_water.png", plot = water_figure,
       width = 250, height = 100, units = 'mm') 







# Table 2. ANOVA 

summary_stats <- final_data %>%
  group_by(year, station,  soil) %>%
  summarize(mean_aflatoxin = mean(AFCON), 
            sd_aflatoxin = sd(AFCON))

# result table2 
stats <- final_data %>%
  group_by(treatment, year, station,  soil, RUN) %>%
  summarize(mean_aflatoxin = mean(AFCON), 
            sd_aflatoxin = sd(AFCON),
            median = median(AFCON),
            Max = max(AFCON))

anova_year <- aov(AFCON ~ factor(year), data = final_data)
anova_county <- aov(AFCON ~ factor(station), data = final_data)
anova_irrigation <- aov(AFCON ~ factor(RUN), data = final_data)
anova_irrigation1 <- aov(AFCON ~ factor(treatment), data = final_data)
anova_sowing <- aov(AFCON ~ factor(planting_date), data = final_data)
anova_soil <- aov(AFCON ~ factor(soil), data = final_data)

# Summary of ANOVA results
summary(anova_year)
summary(anova_county)
summary(anova_irrigation)
summary(anova_irrigation1)
summary(anova_sowing)
summary(anova_soil)

anova_results <- aov(AFCON ~ Year + planting_date +  soil + treatment +station , data = final_data)
summary(anova_results)



## Table3
# Perform paired t-test for means
t_test_means <- t.test(county_level$mean_Observed, county_level$mean_Simulated, paired = TRUE)
t_test_means

# Perform paired t-test for medians
t_test_medians <- t.test(county_level$median_Observed, county_level$median_Simulated, paired = TRUE)
t_test_medians


rmse <- function(obs, sim) {
  sqrt(mean((obs - sim)^2, na.rm = TRUE))
}


# RMSE for means
rmse_means <- rmse(county_level$mean_Observed, county_level$mean_Simulated)
# RMSE for medians
rmse_medians <- rmse(county_level$median_Observed, county_level$median_Simulated)

cat("RMSE for means:", rmse_means, "\nRMSE for medians:", rmse_medians)


bias <- function(obs, sim) {
  mean(sim - obs, na.rm = TRUE)
}

# Bias for means
bias_means <- bias(county_level$mean_Observed, county_level$mean_Simulated)
# Bias for medians
bias_medians <- bias(county_level$median_Observed, county_level$median_Simulated)

cat("Bias for means:", bias_means, "\nBias for medians:", bias_medians)


# MAE
mae <- function(observed, simulated) {
  mean(abs(observed - simulated), na.rm = TRUE)
}

mae_means <- mae(county_level$mean_Observed, county_level$mean_Simulated)
mae_medians <- mae(county_level$median_Observed, county_level$median_Simulated)

cat("MAE for means:", mae_means, "\nMAE for medians:", mae_medians)


# Calculate R2
r_squared <- function(observed, simulated) {
  ss_total <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  ss_residual <- sum((observed - simulated)^2, na.rm = TRUE)
  1 - (ss_residual / ss_total)
}

# R2 for means
r2_means <- r_squared(county_level$mean_Observed, county_level$mean_Simulated)
# R2 for medians
r2_medians <- r_squared(county_level$median_Observed, county_level$median_Simulated)

cat("R?? for means:", r2_means, "\nR?? for medians:", r2_medians)
