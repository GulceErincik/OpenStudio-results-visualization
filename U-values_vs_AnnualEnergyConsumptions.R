##
# Author: Gulce Erincik
# Created: 05.24.2024
# 
# Description: U values vs Annual Energy Consumptions
##

install.packages("readxl")
install.packages("tidyverse")

library("readxl")
library("tidyverse")

# ================================================
# Read xlsx files
# ================================================

HVACSizingSummary <- read_excel("RvaluesVsAnnualEnergy.xlsx")
glimpse(HVACSizingSummary)

# ================================================
# New Columns
# ================================================

# Add a new column with the sum of District Heating and District Cooling
HVACSizingSummary <- HVACSizingSummary %>%
  mutate(Total_District_Energy = `District Heating (kWh/m2)` + `District Cooling (kWh/m2)`)

# ================================================
# Line Graph Showing the Relationship between Annual Energy Consumption and Wall U values
# ================================================

# Reorder Materials based on Wall Section U Value
material_order <- HVACSizingSummary %>%
  arrange(`Wall Section U Value`) %>%
  pull(Materials)

# Plotting Materials and Total District Energy
ggplot(HVACSizingSummary, aes(x = factor(Materials, levels = material_order))) +
  geom_line(aes(y = Total_District_Energy, color = "Annual Consumed Energy (kWh/m2)", group = 1), size = 1) +
  geom_line(aes(y = `Wall Section U Value`, color = "Wall Section U Value", group = 1), size = 1) +
  geom_label(aes(y = Total_District_Energy, label = round(Total_District_Energy, 2), color = "Annual Consumed Energy (kWh/m2)"), 
             size = 3, show.legend = FALSE, hjust = -0.1, vjust = 0.5, nudge_y = 0.5, angle = 90, label.r = unit(0, "lines")) + # Remove rectangle around label
  geom_label(aes(y = `Wall Section U Value`, label = round(`Wall Section U Value`, 2), color = "Wall Section U Value"), 
             size = 3, show.legend = FALSE, hjust = -0.1, vjust = 0.5, nudge_y = 0.5, angle = 90, label.r = unit(0, "lines")) + # Remove rectangle around label
  labs(
    title = "Comparison of Total District Energy and Wall Section U Values",
    y = "Value (Total District Energy or U Value)",
    x = "Infill Materials"
  ) +
  scale_color_manual(
    values = c("Annual Consumed Energy (kWh/m2)" = "#F27405", "Wall Section U Value" = "#03738C"),
    name = "Legend",
    labels = c("Total District Energy (kWh/m2)" = "Total District Energy (kWh/m2)", "Wall Section U Value" = "Wall Section U Value")
  ) +
  scale_y_continuous(breaks = seq(0, 90, by = 10), limits = c(0, 90), expand = c(0.1, 0.1)) + # Adjust y-axis limits
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
    axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(20, 20, 20, 20) # Adjust plot margins, increasing the top margin
  ) +
  guides(color = guide_legend(title = "Legend", keywidth = 1, keyheight = 1))

# ================================================
# Line Graph with Expanded Wall U values
# ================================================

# Plotting Materials and Total District Energy
ggplot(HVACSizingSummary, aes(x = factor(Materials, levels = material_order))) +
  geom_line(aes(y = Total_District_Energy, color = "Annual Consumed Energy (kWh/m2)", group = 1), size = 1) +
  geom_line(aes(y = `Wall Section U Value` * 100, color = "Wall Section U Value", group = 1), size = 1) + # Multiply by 100 to expand
  geom_label(aes(y = Total_District_Energy, label = round(Total_District_Energy, 2), color = "Annual Consumed Energy (kWh/m2)"), 
             size = 3, show.legend = FALSE, hjust = -0.1, vjust = 0.5, nudge_y = 0.5, angle = 90, label.r = unit(0, "lines")) + # Remove rectangle around label
  geom_label(aes(y = `Wall Section U Value` * 100, label = round(`Wall Section U Value` * 100, 2), color = "Wall Section U Value"), 
             size = 3, show.legend = FALSE, hjust = -0.1, vjust = 0.5, nudge_y = 0.5, angle = 90, label.r = unit(0, "lines")) + # Remove rectangle around label
  labs(
    title = "Comparison of Total District Energy and Wall Section U Values",
    y = "Value (Total District Energy or U Value)",
    x = "Infill Materials"
  ) +
  scale_color_manual(
    values = c("Annual Consumed Energy (kWh/m2)" = "#F27405", "Wall Section U Value" = "#03738C"),
    name = "Legend",
    labels = c("Annual Consumed Energy (kWh/m2)" = "Annual Consumed Energy (kWh/m2)", "Wall Section U Value" = "Wall Section U Value")
  ) +
  scale_y_continuous(breaks = seq(0, 90, by = 10), limits = c(0, 90), expand = c(0.1, 0.1)) + # Adjust y-axis limits
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
    axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(20, 20, 20, 20) # Adjust plot margins, increasing the top margin
  ) +
  guides(color = guide_legend(title = "Legend", keywidth = 1, keyheight = 1))




