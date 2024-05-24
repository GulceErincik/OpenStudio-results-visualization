##
# Author: Gulce Erincik
# Created: 05.24.2024
# 
# Description: Annual Energy Demands 
##

install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library("readxl")
library("ggplot2")
library("dplyr")
library("tidyr")

# ================================================
# Read xlsx files
# ================================================

AnnualEnergyConsumptions <- read_excel("AnnualEnergyConsumptions.xlsx")
glimpse(AnnualEnergyConsumptions)

# ================================================
# Visualize - Annual Energy Demands (kWh/m2)
# ================================================

# Create a new column for the sum of cooling and heating
AnnualEnergyConsumptions <- AnnualEnergyConsumptions %>%
  mutate(`Sum (kWh/m2)` = `District Cooling (kWh/m2)` + `District Heating (kWh/m2)`)

# Calculate the total energy consumption for each material
total_energy <- AnnualEnergyConsumptions %>%
  group_by(Materials) %>%
  summarise(Total = sum(`Sum (kWh/m2)`, na.rm = TRUE)) %>%
  arrange(desc(Total))

# Reorder the Materials factor based on total energy consumption
AnnualEnergyConsumptions <- AnnualEnergyConsumptions %>%
  mutate(Materials = factor(Materials, levels = total_energy$Materials))

# Reshape the data to long format for ggplot2
data_long <- AnnualEnergyConsumptions %>%
  select(Materials, `District Cooling (kWh/m2)`, `District Heating (kWh/m2)`, `Sum (kWh/m2)`) %>%
  pivot_longer(cols = starts_with("District") | starts_with("Sum"), 
               names_to = "Type", values_to = "Value")

# Create the bar plot with values displayed on the bars
ggplot(data_long, aes(x = Materials, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.25) +
  geom_text(aes(label = round(Value, 1), color = "grayish"),  # Set color to grayish
            position = position_dodge(width = 0.8), vjust = -0.8, hjust = 1, size = 3, angle = 90) +  # Rotate by 90 degrees
  scale_color_manual(values = c(grayish = "#666666"), guide = FALSE) +  # Define grayish color
  labs(title = "Annual Energy Consumption per Material",
       y = "Energy Consumption (kWh/m2)",
       x = "Infill Materials") +
  scale_fill_manual(values = c("District Cooling (kWh/m2)" = "#03738C",
                               "District Heating (kWh/m2)" = "#F27405",
                               "Sum (kWh/m2)" = "#F5C464"),
                    name = "Activity",
                    labels = c("District Cooling (kWh/m2)" = "Cooling",
                               "District Heating (kWh/m2)" = "Heating",
                               "Sum (kWh/m2)" = "Total")) +
  scale_x_discrete(labels = c("Adobe brick" = "Adobe Brick",
                              "Aerated concrete" = "Aerated Concrete")) +
  scale_y_continuous(breaks = seq(0, max(data_long$Value, na.rm = TRUE), by = 10)) +  # Adjust breaks for more numbers
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, face = "bold"),  # Adjust y-axis label position
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

# ================================================
# Visualize - Annual Energy Demands (kWh)
# ================================================

# Create a new column for the sum of cooling and heating
AnnualEnergyConsumptions <- AnnualEnergyConsumptions %>%
  mutate(`Sum (kWh)` = `District Cooling (kWh)` + `District Heating (kWh)`)

# Calculate the total energy consumption for each material
total_energy_kWh <- AnnualEnergyConsumptions %>%
  group_by(Materials) %>%
  summarise(Total = sum(`Sum (kWh)`, na.rm = TRUE)) %>%
  arrange(desc(Total))

# Reorder the Materials factor based on total energy consumption
AnnualEnergyConsumptions <- AnnualEnergyConsumptions %>%
  mutate(Materials = factor(Materials, levels = total_energy_kWh$Materials))

# Reshape the data to long format for ggplot2
data_long2 <- AnnualEnergyConsumptions %>%
  select(Materials, `District Cooling (kWh)`, `District Heating (kWh)`, `Sum (kWh)`) %>%
  pivot_longer(cols = starts_with("District") | starts_with("Sum"), 
               names_to = "Type", values_to = "Value")

# Determine maximum y-axis limit
max_y <- max(data_long2$Value, na.rm = TRUE)

# Determine breaks for y-axis
y_breaks <- seq(0, max_y, by = 20000)
# Add 180000 if it's greater than the current maximum
if (180000 > max_y) {
  y_breaks <- c(y_breaks, 180000)
}

# Create the bar plot with values displayed on the bars
ggplot(data_long2, aes(x = Materials, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.25) +
  geom_text(aes(label = round(Value, 1), color = "grayish"),  # Set color to grayish
            position = position_dodge(width = 0.8), vjust = -0.8, hjust = 1, size = 3, angle = 90) +  # Rotate by 90 degrees
  scale_color_manual(values = c(grayish = "#666666"), guide = FALSE) +  # Disable legend for grayish color
  labs(title = "Annual Energy Consumption per Material",
       y = "Energy Consumption (kWh/m2)",
       x = "Infill Materials") +
  scale_fill_manual(values = c("District Cooling (kWh)" = "#03738C",
                               "District Heating (kWh)" = "#F27405",
                               "Sum (kWh)" = "#F5C464"),
                    name = "Activity",
                    labels = c("District Cooling (kWh)" = "Cooling",
                               "District Heating (kWh)" = "Heating",
                               "Sum (kWh)" = "Total")) +
  scale_x_discrete(labels = c("Adobe brick" = "Adobe Brick",
                              "Aerated concrete" = "Aerated Concrete")) +
  scale_y_continuous(breaks = y_breaks,  # Use the custom breaks
                     expand = c(0, 0),
                     limits = c(0, max_y * 1.1)) +  # Set y-axis limits to accommodate highest value
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, face = "bold"),  # Adjust y-axis label position
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +  # Set background to white
  guides(fill = guide_legend(keywidth = 1, keyheight = 1)) +
  theme(legend.position = "right")  # Move legend to the right
