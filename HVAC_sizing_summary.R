
install.packages("readxl")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")

library("readxl")
library("ggplot2")
library("dplyr")
library("tidyr")

# ================================================
# HVAC Sizing Summary - Zone Evaluations
# ================================================

# read xlsx files

HVACSizingSummary <- read_excel("HVAC_Sizing_Summary.xlsx")
glimpse(HVACSizingSummary)

# ================================================
# Visualize - "Zone Sensible Cooling - User Design Load per Area (W/m2)" by number of floor and orientation.
# ================================================

# Filter the dataset for "Hollow Clay Tile"
hollow_clay_tile_data <- HVACSizingSummary %>%
  filter(Material == "Hollow Clay Tile")

# Convert 'Thermal Zone Orientation' to a factor with specified levels
hollow_clay_tile_data <- hollow_clay_tile_data %>%
  mutate(`Thermal Zone Orientation` = factor(`Thermal Zone Orientation`, levels = c("North", "East", "South", "West")))

# Create custom color palette
custom_colors <- c("North" = "#03738C", "East" = "#F5C464", "South" = "#F27405", "West" = "#666666")

# Create custom labels for the Thermal Zone Floor
custom_labels <- c("1" = "1st Floor", "2" = "2nd Floor", "3" = "3rd Floor", "4" = "4th Floor", "5" = "5th Floor", "6" = "6th Floor")

# Create the bar plot with values displayed on the bars
ggplot(hollow_clay_tile_data, aes(x = factor(`Thermal Zone Floor`), 
                                  y = `Zone Sensible Cooling - User Design Load per Area (W/m2)`, 
                                  fill = `Thermal Zone Orientation`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.4) +
  geom_text(aes(label = round(`Zone Sensible Cooling - User Design Load per Area (W/m2)`, 1)), 
            position = position_dodge(width = 0.8), vjust = 0.3, hjust = -0.1, size = 3, angle = 90, color = "#666666") +
  scale_fill_manual(values = custom_colors, name = "Thermal Zone (Flat) Orientation") +
  labs(title = "Zone Sensible Cooling Load per Area for Hollow Clay Tile",
       x = "Thermal Zone (Flat) Floor",
       y = "Sensible Cooling Load (W/m2)") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(hollow_clay_tile_data$`Zone Sensible Cooling - User Design Load per Area (W/m2)`) * 1.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1)) +
  theme(legend.position = "right")

# ================================================
# Visualize - "Zone Sensible Heating - User Design Load per Area (W/m2)" by number of floor and orientation.
# ================================================

# Create the bar plot with values displayed on the bars
ggplot(hollow_clay_tile_data, aes(x = factor(`Thermal Zone Floor`), 
                                  y = `Zone Sensible Heating - User Design Load per Area (W/m2)`, 
                                  fill = `Thermal Zone Orientation`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.4) +
  geom_text(aes(label = round(`Zone Sensible Heating - User Design Load per Area (W/m2)`, 1)), 
            position = position_dodge(width = 0.8), vjust = 0.3, hjust = -0.1, size = 3, angle = 90, color = "#666666") +
  scale_fill_manual(values = custom_colors, name = "Thermal Zone Orientation") +
  labs(title = "Zone Sensible Heating Load per Area for Hollow Clay Tile",
       x = "Thermal Zone Floor",
       y = "Sensible Heating Load (W/m2)") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(hollow_clay_tile_data$`Zone Sensible Heating - User Design Load per Area (W/m2)`) * 1.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1)) +
  theme(legend.position = "right")

# ================================================
# Visualize - "Zone Sensible Cooling - User Design Load per Area (W/m2)" for 6th floor and all the materials.
# ================================================

# Filter the dataset for the 6th floor
sixth_floor_data_cooling <- HVACSizingSummary %>%
  filter(`Thermal Zone Floor` == 6)

# Filter for only the thermal zone orientation with the maximum load value for each material
max_load_per_material_cooling <- sixth_floor_data_cooling %>%
  group_by(Material) %>%
  top_n(1, `Zone Sensible Cooling - User Design Load per Area (W/m2)`) %>%
  ungroup()

# Reorder materials from highest to lowest total load
max_load_per_material_cooling <- max_load_per_material_cooling %>%
  mutate(Material = reorder(Material, -`Zone Sensible Cooling - User Design Load per Area (W/m2)`))

# Create the bar plot
ggplot(max_load_per_material_cooling, aes(x = Material, 
                                          y = `Zone Sensible Cooling - User Design Load per Area (W/m2)`, 
                                          fill = Material)) +
  geom_bar(stat = "identity", width = 0.3, fill = "#487F8F") +
  geom_text(aes(label = round(`Zone Sensible Cooling - User Design Load per Area (W/m2)`, 1)), 
            vjust = -0.3, hjust = 0.5, size = 3, color = "#666666") +
  labs(title = "Max Zone Sensible Cooling Load per Area for 6th Floor by Material",
       x = "Infill Material",
       y = "Max Sensible Cooling Load (W/m2)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(max_load_per_material_cooling$`Zone Sensible Cooling - User Design Load per Area (W/m2)`) * 1.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +
  guides(fill = FALSE) +
  theme(legend.position = "none")

# ================================================
# Visualize - "Zone Sensible Cooling - User Design Load per Area (W/m2)" for 1st floor and all the materials.
# ================================================

# Filter the dataset for the 1st floor
first_floor_data_cooling <- HVACSizingSummary %>%
  filter(`Thermal Zone Floor` == 1)

# Filter for only the thermal zone orientation with the maximum load value for each material
max_load_per_material_cooling_1st <- first_floor_data_cooling %>%
  group_by(Material) %>%
  top_n(1, `Zone Sensible Cooling - User Design Load per Area (W/m2)`) %>%
  ungroup()

# Reorder materials from highest to lowest total load
max_load_per_material_cooling_1st <- max_load_per_material_cooling_1st %>%
  mutate(Material = reorder(Material, -`Zone Sensible Cooling - User Design Load per Area (W/m2)`))

# Create the bar plot
ggplot(max_load_per_material_cooling_1st, aes(x = Material, 
                                              y = `Zone Sensible Cooling - User Design Load per Area (W/m2)`, 
                                              fill = Material)) +
  geom_bar(stat = "identity", width = 0.3, fill = "#487F8F") +
  geom_text(aes(label = round(`Zone Sensible Cooling - User Design Load per Area (W/m2)`, 1)), 
            vjust = -0.3, hjust = 0.5, size = 3, color = "#666666") +
  labs(title = "Max Zone Sensible Cooling Load per Area for 1st Floor by Material",
       x = "Infill Material",
       y = "Max Sensible Cooling Load (W/m2)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(max_load_per_material_cooling_1st$`Zone Sensible Cooling - User Design Load per Area (W/m2)`) * 1.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +
  guides(fill = FALSE) +
  theme(legend.position = "none")

# ================================================
# Visualize - "Zone Sensible Heating - User Design Load per Area (W/m2)" for 6th floor and all the materials.
# ================================================

# Filter the dataset for the 6th floor
sixth_floor_data_heating <- HVACSizingSummary %>%
  filter(`Thermal Zone Floor` == 6)

# Filter for only the thermal zone orientation with the maximum load value for each material
max_load_per_material_heating <- sixth_floor_data_heating %>%
  group_by(Material) %>%
  top_n(1, `Zone Sensible Heating - User Design Load per Area (W/m2)`) %>%
  ungroup()

# Reorder materials from highest to lowest total load
max_load_per_material_heating <- max_load_per_material_heating %>%
  mutate(Material = reorder(Material, -`Zone Sensible Heating - User Design Load per Area (W/m2)`))

# Create the bar plot
ggplot(max_load_per_material_heating, aes(x = Material, 
                                          y = `Zone Sensible Heating - User Design Load per Area (W/m2)`, 
                                          fill = Material)) +
  geom_bar(stat = "identity", width = 0.3, fill = "#D77E2F") +
  geom_text(aes(label = round(`Zone Sensible Heating - User Design Load per Area (W/m2)`, 1)), 
            vjust = -0.3, hjust = 0.5, size = 3, color = "#666666") +
  labs(title = "Max Zone Sensible Heating Load per Area for 6th Floor by Material",
       x = "Infill Material",
       y = "Max Sensible Heating Load (W/m2)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(max_load_per_material_heating$`Zone Sensible Heating - User Design Load per Area (W/m2)`) * 1.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +
  guides(fill = FALSE) +
  theme(legend.position = "none")

# ================================================
# Visualize - "Zone Sensible Heating - User Design Load per Area (W/m2)" for 1st floor and all the materials.
# ================================================

# Filter the dataset for the 1st floor
first_floor_data_heating <- HVACSizingSummary %>%
  filter(`Thermal Zone Floor` == 1)

# Filter for only the thermal zone orientation with the maximum load value for each material
max_load_per_material_heating_1st <- first_floor_data_heating %>%
  group_by(Material) %>%
  top_n(1, `Zone Sensible Heating - User Design Load per Area (W/m2)`) %>%
  ungroup()

# Reorder materials from highest to lowest total load
max_load_per_material_heating_1st <- max_load_per_material_heating_1st %>%
  mutate(Material = reorder(Material, -`Zone Sensible Heating - User Design Load per Area (W/m2)`))

# Create the bar plot
ggplot(max_load_per_material_heating_1st, aes(x = Material, 
                                              y = `Zone Sensible Heating - User Design Load per Area (W/m2)`, 
                                              fill = Material)) +
  geom_bar(stat = "identity", width = 0.3, fill = "#D77E2F") +
  geom_text(aes(label = round(`Zone Sensible Heating - User Design Load per Area (W/m2)`, 1)), 
            vjust = -0.3, hjust = 0.5, size = 3, color = "#666666") +
  labs(title = "Max Zone Sensible Heating Load per Area for 1st Floor by Material",
       x = "Infill Material",
       y = "Max Sensible Heating Load (W/m2)") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(max_load_per_material_heating_1st$`Zone Sensible Heating - User Design Load per Area (W/m2)`) * 1.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust = 4, hjust = 0),
        axis.title.x = element_text(size = 10, vjust = 0, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white")) +
  guides(fill = FALSE) +
  theme(legend.position = "none")