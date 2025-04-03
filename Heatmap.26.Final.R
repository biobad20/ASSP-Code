# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggnewscale)

# Load and clean data
file_path <- "Ecolog_Data (1).xlsx"
data <- read_excel(file_path, sheet = "Ark1", skip = 1)
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Select needed columns
data_means <- data %>%
  select(Compound, substrate.group, contains("mean"))

# Ensure `substrate.group` is included from the beginning
heatmap_data <- data_means %>% select(Compound, substrate.group, all_of(selected_cols))

# Remove rows where Compound or substrate.group is NA
heatmap_data <- heatmap_data %>% drop_na(Compound, substrate.group)

# Reshape the data from wide to long format
heatmap_melted <- heatmap_data %>%
  pivot_longer(cols = -c(Compound, substrate.group), names_to = "Condition", values_to = "Value") %>%
  drop_na()  # Remove any remaining NA values

# Verify that substrate.group exists
print(head(heatmap_melted))  # Check the first few rows

# Order by substrate.group
heatmap_melted <- heatmap_melted %>%
  arrange(substrate.group, Compound)  # Ensure grouping

# Rename x-axis labels
new_labels <- c(
  "Con..Soil.mean.5" = "Adapted Soil",
  "Pri..Soil.mean.5" = "Pristine Soil",
  "New.soil.mean.5" = "New Soil",
  "Cryco.mean.5" = "Cryoconite",
  "sub.gla.mean.5" = "Subglacial",
  "Sea.Ice.bottom.mean.5" = "Sea Ice Bottom",
  "Sea.Ice.top.mean.5" = "Sea Ice Top",
  "int..Tidal.mean.5" = "Intertidal"
)

# Create the heatmap with ordered compounds
ggplot(heatmap_melted, aes(x = Condition, y = reorder(Compound, as.numeric(factor(substrate.group))), fill = Value)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "purple") +
  theme_minimal() +
  labs(title = "3/26 Ecolog Observations by Sample and Substrate",
       x = "Sample",
       y = "Substrate") +
  scale_x_discrete(labels = new_labels) +  # Rename x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(color = "black"),  # Make y-axis labels black
    axis.title.x = element_text(face = "bold", size = 14),  # Bold x-axis title
    axis.title.y = element_text(face = "bold", size = 14)   # Bold y-axis title
    )

