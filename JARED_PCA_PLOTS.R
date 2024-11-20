
#### EEB313 Project ####

#### calculating diversity per site ####

all_insect_data <- read.csv("arthropod_list.csv")

test_data <- read.csv("final_arthro.csv")

new_test_data <- test_data[-351]

shannon_data <- data.frame(Site = new_test_data$Plot,
           Shannon =  diversity(x = new_test_data[, -1], 
                                index = 'shannon'
           )
)

# Load necessary libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(vegan)
library(plyr)

# Load the data
data <- read.csv("new_merged_data.csv")

# Select numeric columns only
numeric_data <- data %>% select_if(is.numeric)

# Perform PCA
pca_result <- PCA(numeric_data, scale.unit = TRUE, graph = FALSE)

# Extract individual coordinates and add Group information
individual_coords <- as.data.frame(pca_result$ind$coord)
individual_coords$Group <- data$Group  # Add the Group column for grouping

# Extract variable contributions and coordinates
variable_contributions <- as.data.frame(pca_result$var$contrib)
variable_coords <- as.data.frame(pca_result$var$coord)  # Variable coordinates for plotting
variable_contributions <- variable_contributions %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Dim.1 = variable_coords$Dim.1, Dim.2 = variable_coords$Dim.2)

# Save the contribution summary as a CSV file
write.csv(variable_contributions, file = "variable_contributions.csv", row.names = FALSE)

# Display the summary table
print(variable_contribution_summary)

# Biplot 1: All Variables with Axes
# Prepare ranked legends for PC1
# Top contributors for PC1 (Dim.1)
top_contributors_pc1 <- data.frame(
  Rank = 1:5,
  Variable = c("Soil.N..mg.g.", "Soil.water....", "Soil.pH", "Aboveground.biomass..g.m.2.", "Soil.C..mg.g."),
  Contribution = c(12.6, 12.5, 11.4, 10.7, 10.7)
) %>%
  mutate(Legend = paste0(Rank, ". ", Variable, " (", Contribution, "%)"))

# Top contributors for PC2 (Dim.2)
top_contributors_pc2 <- data.frame(
  Rank = 1:5,
  Variable = c("Soil.C..mg.g.", "Plant.density..individuals.m.2.", "Soil.salinity....", "Soil.N..mg.g.", "Leaf.P..mg.g."),
  Contribution = c(20.1, 16.3, 15.4, 14.2, 13.3)
) %>%
  mutate(Legend = paste0(Rank, ". ", Variable, " (", Contribution, "%)"))

# Combine legends into strings
legend_pc1 <- paste(top_contributors_pc1$Legend, collapse = "\n")
legend_pc2 <- paste(top_contributors_pc2$Legend, collapse = "\n")

# Adjust scaling for compactness and clarity
scaling_factor <- 2.3  # Adjust arrow length scaling
text_offset <- 0.5     # Adjust text offset for labels
legend_text_size <- 2.0  # Smaller text for legends
variable_text_size <- 2.5  # Larger text for variable labels
legend_x_adjust <- 0.6  # Factor to move legends closer to the center (reduce value to move left)
legend_pc2_y_adjust <- 0.7  # Adjust vertical position of the bottom legend to move up
legend_title_size <- 2.5  # Size for legend titles

# Plot with clearer layout
ggplot(individual_coords, aes(x = Dim.1, y = Dim.2)) +
  geom_point(alpha = 0.7, color = "gray") +
  geom_segment(data = variable_contributions, 
               aes(x = 0, y = 0, xend = Dim.1 * scaling_factor, yend = Dim.2 * scaling_factor), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "red") +
  geom_text(data = variable_contributions, 
            aes(x = Dim.1 * (scaling_factor + text_offset), 
                y = Dim.2 * (scaling_factor + text_offset), 
                label = Variable), 
            color = "red", 
            size = variable_text_size,  # Larger text for variable labels
            fontface = "bold", 
            hjust = 0.5, vjust = 0.5) +
  # Add "PC1 Variables" title above the top legend
  annotate("text", 
           x = max(individual_coords$Dim.1) * legend_x_adjust, 
           y = max(individual_coords$Dim.2) * 1.1,  # Adjust to position above the legend
           label = "PC1 Variables", 
           color = "blue", 
           size = legend_title_size, 
           fontface = "bold", 
           hjust = 0) +
  # Add the top legend for PC1
  annotate("text", 
           x = max(individual_coords$Dim.1) * legend_x_adjust, 
           y = max(individual_coords$Dim.2) * 0.9, 
           label = legend_pc1, 
           color = "blue", 
           size = legend_text_size, 
           hjust = 0, 
           fontface = "italic") +
  # Add "PC2 Variables" title above the bottom legend
  annotate("text", 
           x = max(individual_coords$Dim.1) * legend_x_adjust, 
           y = min(individual_coords$Dim.2) * legend_pc2_y_adjust + 0.6,  # Move up slightly
           label = "PC2 Variables", 
           color = "darkgreen", 
           size = legend_title_size, 
           fontface = "bold", 
           hjust = 0) +
  # Add the bottom legend for PC2
  annotate("text", 
           x = max(individual_coords$Dim.1) * legend_x_adjust, 
           y = min(individual_coords$Dim.2) * legend_pc2_y_adjust, 
           label = legend_pc2, 
           color = "darkgreen", 
           size = legend_text_size, 
           hjust = 0, 
           fontface = "italic") +
  labs(
    title = "PCA Biplot: Comparing Contributions per Axis",
    x = paste0("PC1 (", round(pca_result$eig[1, 2], 2), "%)"),
    y = paste0("PC2 (", round(pca_result$eig[2, 2], 2), "%)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margins
  )

# Biplot 2: Group Ellipses
ggplot(individual_coords, aes(x = Dim.1, y = Dim.2, color = Group)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "t", level = 0.95) +
  labs(
    title = "PCA Biplot: PC1 vs PC2 with Group Ellipses",
    x = paste0("PC1 (", round(pca_result$eig[1, 2], 2), "%)"),
    y = paste0("PC2 (", round(pca_result$eig[2, 2], 2), "%)")
  ) +
  theme_minimal()



