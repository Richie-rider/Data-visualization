


# Sankey diagram
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggalluvial)

# Load the datasets
sc_data <- read.csv("~/DEG_scRNA_seq/All_cell type_DEG/All_cell type_DEGs.csv")
bulk_data <- read.csv("~/bulk_RNA_sig_genes_DESeq2.csv")

# Assume the bulk and scRNA-seq datasets have the column 'gene_id' for merging
# and 'Direction' columns named appropriately ('Direction_bulk' and 'Direction_sc')
combined_data <- merge(sc_data, bulk_data, by = "gene_id", all.x = TRUE)


# Structure the data for the Sankey diagram
sankey_data <- combined_data %>%
  select(gene_id, Direction_bulk, Direction_sc, Cell_type) %>%
  group_by(Direction_bulk, Direction_sc, Cell_type) %>%
  summarise(Count = n())

sankey_data <- na.omit(sankey_data)


ggplot(sankey_data, aes(axis1 = Direction_bulk, axis2 = Direction_sc, axis3 = Cell_type, y = Count)) +
  geom_alluvium(aes(fill = Direction_sc)) +
  scale_fill_manual(values = c("Down" = "blue", "Up" = "red")) +  # Assign colors
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 7) +  # Set label size
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    axis.text.x = element_blank(),  # Hide the original x-axis text
    axis.ticks.x = element_blank(),  # Hide the original x-axis ticks
    axis.line.x = element_blank(),  # Hide the original x-axis ticks
    axis.line = element_line(color = "black", size = 0.5),  # Set axis lines
    axis.ticks = element_line(color = "black", size = 0.5),  # Set tick marks
    axis.ticks.length = unit(0.2, "cm"),  # Set tick length
    axis.text.y = element_text(color = "black"),  # Set y-axis text color
    axis.ticks.y = element_line(color = "black"),  # Set y-axis tick color
    legend.position = "bottom"  # Move legend to bottom
  ) +
  ylab('Number of DEGs') +
  ggtitle("DEGs in Bulk RNA-seq and scRNA-seq by Cell Type") +
  annotate("text", x = 1, y = -100, label = "Bulk RNA-seq", size = 7, hjust = 0.5) +  # Add custom x-axis labels
  annotate("text", x = 2, y = -100, label = "scRNA-seq", size = 7, hjust = 0.5) +
  annotate("text", x = 3, y = -100, label = "Cell types", size = 7, hjust = 0.5)


