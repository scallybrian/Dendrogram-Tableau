library(ggdendro)
library(dplyr)
library(tibble)
library(tidyr)

# Define dataset
df = USArrests

# Perform clustering & extract data with dendro_data()
hc = hclust(dist(df), "ward.D")
hcdata = dendro_data(hc, type = "rectangle")

segments = hcdata$segments

# Vertical path
segments = segments %>%
  arrange(xend) %>%
  distinct(xend) %>%
  mutate(vertical_path = row_number()) %>%
  left_join(segments, by = "xend")

# Horz path
segments = segments %>%
  arrange(yend) %>%
  distinct(yend) %>%
  mutate(horz_path = row_number()) %>%
  left_join(segments, by = "yend")

# Add labels
hc_labels = hcdata$labels
segments = segments %>%
  left_join(hc_labels, by = c("xend" = "x", "yend" = "y"))


# Add cluster Cluster labels
cuts = rownames_to_column(
  data.frame(cutree(hc, k = 4)))

names(cuts) = c("ID", "Cluster")
                
segments = segments %>%
  left_join(cuts, by = c("label" = "ID"))

# Sort by xend and Cluster, then fill down Cluster
segments = segments %>%
  arrange(xend, Cluster) %>%
  fill(Cluster)
