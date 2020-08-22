library(ggdendro)
library(dplyr)
library(tibble)
library(tidyr)
library(here)

# Define dataset
df = USArrests

# Perform clustering & extract data with dendro_data()
hc = hclust(dist(df), "average")
hcdata = dendro_data(hc, type = "rectangle")

segments = hcdata$segments
segments = segments[,c("xend", "yend")]


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
  data.frame(cutree(hc, k = 4))) # Choose number of clusters

names(cuts) = c("ID", "Cluster")
                
segments = segments %>%
  left_join(cuts, by = c("label" = "ID"))

# Sort by xend and Cluster, then fill down Cluster
segments = segments %>%
  arrange(xend, Cluster) %>%
  fill(Cluster)

# Write dendrogram data
write.csv(segments, paste(here(), 'Dendrogram_data.csv', sep = '/'), row.names = F, na = "")

# Attach clusters to original data and write out
df = rownames_to_column(df)
names(df)[1] = "label"

df = df %>%
  left_join(cuts, by = c("label" = "ID"))

write.csv(df, paste(here(), 'USCrimesData_Clustered.csv', sep = '/'), row.names = F, na = "")

