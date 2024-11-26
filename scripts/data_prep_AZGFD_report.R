# Convert CMR data to AZGFD report
# EKB

# LIBRARIES and DATA
library(tidyverse)
data <- read_csv("data_raw/mark_recapture_data.csv")

# Create Scientific Name, Common Name, and Date columns
data <- data %>% 
  fill(day) %>% 
  mutate(ScientificName = case_when(species == "SH" ~ "Sigmodon hispidus",
                                    species == "DM" ~ "Dipodomys merriami",
                                    species == "PE" ~ "Peromyscus eremicus",
                                    species == "PP" ~ "Chaetodipus penicillatus",
                                    species == "RF" ~ "Reithrodontomys fulvescens"),
         CommonName = case_when(species == "SH" ~ "hispid cotton rat",
                                species == "DM" ~ "Merriam's kangaroo rat",
                                species == "PE" ~ "cactus mouse",
                                species == "PP" ~ "desert pocket mouse",
                                species == "RF" ~ "fulvous harvest mouse"),
         Date = make_date(year, month, day))

# Summarize Data
data_counts <- data %>% 
  group_by(Date, site, species, ScientificName, CommonName, sex, juvenile) %>% 
  count() %>%
  mutate(Lat = if_else(site == "DRX", "32.148099","32.204614"),
         Long = if_else(site == "DRX", "-110.994286" ,"-110.989142"),
         Sex = if_else(is.na(sex), "U", sex),
         Lifestage = if_else(is.na(juvenile), "Adult", "Juvenile"))

# save output
write_csv(data_counts, "data_clean/AZGFD_report_2024.csv")
