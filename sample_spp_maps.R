### sample a few species
library(tidyverse)
library(here)

### how many spp to sample per taxon?  4 maps per spp, ~250k per map
n_spp_per_taxon <- 10

### list source maps and put into dataframe by species and taxon
spp_maps <- list.files(here('lawler_data/species_projections'), pattern = '.tif',
                       full.names = FALSE, recursive = TRUE)
spp_df <- data.frame(f = spp_maps,
                     spp = basename(spp_maps) %>%
                       str_remove('(_gf85|_in85|_mc85|_bias).+') %>%
                       str_replace_all('_', ' '),
                     tax = basename(dirname(spp_maps)))

### sample species per taxon and grab just those maps
set.seed(42)
spp_sample_df <- spp_df %>%
  select(-f) %>%
  distinct() %>%
  group_by(tax) %>%
  slice_sample(n = n_spp_per_taxon, replace = FALSE)

spp_sample_fs <- spp_df %>%
  filter(spp %in% spp_sample_df$spp)

### Identify directory structure in source folder
dir_structure <- spp_sample_fs %>%
  mutate(dir1 = dirname(f),
         dir2 = dirname(dir1),
         dir3 = dirname(dir2)) %>%
  select(starts_with('dir')) %>%
  distinct()

### if necessary, set up directory structure in destination folder
if(any(!file.exists(here('lawler_spp_sample', dir_structure$dir1)))) {
  x <- lapply(here('lawler_spp_sample', unique(dir_structure$dir3)), dir.create)
  y <- lapply(here('lawler_spp_sample', unique(dir_structure$dir2)), dir.create)
  z <- lapply(here('lawler_spp_sample', unique(dir_structure$dir1)), dir.create)
}

### copy the files
file.copy(from = file.path(here('lawler_data/species_projections'), spp_sample_fs$f),
          to   = file.path(here('lawler_spp_sample'), spp_sample_fs$f),
          recursive = TRUE)

### check total file size
sample_fs <- list.files(here('lawler_spp_sample'), full.names = TRUE, recursive = TRUE)
sum(file.size(sample_fs)) / 1e6 ### report value in MB
