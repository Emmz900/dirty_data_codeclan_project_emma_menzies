library(readxl)
library(tidyverse)

seabirds_ships <- read_xls("raw_data/seabirds.xls")
seabirds_birds <- read_xls("raw_data/seabirds.xls", sheet = 2)
seabirds_ship_key <- read_xls("raw_data/seabirds.xls", sheet = 3, skip = 1)
seabirds_bird_key <- read_xls("raw_data/seabirds.xls", sheet = 4, skip = 1)

seabirds_joined <- full_join(seabirds_ships, seabirds_birds, by = "RECORD ID")
names(seabirds_joined)
