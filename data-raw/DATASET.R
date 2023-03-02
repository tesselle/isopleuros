## Data from Aitchison 1986
arctic <- read.table("data-raw/arctic.csv", header = TRUE, sep = ",", dec = ".",
                   row.names = 1)
usethis::use_data(arctic, internal = FALSE, overwrite = TRUE)

lava <- read.table("data-raw/lava.csv", header = TRUE, sep = ",", dec = ".",
                   row.names = 1)
usethis::use_data(lava, internal = FALSE, overwrite = TRUE)

boxite <- read.table("data-raw/boxite.csv", header = TRUE, sep = ",", dec = ".",
                   row.names = 1)
usethis::use_data(boxite, internal = FALSE, overwrite = TRUE)

# Internal =====================================================================
## Phase diagram
phases_triangle <- read.table("data-raw/phases.csv", header = TRUE, sep = ",", dec = ".")
phases_chart <- phases_triangle$chart
.phases <- split(phases_triangle[, -1], f = phases_chart)

## Soil texture
soil_triangle <- read.table("data-raw/soil.csv", header = TRUE, sep = ",", dec = ".")
soil_chart <- soil_triangle$chart
.soil <- split(soil_triangle[, -1], f = soil_chart)

usethis::use_data(.soil, .phases, internal = TRUE, overwrite = TRUE)
