## Phase diagram
phases_triangle <- read.table("data-raw/phases.csv", header = TRUE, sep = ",", dec = ".")
phases_chart <- phases_triangle$chart
.phases <- split(phases_triangle[, -1], f = phases_chart)

## Soil texture
soil_triangle <- read.table("data-raw/soil.csv", header = TRUE, sep = ",", dec = ".")
soil_chart <- soil_triangle$chart
.soil <- split(soil_triangle[, -1], f = soil_chart)

usethis::use_data(.soil, .phases, internal = TRUE, overwrite = TRUE)
