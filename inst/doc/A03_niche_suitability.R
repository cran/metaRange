## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----create_landscape---------------------------------------------------------
library(metaRange)
library(terra)
set_verbosity(2)

# find the example raster file
raster_file <- system.file("ex/elev.tif", package = "terra")

# load it
r <- rast(raster_file)

# adjust the values
temperature <- scale(r, center = FALSE, scale = TRUE) * 10 + 273.15
precipitation <- r * 2

## -----------------------------------------------------------------------------
temperature <- rep(temperature, 10)
precipitation <- rep(precipitation, 10)
landscape <- sds(temperature, precipitation)
names(landscape) <- c("temperature", "precipitation")

## ----landscape_01, fig.cap = "Figure 1: The temperature of the example landscape. Only the first layer of 10 identical ones is shown."----
terra::plot(
    landscape$temperature[[1]],
    col = hcl.colors(100, "RdYlBu", rev = TRUE),
    main = "Temperature [K]"
)

## ----landscape_02, fig.cap = "Figure 2: The precipitation of the example landscape. Only the first layer of 10 identical ones is shown."----
terra::plot(
    landscape$precipitation[[1]],
    col = hcl.colors(100, "Earth"),
    main = "Precipitation [mm]"
)

## ----add_species--------------------------------------------------------------
sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)
sim$add_species(name = "species_1")
sim$add_species(name = "species_2")

## ----add_trait----------------------------------------------------------------
sim$add_traits(
    species = c("species_1", "species_2"),
    population_level = TRUE,
    abundance = 500,
    climate_suitability = 1,
    reproduction_rate = 0.3,
    carrying_capacity = 1000
)

## ----add_trait_3--------------------------------------------------------------
sim$add_traits(
    species = "species_1",
    population_level = FALSE,
    max_temperature = 300,     # Kelvin
    optimal_temperature = 288, # Kelvin
    min_temperature = 280,     # Kelvin
    max_precipitation = 1000,    # mm
    optimal_precipitation = 700, # mm
    min_precipitation = 200      # mm
)
sim$add_traits(
    species = "species_2",
    population_level = FALSE,
    max_temperature = 290,
    optimal_temperature = 285,
    min_temperature = 270,
    max_precipitation = 1000,
    optimal_precipitation = 500,
    min_precipitation = 0
)

## ----calculate_suitability, fig.cap="Figure 3: Example suitability curve for the temperature niche of species 2."----
min_value <- 270
opt_value <- 285
max_value <- 290
x <- seq(min_value, max_value, length.out = 100)
y <- calculate_suitability(max_value, opt_value, min_value, x)
plot(x, y, type = "l", xlab = "Temperature [K]", ylab = "Suitability")

## ----Suitability--------------------------------------------------------------
sim$add_process(
    species = c("species_1", "species_2"),
    process_name = "calculate_suitability",
    process_fun = function() {
        self$traits$climate_suitability <-
            calculate_suitability(
                self$traits$max_temperature,
                self$traits$optimal_temperature,
                self$traits$min_temperature,
                self$sim$environment$current$temperature
            ) *
            calculate_suitability(
                self$traits$max_precipitation,
                self$traits$optimal_precipitation,
                self$traits$min_precipitation,
                self$sim$environment$current$precipitation
            )
    },
    execution_priority = 1
)

## ----Reproduction-------------------------------------------------------------
sim$add_process(
    species = c("species_1", "species_2"),
    process_name = "reproduction",
    process_fun = function() {
        self$traits$abundance <-
            ricker_reproduction_model(
                self$traits$abundance,
                self$traits$reproduction_rate * self$traits$climate_suitability,
                self$traits$carrying_capacity * self$traits$climate_suitability
            )
    },
    execution_priority = 2
)

## ----run_simulation-----------------------------------------------------------
set_verbosity(1)
sim$begin()

## ----resA, fig.cap = "Figure 6: The resulting abundance distribution of species 1 after 10 simulation time steps."----
# define a nice color palette
plot_cols <- hcl.colors(100, "Purple-Yellow", rev = TRUE)
plot(
    sim,
    obj = "species_1",
    name = "abundance",
    main = "Species 1: abundance",
    col = plot_cols
)

## ----resB, fig.cap = "Figure 7: The resulting abundance distribution of species 2 after 10 simulation time steps."----
plot(
    sim$species_2,
    trait = "abundance",
    main = "Species 2: abundance",
    col = plot_cols
)

