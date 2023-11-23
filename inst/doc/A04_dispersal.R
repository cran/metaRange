## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----kernel-------------------------------------------------------------------
max_dispersal_distance = 10
mean_dispersal_distance = 7
dispersal_kernel = metaRange::calculate_dispersal_kernel(
    max_dispersal_dist = max_dispersal_distance, # units: grid cells
    kfun = metaRange::negative_exponential_function, # this function calculates the kernel values
    mean_dispersal_dist = mean_dispersal_distance # this is passed to the kfun
)

## ----plot_kernel, fig.cap="Figure 1: Example dispersal kernel for a negative exponential function"----
terra::plot(
    terra::rast(
        dispersal_kernel,
        extent = terra::ext(
            -max_dispersal_distance, max_dispersal_distance,
            -max_dispersal_distance, max_dispersal_distance
        )
    ),
    main = "Dispersal probability"
)

## ----create_landscape---------------------------------------------------------
library(metaRange)
library(terra)
set_verbosity(2)

raster_file <- system.file("ex/elev.tif", package = "terra")
r <- rast(raster_file)
temperature <- scale(r, center = FALSE, scale = TRUE) * 10 + 273.15
temperature <- rep(temperature, 10)
landscape <- sds(temperature)
names(landscape) <- c("temperature")

## ----setup_sim----------------------------------------------------------------
sim <- create_simulation(landscape)
sim$add_species("species_1")
sim$add_species("species_2")
sim$add_traits(
    species = c("species_1", "species_2"),
    abundance = 100,
    climate_suitability = 1,
    reproduction_rate = 0.3,
    carrying_capacity = 1000
)
sim$add_traits(
    species = c("species_1", "species_2"),
    population_level = FALSE,
    max_temperature = 300,
    optimal_temperature = 288,
    min_temperature = 280,
    dispersal_kernel = calculate_dispersal_kernel(
        max_dispersal_dist = 7,
        kfun = negative_exponential_function,
        mean_dispersal_dist = 4
    )
)
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
            )
    },
    execution_priority = 1
)
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

## ----Dispersal1---------------------------------------------------------------
sim$add_process(
    species = "species_1",
    process_name = "dispersal_process",
    process_fun = function() {
        self$traits[["abundance"]] <- dispersal(
            abundance = self$traits[["abundance"]],
            dispersal_kernel = self$traits[["dispersal_kernel"]]
        )
    },
    execution_priority = 3
)

## ----Dispersal2---------------------------------------------------------------
sim$add_process(
    species = "species_2",
    process_name = "dispersal_process",
    process_fun = function() {
        self$traits[["abundance"]] <- dispersal(
            abundance = self$traits[["abundance"]],
            dispersal_kernel = self$traits[["dispersal_kernel"]],
            weights = self$traits[["climate_suitability"]]
        )
    },
    execution_priority = 3
)

## ----run_sim, fig.cap="Figure 2: Resulting abundance of species 1 (with unweighted dispersal) after 10 time steps."----
set_verbosity(0)
sim$begin()
plot_cols <- hcl.colors(100, "Purple-Yellow", rev = TRUE)
plot(sim, "species_1", "abundance", col = plot_cols)

## ----run_sim2, fig.cap="Figure 3: Resulting abundance of species 2 (with weighted dispersal) after 10 time steps."----
plot(sim, "species_2", "abundance", col = plot_cols)

