## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(metaRange)
library(terra)
set_verbosity(0)

raster_file <- system.file("ex/elev.tif", package = "terra")
r <- rast(raster_file)
temperature <- scale(r, center = FALSE, scale = TRUE) * 10 + 273.15
precipitation <- r * 2
temperature <- rep(temperature, 10)
precipitation <- rep(precipitation, 10)
landscape <- sds(temperature, precipitation)
names(landscape) <- c("temperature", "precipitation")

sim <- create_simulation(landscape)
sim$add_species(name = "species_1")

## ----define_traits------------------------------------------------------------
sim$add_traits(
    species = "species_1",
    population_level = FALSE,
    temperature_maximum = 300,
    temperature_optimum = 288,
    temperature_minimum = 280
)

## ----global-------------------------------------------------------------------
sim$add_globals(
    "E_reproduction_rate" = -0.65,
    "E_carrying_capacity" = 0.65,
    "exponent_reproduction_rate" = -1 / 4,
    "exponent_carrying_capacity" = -3 / 4,
    "k" = 8.617333e-05
)

## ----add_rep_traits-----------------------------------------------------------
sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    "abundance" = 100,
    "reproduction_rate" = 0.5,
    "carrying_capacity" = 1000,
    "mass" = 1
)

## ----add_more_traits----------------------------------------------------------
sim$add_traits(
    species = "species_1",
    population_level = FALSE,
    "reproduction_rate_mte_constant" = calculate_normalization_constant(
        parameter_value = sim$species_1$traits[["reproduction_rate"]][[1]],
        scaling_exponent = sim$globals[["exponent_reproduction_rate"]],
        mass = sim$species_1$traits[["mass"]][[1]],
        reference_temperature = sim$species_1$traits[["temperature_optimum"]],
        E = sim$globals[["E_reproduction_rate"]],
        k = sim$globals[["k"]]
    ),
    "carrying_capacity_mte_constant" = calculate_normalization_constant(
        parameter_value = sim$species_1$traits[["carrying_capacity"]][[1]],
        scaling_exponent = sim$globals[["exponent_carrying_capacity"]],
        mass = sim$species_1$traits[["mass"]][[1]],
        reference_temperature = sim$species_1$traits[["temperature_optimum"]],
        E = sim$globals[["E_carrying_capacity"]],
        k = sim$globals[["k"]]
    )
)

## ----add_processes------------------------------------------------------------
sim$add_process(
    species = "species_1",
    process_name = "mte",
    process_fun = function() {
        self$traits[["reproduction_rate"]] <- metabolic_scaling(
            normalization_constant = self$traits[["reproduction_rate_mte_constant"]],
            scaling_exponent = self$sim$globals[["exponent_reproduction_rate"]],
            mass = self$traits[["mass"]],
            temperature = self$sim$environment$current[["temperature"]],
            E = self$sim$globals[["E_reproduction_rate"]],
            k = self$sim$globals[["k"]]
        )

        self$traits[["carrying_capacity"]] <- metabolic_scaling(
            normalization_constant = self$traits[["carrying_capacity_mte_constant"]],
            scaling_exponent = self$sim$globals[["exponent_carrying_capacity"]],
            mass = self$traits[["mass"]],
            temperature = self$sim$environment$current[["temperature"]],
            E = self$sim$globals[["E_carrying_capacity"]],
            k = self$sim$globals[["k"]]
        )
    },
    execution_priority = 2
)

## ----add_processes2, fig.show="hold"------------------------------------------
sim$set_time_layer_mapping(c(1, 2))
sim$begin()
plot_cols <- hcl.colors(100, "Purple-Yellow", rev = TRUE)
plot(sim, "species_1", "reproduction_rate", col = plot_cols, main = "Reproduction rate")
plot(sim, "species_1", "carrying_capacity", col = plot_cols, main = "Carrying capacity")

