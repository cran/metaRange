## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup, fig.cap = "Figure 1: The habitat quality of the example landscape. Note: higher value = better habitat quality"----
library(metaRange)
library(terra)

# find the file
raster_file <- system.file("ex/elev.tif", package = "terra")

# load it
r <- rast(raster_file)

# scale it
r <- scale(r, center = FALSE, scale = TRUE)

r <- rep(r, 10)
landscape <- sds(r)
names(landscape) <- c("habitat_quality")

# plot the first layer of the landscape
plot(landscape[["habitat_quality"]][[1]], main = "Habitat quality")

## ----create_simulation--------------------------------------------------------
sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)

## ----add_species--------------------------------------------------------------
sim$add_species(c("species_1", "species_2"))

## ----species_names------------------------------------------------------------
sim$species_names()

## ----add_trait----------------------------------------------------------------
sim$add_traits(
    species = c("species_1", "species_2"),
    population_level = TRUE,
    abundance = 200 * sim$environment$current[["habitat_quality"]]
)

## ----initAbundance, fig.cap = "Figure 2: The initial abundance of the species."----
# define a nice color palette
plot_cols <- hcl.colors(100, "BluYl", rev = TRUE)
plot(
    sim,
    obj = "species_1",
    name = "abundance",
    main = "Initial abundance",
    col = plot_cols
)

## ----add_trait_2--------------------------------------------------------------
sim$add_traits(
    species = sim$species_names(),
    population_level = TRUE,
    reproduction_rate = 1.5,
    carrying_capacity = 1000,
    allee_threshold = 150
)

## ----Reproduction-------------------------------------------------------------
sim$add_process(
    species = "species_1",
    process_name = "reproduction",
    process_fun = function() {
        ricker_reproduction_model(
            self$traits$abundance,
            self$traits$reproduction_rate,
            self$traits$carrying_capacity * self$sim$environment$current$habitat_quality
        )
        print(
            paste0(self$name, " mean abundance: ", mean(self$traits$abundance))
        )
    },
    execution_priority = 1
)

## ----Reproduction2------------------------------------------------------------
sim$add_process(
    species = "species_2",
    process_name = "reproduction",
    process_fun = function() {
        self$traits$abundance <-
            ricker_allee_reproduction_model(
                self$traits$abundance,
                self$traits$reproduction_rate,
                self$traits$carrying_capacity * self$sim$environment$current$habitat_quality,
                self$traits$allee_threshold
            )
        print(
            paste0(self$name, " mean abundance: ", mean(self$traits$abundance))
        )
    },
    execution_priority = 1
)

## ----run_simulation-----------------------------------------------------------
set_verbosity(0)
sim$begin()

## ----resA, fig.cap = "Figure 3: The resulting abundance distribution of species 1 after 10 simulation time steps."----
plot(
    sim,
    obj = "species_1",
    name = "abundance",
    main = "Species 1: abundance",
    col = plot_cols
)

## ----resB, fig.cap = "Figure 4: The resulting abundance distribution of species 2 after 10 simulation time steps."----
plot(
    sim,
    obj = "species_2",
    name = "abundance",
    main = "Species 2: abundance",
    col = plot_cols
)

