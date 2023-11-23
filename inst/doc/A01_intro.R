## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----echo = FALSE, out.width="100%", fig.cap = "Figure 1: Example of some of the different environmental factors, species traits and processes that could be included in a simulation."----
knitr::include_graphics("../man/figures/example_simulation.svg")

## ----echo = FALSE, out.width="100%", fig.cap = "Figure 2: Overview of the different components of a simulation and how they interact with each other. Note that the number of species as well as the number of traits and processes per species is not limited, but only a selection is shown for simplicity."----
knitr::include_graphics("../man/figures/simulation_high_level_overview.svg")

## ----setup--------------------------------------------------------------------
library(metaRange)
library(terra)

## ----create_landscape, fig.cap = "Figure 3: The habitat quality of the example landscape. Note: higher value = better habitat quality"----
# find the file
raster_file <- system.file("ex/elev.tif", package = "terra")

# load it
r <- rast(raster_file)

# scale it
r <- scale(r, center = FALSE, scale = TRUE)
plot(r, main = "Habitat quality")

## -----------------------------------------------------------------------------
r <- rep(r, 10)
landscape <- sds(r)
names(landscape) <- c("habitat_quality")
landscape

## ----enable_reporting---------------------------------------------------------
# 0 = no reporting
# 1 = a bit of info
# 2 = very verbose
set_verbosity(2)

## ----create_simulation--------------------------------------------------------
sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)

## ----simulation_summary-------------------------------------------------------
sim
summary(sim)

## ----add_species--------------------------------------------------------------
sim$add_species(name = "species_1")

## ----access_species-----------------------------------------------------------
sim$species_1

## ----add_trait----------------------------------------------------------------
sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    abundance = 100,
    reproduction_rate = 0.5,
    carrying_capacity = 1000
    # ...
    # Note that here could be more traits, there is no limit
)

## -----------------------------------------------------------------------------
sim$species_1$traits

## ----fig.cap = "Figure 4: The initial abundance of the species."--------------
plot(sim$species_1, "abundance")

## ----Reproduction-------------------------------------------------------------
sim$add_process(
    species = "species_1",
    process_name = "reproduction",
    process_fun = function() {
        # use a ricker reproduction model
        # to calculate the new abundance
        # and let the carrying capacity
        # depend on the habitat quality
        ricker_reproduction_model(
            self$traits$abundance,
            self$traits$reproduction_rate,
            self$traits$carrying_capacity * self$sim$environment$current$habitat_quality
        )

        # print out the current mean abundance
        print(
            paste0("mean abundance: ", mean(self$traits$abundance))
        )
    },
    execution_priority = 1
)

## ----run_simulation-----------------------------------------------------------
sim$begin()

## ----resA, fig.cap = "Figure 5: The resulting abundance distribution of species 1 after 10 simulation time steps."----
# define a nice color palette
plot_cols <- hcl.colors(100, "BluYl", rev = TRUE)
plot(
    sim,
    obj = "species_1",
    name = "abundance",
    main = "Species 1: abundance",
    col = plot_cols
)

## ----save_results, eval = FALSE-----------------------------------------------
#  save_species(
#      sim$species_1,
#      traits = c("name", "of", "one_or_more", "traits"),
#      path = "path/to/a/folder/"
#  )

