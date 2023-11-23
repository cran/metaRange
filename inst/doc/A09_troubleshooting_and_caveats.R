## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----typo, error=TRUE---------------------------------------------------------
library(metaRange)
library(terra)
set_verbosity(0)
raster_file <- system.file("ex/elev.tif", package = "terra")
r <- rast(raster_file)
r <- scale(r, center = FALSE, scale = TRUE)
r <- rep(r, 10)
landscape <- sds(r)
names(landscape) <- c("habitat_quality")

sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)

sim$add_species(name = "species_1")
sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    abundance = 100,
    reproduction_rtae = 0.5,
    carrying_capacity = 1000
)
sim$add_process(
    species = "species_1",
    process_name = "reproduction",
    process_fun = function() {
        ricker_reproduction_model(
            self$traits$abundance,
            self$traits$reproduction_rate,
            self$traits$carrying_capacity * self$sim$environment$current$habitat_quality
        )
    },
    execution_priority = 1
)
sim$begin()

## ----typo2, error=TRUE--------------------------------------------------------
set_verbosity(2)

sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)

sim$add_species(name = "species_1")
sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    abundance = 100,
    reproduction_rtae = 0.5,
    carrying_capacity = 1000
)
sim$add_process(
    species = "species_1",
    process_name = "reproduction",
    process_fun = function() {
        ricker_reproduction_model(
            self$traits$abundance,
            self$traits$reproduction_rate,
            self$traits$carrying_capacity * self$sim$environment$current$habitat_quality
        )
    },
    execution_priority = 1
)
sim$begin()

## ----typo3, eval=FALSE--------------------------------------------------------
#  set_verbosity(2)
#  
#  sim <- create_simulation(
#      source_environment = landscape,
#      ID = "example_simulation",
#      seed = 1
#  )
#  
#  sim$add_species(name = "species_1")
#  sim$add_traits(
#      species = "species_1",
#      population_level = TRUE,
#      abundance = 100,
#      reproduction_rtae = 0.5,
#      carrying_capacity = 1000
#  )
#  sim$add_process(
#      species = "species_1",
#      process_name = "reproduction",
#      process_fun = function() {
#          browser()
#  
#          ricker_reproduction_model(
#              self$traits$abundance,
#              self$traits$reproduction_rate,
#              self$traits$carrying_capacity * self$sim$environment$current$habitat_quality
#          )
#      },
#      execution_priority = 1
#  )
#  sim$begin()

## ----typo4, eval=FALSE--------------------------------------------------------
#  # type this in the console,
#  # once the browser has halted the code execution
#  ls()

## ----typo5, echo=FALSE--------------------------------------------------------
set_verbosity(0)

sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)

sim$add_species(name = "species_1")
sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    abundance = 100,
    reproduction_rtae = 0.5,
    carrying_capacity = 1000
)
ls(sim$species_1)

## ----typo6, eval=FALSE--------------------------------------------------------
#  # type this in the console,
#  # once the browser has halted the code execution
#  self$traits

## ----typo7, echo=FALSE--------------------------------------------------------
set_verbosity(0)

sim <- create_simulation(
    source_environment = landscape,
    ID = "example_simulation",
    seed = 1
)

sim$add_species(name = "species_1")
sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    abundance = 100,
    reproduction_rtae = 0.5,
    carrying_capacity = 1000
)
sim$species_1$traits

