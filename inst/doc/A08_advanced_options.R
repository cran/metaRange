## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(metaRange)
library(terra)

## ----load_landscape_fun-------------------------------------------------------
raster_file <- system.file("ex/elev.tif", package = "terra")
r <- rast(raster_file)
temperature <- scale(r, center = FALSE, scale = TRUE) * 10 + 273.15
precipitation <- r * 2
temperature <- rep(temperature, 10)
precipitation <- rep(precipitation, 10)
landscape <- sds(temperature, precipitation)
names(landscape) <- c("temperature", "precipitation")

## ----setup2-------------------------------------------------------------------
sim <- create_simulation(landscape)
sim$number_time_steps
sim$time_step_layer

## ----static-------------------------------------------------------------------
sim$set_time_layer_mapping(rep_len(1, 10))
sim$number_time_steps
sim$time_step_layer

## ----change_num_time----------------------------------------------------------
sim$set_time_layer_mapping(c(2, 4, 6))
sim$number_time_steps
sim$time_step_layer

## ----burnin-------------------------------------------------------------------
sim$set_time_layer_mapping(c(rep_len(1, 10), 2:10))
sim$number_time_steps
sim$time_step_layer

## ----globals------------------------------------------------------------------
sim$add_species("species_one")
sim$add_species("species_two")
sim$add_globals(
    mean_abundance_over_time = list(
        "species_one" = c(),
        "species_two" = c()
    )
    # ... more global variables
)
sim$globals$global_var

## ----processes----------------------------------------------------------------
sim$add_process(
    # Note the missing species argument
    process_name = "global_process",
    process_fun = function() {
        # self = simulation object
        # easy access to simulation functions
        for (sp in self$species_names()) {
            self$globals$mean_abundance_over_time[[sp]] <-
                c(
                    self$globals$mean_abundance_over_time[[sp]],
                    mean(self[[sp]]$traits$abundance)
                )
        }
    },
    execution_priority = 1
)
sim$processes$global_process

## ----quequing-----------------------------------------------------------------
sim <- create_simulation(landscape)
sim$set_time_layer_mapping(c(1:6))
sim$add_species(name = "species_1")
sim$add_process(
    species = "species_1",
    process_name = "invasion",
    process_fun = function() {
        message("Species invades!")
    },
    execution_priority = 1,
    # Note the queue = FALSE argument
    queue = FALSE
)
sim$add_process(
    process_name = "activate_species_1",
    process_fun = function() {
        message(paste0("time step: ", self$get_current_time_step()))
        # Note that when manually changing the queue,
        # the changes will take place in the
        # _next_ time step
        # e.g. the following will lead to the process
        # being first executed in time step 4)
        if (self$get_current_time_step() == 3) {
            message("Activating species 1")
            for (pr in self$species_1$processes) {
                self$queue$enqueue(pr)
            }
        }
    },
    execution_priority = 1
)
sim$begin()

## ----dequeuing----------------------------------------------------------------
sim <- create_simulation(landscape)
sim$set_time_layer_mapping(c(1:6))
sim$add_species(name = "species_1")
sim$add_process(
    species = "species_1",
    process_name = "invasion",
    process_fun = function() {
        message("Species invades!")
    },
    execution_priority = 1,
)
sim$add_process(
    process_name = "stop_invasion",
    process_fun = function() {
        message(paste0("time step: ", self$get_current_time_step()))
        if (self$get_current_time_step() == 3) {
            message("Extiction species 1")
            for (pr in self$species_1$processes) {
                # Here we are querying the process ID,
                # which is a unique identifier for each process
                # so that the priority queue knows what to remove
                self$queue$dequeue(pr$get_PID())
            }
        }
    },
    execution_priority = 1
)
sim$begin()

## ----exit---------------------------------------------------------------------
sim <- create_simulation(landscape)
sim$set_time_layer_mapping(c(1:6))
sim$add_species(name = "species_1")
sim$add_process(
    species = "species_1",
    process_name = "invasion",
    process_fun = function() {
        message("Species invades!")
    },
    execution_priority = 1,
)
sim$add_process(
    process_name = "end_simualtion",
    process_fun = function() {
        message(paste0("time step: ", self$get_current_time_step()))
        if (self$get_current_time_step() == 4) {
            message("Ending simulation early")
            self$exit()
        }
    },
    execution_priority = 1
)
sim$begin()

