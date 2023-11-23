## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup, class.source = 'fold-hide'----------------------------------------
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
sim$add_species(name = "species_2")

sim$add_traits(
    species = c("species_1", "species_2"),
    population_level = TRUE,
    abundance = 900,
    climate_suitability = 1,
    reproduction_rate = 0.3,
    carrying_capacity = 1000
)
sim$add_traits(
    species = c("species_1", "species_2"),
    population_level = FALSE,
    dispersal_kernel = calculate_dispersal_kernel(
        max_dispersal_dist = 3,
        kfun = negative_exponential_function,
        mean_dispersal_dist = 1
    )
)

## ----suitability1, fig.cap="Figure 1: Suitability curve for the temperature niches of species 1 (blue) and species 2 (red)."----
opt_temp <- 285
min_temp_sp1 <- 270
max_temp_sp1 <- 290
min_temp_sp2 <- 260
max_temp_sp2 <- 295

opt_prec <- 700
min_prec_sp1 <- 200
max_prec_sp1 <- 1000
min_prec_sp2 <- 0
max_prec_sp2 <- 1200

x <- seq(min_temp_sp2, max_temp_sp2, length.out = 100)
y_sp1 <- calculate_suitability(max_temp_sp1, opt_temp, min_temp_sp1, x)
y_sp2 <- calculate_suitability(max_temp_sp2, opt_temp, min_temp_sp2, x)
plot(x, y_sp1, type = "l", xlab = "Temperature [K]", ylab = "Suitability", col = "darkred", lwd = 2)
lines(x, y_sp2, col = "darkblue", lwd = 2)
legend(
    "topleft",
    legend = c("species 1", "species 2"),
    col = c("darkred", "darkblue"),
    lty = 1,
    lwd = 2,
    cex = 0.7
)

## ----suitability2, fig.cap="Figure 2: Suitability curve for the precipitation niches of species 1 (blue) and species 2 (red)."----
x <- seq(min_prec_sp2, max_prec_sp2, length.out = 100)
y_sp1 <- calculate_suitability(max_prec_sp1, opt_prec, min_prec_sp1, x)
y_sp2 <- calculate_suitability(max_prec_sp2, opt_prec, min_prec_sp2, x)
plot(x, y_sp1, type = "l", xlab = "precipitation [mm]", ylab = "Suitability", col = "darkred", lwd = 2)
lines(x, y_sp2, col = "darkblue", lwd = 2)
legend(
    "topleft",
    legend = c("species 1", "species 2"),
    col = c("darkred", "darkblue"),
    lty = 1,
    lwd = 2,
    cex = 0.7
)

## ----suitability3, fig.cap="Figure 3: Example suitability curve for the temperature niche of species 2."----
sim$add_traits(
    species = "species_1",
    population_level = FALSE,
    max_temperature = max_temp_sp1,
    optimal_temperature = opt_temp,
    min_temperature = min_temp_sp1,
    max_precipitation = max_prec_sp1,
    optimal_precipitation = opt_prec,
    min_precipitation = min_prec_sp1
)
sim$add_traits(
    species = "species_2",
    population_level = FALSE,
    max_temperature = max_temp_sp2,
    optimal_temperature = opt_temp,
    min_temperature = min_temp_sp2,
    max_precipitation = max_prec_sp2,
    optimal_precipitation = opt_prec,
    min_precipitation = min_prec_sp2
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
sim$add_process(
    species = c("species_1", "species_2"),
    process_name = "dispersal_process",
    process_fun = function() {
        self$traits[["abundance"]] <- dispersal(
            abundance = self$traits[["abundance"]],
            dispersal_kernel = self$traits[["dispersal_kernel"]]
        )
    },
    execution_priority = 3
)

## ----Competition--------------------------------------------------------------
sim$add_process(
    species = "species_2",
    process_name = "competition",
    process_fun = function() {
        max_capacity <- max(self$traits$carrying_capacity)
        self$traits$carrying_capacity <-
            pmax(max_capacity - self$sim$species_1$traits$abundance, 0)
    },
    execution_priority = 4
)

## ----resA, fig.show="hold"----------------------------------------------------
sim$begin()
plot_cols <- hcl.colors(100, "Purple-Yellow", rev = TRUE)
plot(sim, "species_1", "abundance", main = "Sp: 1 abundance", col = plot_cols)
plot(sim, "species_2", "abundance", main = "Sp: 2 abundance", col = plot_cols)

## ----setup_again, class.source = 'fold-hide'----------------------------------
sim <- create_simulation(landscape)
sim$add_species(name = "species_1")
sim$add_species(name = "species_2")

sim$add_traits(
    species = "species_1",
    population_level = TRUE,
    abundance = 10000,
    climate_suitability = 1,
    reproduction_rate = 0.5,
    carrying_capacity = 10000
)
sim$add_traits(
    species = "species_2",
    population_level = TRUE,
    abundance = 500,
    climate_suitability = 1,
    reproduction_rate = 0.3,
    carrying_capacity = 1000
)
sim$add_traits(
    species = c("species_1", "species_2"),
    population_level = FALSE,
    dispersal_kernel = calculate_dispersal_kernel(
        max_dispersal_dist = 3,
        kfun = negative_exponential_function,
        mean_dispersal_dist = 1
    )
)
sim$add_traits(
    species = "species_1",
    population_level = FALSE,
    max_temperature = 300,
    optimal_temperature = 290,
    min_temperature = 270,
    max_precipitation = 1000,
    optimal_precipitation = 800,
    min_precipitation = 0
)
sim$add_traits(
    species = "species_2",
    population_level = FALSE,
    max_temperature = 300,
    optimal_temperature = 270,
    min_temperature = 260,
    max_precipitation = 1000,
    optimal_precipitation = 300,
    min_precipitation = 0
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
sim$add_process(
    species = c("species_1", "species_2"),
    process_name = "dispersal_process",
    process_fun = function() {
        self$traits[["abundance"]] <- dispersal(
            abundance = self$traits[["abundance"]],
            dispersal_kernel = self$traits[["dispersal_kernel"]]
        )
    },
    execution_priority = 3
)

## ----predation----------------------------------------------------------------
sim$add_globals(trophic_conversion_factor = 0.5)
sim$add_process(
    species = "species_2",
    process_name = "predation",
    process_fun = function() {
        self$traits$abundance <-
            self$sim$species_1$traits$abundance *
            self$traits$climate_suitability *
            self$sim$globals$trophic_conversion_factor

        self$sim$species_1$traits$abundance <-
            self$sim$species_1$traits$abundance -
            self$sim$species_1$traits$abundance *
            self$traits$climate_suitability
    },
    execution_priority = 4
)

## ----resB, fig.show="hold"----------------------------------------------------
sim$begin()
plot_cols <- hcl.colors(100, "Purple-Yellow", rev = TRUE)
plot(sim, "species_1", "abundance", main = "Sp: 1 abundance", col = plot_cols)
plot(sim, "species_2", "abundance", main = "Sp: 2 abundance", col = plot_cols)

## ----resC, eval= FALSE--------------------------------------------------------
#  max_temperature = 300
#  optimal_temperature = 290
#  min_temperature = 270
#  max_precipitation = 1000
#  optimal_precipitation = 800
#  min_precipitation = 0

## ----resD, eval= FALSE--------------------------------------------------------
#  max_temperature = 300
#  optimal_temperature = 270
#  min_temperature = 260
#  max_precipitation = 1000
#  optimal_precipitation = 300
#  min_precipitation = 0

