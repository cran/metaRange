% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{summary.metaRangeSpecies}
\alias{summary.metaRangeSpecies}
\title{Summary for metaRange species}
\usage{
\method{summary}{metaRangeSpecies}(object, ...)
}
\arguments{
\item{object}{\verb{<metaRangeSpecies>} The \link{metaRangeSpecies} object to summarize.}

\item{...}{\verb{<any>} ignored.}
}
\value{
\verb{<invisible NULL>}
}
\description{
Summary for metaRange species
}
\examples{
sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
names(sim_env) <- "env_01"
test_sim <- metaRangeSimulation$new(source_environment = sim_env)
test_sim$add_species("species_01")
summary(test_sim$species_01)
}
