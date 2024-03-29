# Copyright (C) 2023, 2024 Stefan Fallert, Lea Li, Juliano Sarmento Cabral
#
# This file is part of metaRange.
#
# metaRange is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.
#
# metaRange is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with metaRange. If not, see <http://www.gnu.org/licenses/>.

#' @title metaRangeProcess object
#'
#' @description Creates an metaRangeProcess object in form of an
#' [R6][R6::R6Class] class that stores and handles all the individual parts
#' that define a process.
#'
#' @return `<metaRangeProcess>` A [metaRangeProcess] object.
#' @export
metaRangeProcess <- R6::R6Class("metaRangeProcess",
    cloneable = FALSE,
    lock_objects = FALSE,
    public = list(
        # ---------- public fields -------------
        #' @field fun `<function>` The processes function.
        fun = NULL,
        # ---------- initialization -----------

        #' @description Creates a new [metaRangeProcess] object
        #' @param process_name `<string>` name of the process.
        #' @param id `<string>` optional ID of the process.
        #' @param process_fun `<function>` The function to be
        #' called when the process is executed. This function will be executed
        #' in the specified environment (see argument: env) and has access to all the
        #' variables in that environment. This function may not have any arguments,
        #' i.e. `is.null(formals(process_fun))` must be `TRUE`.
        #' @param execution_priority `<integer>` the priority of the process.
        #' The lower the number the earlier the process is executed.
        #' Note that the priority is only used to sort the processes
        #' in the priority queue. The actual execution order is determined
        #' by the order of the processes in the queue.
        #' @param env `<environment>` the environment where the process should be executed.
        #' @param env_label `<string>` optional name of the execution environment.
        #' Just used as a human readable label for debug purposes.
        #' @examples
        #' # Note: Only for illustration purposes. Use the add_process method of the
        #' # simulation object to add processes to a simulation.
        #' pr <- metaRangeProcess$new(
        #'    process_name = "ecological_process",
        #'    process_fun = function() {
        #'       cat("Execute ecological process!")
        #'    },
        #'    execution_priority = 1L,
        #'    env = new.env(),
        #'    env_label = "a_species_name"
        #' )
        #' pr
        #' @return `<metaRangeProcess>` A [metaRangeProcess] object.
        initialize = function(process_name, id = "", process_fun, execution_priority, env, env_label = NULL) {
            checkmate::assert_string(x = process_name, min.chars = 1, max.chars = 64)
            checkmate::assert_string(x = id, min.chars = 0, max.chars = 64)
            checkmate::assert_function(x = process_fun, args = character(0))
            execution_priority <- checkmate::assert_int(x = execution_priority, lower = 1, coerce = TRUE)
            checkmate::assert_environment(x = env)
            checkmate::assert_string(x = env_label, min.chars = 0, max.chars = 64, null.ok = TRUE)
            private$name <- process_name
            self$fun <- process_fun
            private$execution_priority <- execution_priority
            private$execution_environment_label <- env_label

            environment(self$fun) <- env

            # Note: we start with "PID" since the process name may contain numeric value
            # and R variable names can't start with those
            private$PID <- paste0(
                "PID-",
                execution_priority,
                as.hexmode(sample.int(.Machine$integer.max, 1)),
                # the id is a running counter that is incremented for each process
                # intended to assuere that the names are unique even if the RNG gives the same number
                id,
                "-",
                process_name,
                "-",
                env_label
            )
            if (nchar(private$PID) > 64) {
                private$PID <- substr(private$PID, 1, 64)
            }
        },
        # ---------- public methods -----------
        #' @description get the process ID
        #' @examples
        #' pr <- metaRangeProcess$new("A", "1", \() {}, 1, new.env())
        #' pr$get_PID()
        #' @return `<string>` The process ID
        get_PID = function() {
            return(private$PID)
        },
        #' @description get the process name
        #' @examples
        #' pr <- metaRangeProcess$new("A", "1", \() {}, 1, new.env())
        #' pr$get_name()
        #' @return `<string>` The process name
        get_name = function() {
            return(private$name)
        },
        #' @description get the process execution priority
        #' @examples
        #' pr <- metaRangeProcess$new("A", "1", \() {}, 1, new.env())
        #' pr$get_priority()
        #' @return `<integer>` The process execution priority
        #' @seealso [metaRangePriorityQueue]
        get_priority = function() {
            return(private$execution_priority)
        },
        #' @description get the name of the process execution environment
        #' @examples
        #' pr <- metaRangeProcess$new("A", "1", \() {}, 1, new.env(), "human_readable_label")
        #' pr$get_env_label()
        #' @return `<string>` The name of the process execution environment or NULL
        get_env_label = function() {
            return(private$execution_environment_label)
        },

        #' @description Prints information about the process to the console
        #' @examples
        #' pr <- metaRangeProcess$new("A", "1", \() {}, 1, new.env())
        #' pr$print()
        #' @return `<invisible self>`
        print = function() {
            cat("Process name: ", private$name, "\n")
            cat("PID: ", private$PID, "\n")
            cat("execution_priority: ", private$execution_priority, "\n")
            cat("execution_environment_label: ", private$execution_environment_label, "\n")
            cat("$fun: ")
            show(self$fun)
            return(invisible(self))
        }
    ),
    private = list(
        # @field PID (process) ID.
        PID = NULL,

        # @field name name of the process.
        name = NULL,

        # @field execution_priority The priority of the process.
        # The lower the number the earlier the process is executed.
        # Note that the priority is only used to sort the processes
        # in the priority queue. The actual execution order is determined
        # by the order of the processes in the queue.
        # @seealso [metaRangePriorityQueue]
        execution_priority = NULL,

        # @field execution_environment_label The name of the environment where the process should be executed.
        execution_environment_label = NULL
    )
)
