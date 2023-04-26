#' Occurrence R6 Class
#'
#' @description An R6 class representing acoustic / vocal occurrence data from Passive Acoustic Monitoring.
#'
#' @field species_id A vector of species IDs to filter the data, see data table `PAGROUP.S_SPECIES`.
#' @field agg_data The aggregated dataset.
#'
Occurrence <- R6::R6Class(classname = "Occurrence", 
                          
                          # super class (parent)
                          inherit = Makara,
                          
                          # public
                          public = list(
                            
                            # fields
                            species_id = NULL,
                            agg_data = NULL,

                            # methods
                            #' @description Reads the data into R from the DB.
                            #'
                            get_data = function() {
                              super$get_data()
                              self$agg_data <- self$data
                            },
                            
                            #' @description Filters the data by species. 
                            #' This method modifies the SQL statement to query the DB. 
                            #' If changed, you must re-execute \code{Occurrence$get_data()}.
                            #' 
                            #' @param species_id Vector of species IDs to filter by (see data table `PAGROUP.S_SPECIES`)
                            #' 
                            filter_species = function(species_id) {
                              self$species_id <- species_id
                              stopifnot(!is.null(self$species_id))
                              self$sql <- paste0('SELECT * FROM ', self$table, ' where SPECIES_ID in (%s)')
                              self$sql <- sprintf(self$sql, toString(sprintf("'%s'", self$species_id)))
                            },
                            
                            #' @description Aggregates the data by grouping variable(s).
                            #' 
                            #' @param .column Column name of the data to be aggregated
                            #' @param .function Character string of the function to aggregate the data in `.column`
                            #' 
                            #' Available functions:
                            #' \itemize{
                            #' \item{'n'}{ Count unique values}
                            #' \item{'min'}{ Minimum value}
                            #' \item{'max'}{ Maximum value}
                            #' \item{'mean'}{ Average value}
                            #' \item{'median'}{ Median value}
                            #' }
                            #' @param ... Grouping variable(s) 
                            #' 
                            #' @importFrom dplyr %>%
                            #'
                            aggregate = function(.column, .function, ...) {
                              stopifnot(!is.null(self$agg_data))
                              stopifnot(is.character(.function))
                              fun <- switch(.function,
                                            "n" = private$count,
                                            "min" = min,
                                            "max" = max,
                                            "mean" = mean,
                                            "median" = median,
                                            stop("Function not found."))
                              self$agg_data <- self$agg_data %>%
                                dplyr::group_by(...) %>%
                                dplyr::summarise("{ .function }_{{ .column }}" := fun({{ .column }}), .groups = 'drop')
                              self
                            },
                            
                            #' @description Prints the Occurrence R6 Object
                            print = function() {
                              # some kind of summary
                            }
                            
                          ),
                          
                          # private (not accessible outside of the class)
                          private = list(
                            
                            # methods
                            count = function(x) length(x)
                            
                          )
                          
)