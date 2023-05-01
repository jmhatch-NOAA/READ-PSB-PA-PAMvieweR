#' Occurrence R6 Class
#'
#' @description An R6 class representing acoustic / vocal occurrence data from Passive Acoustic Monitoring (PAM).
#'
#' @field species_id A vector of species IDs, see data table `PAGROUP.S_SPECIES`.
#' @field pam_data The dataset (may be filtered, grouped, and / or summarized).
#'
#' @export
#'
Occurrence <- R6::R6Class(classname = "Occurrence", 
                          
                          # super class (parent)
                          inherit = Makara,
                          
                          # public
                          public = list(
                            
                            # fields
                            species_id = NULL,
                            pam_data = NULL,

                            # methods
                            #' @description Queries the DB using an SQL statement and fetches the data into R.
                            #'
                            get_data = function() {
                              super$get_data()
                              self$pam_data <- private$data
                            },
                            
                            #' @description Modifies the where clause of the SQL statement used by `get_data()` to include species IDs. 
                            #' 
                            #' @details 
                            #' This method modifies the SQL statement used to query the DB. 
                            #' If changed, you must re-execute \code{Occurrence$get_data()}.
                            #' 
                            #' @param species_id Vector of species IDs (see data table `PAGROUP.S_SPECIES`)
                            #' 
                            where_species = function(species_id) {
                              self$species_id <- species_id
                              if (is.null(self$species_id)) {
                                self$sql <- paste0('SELECT * FROM ', self$table)
                              } else {
                                self$sql <- paste0('SELECT * FROM ', self$table, ' where SPECIES_ID in (%s)')
                                self$sql <- sprintf(self$sql, toString(sprintf("'%s'", self$species_id)))
                              }
                              invisible(self)
                            },
                            
                            #' @description Filters the data. 
                            #'
                            #' @param ... See \link[dplyr]{filter}
                            #' 
                            #' @importFrom magrittr %<>%
                            #' 
                            filter = function(...) {
                              self$pam_data %<>%
                                dplyr::filter(...)
                              private$filtered <- TRUE
                              invisible(self)
                            },
                            
                            #' @description Groups the data. 
                            #' 
                            #' @param ... See \link[dplyr]{group_by}
                            #' 
                            #' @importFrom magrittr %<>%
                            #' 
                            group = function(...) {
                              self$pam_data %<>%
                                dplyr::group_by(...)
                              private$grouped <- TRUE
                              invisible(self)
                            },
                            
                            #' @description Summarizes the data.
                            #' 
                            #' @param ... See \link[dplyr]{summarize}
                            #' 
                            #' @importFrom magrittr %<>%
                            #' 
                            aggregate = function(...) {
                              self$pam_data %<>%
                                dplyr::summarize(...)
                              private$summarized <- TRUE
                              invisible(self)
                            },
                            
                            #' @description Modifies the data.
                            #' 
                            #' @param ... See \link[dplyr]{mutate}
                            #' 
                            #' @importFrom magrittr %<>%
                            #' 
                            mutate = function(...) {
                              self$pam_data %<>%
                                dplyr::mutate(...)
                              private$mutated <- TRUE
                              invisible(self)
                            },
                            
                            #' @description Resets the data.
                            #' 
                            reset = function() {
                              if(any(c(private$filtered, private$grouped, private$summarized, private$mutated))) {
                                self$pam_data <- private$data
                                message('Occurrence$pam_data has been reset.')
                              } else {
                                message('No need to reset Occurrence$pam_data.')
                              }
                              invisible(self)
                            },
                            
                            #' @description Prints the Occurrence R6 Object
                            print = function() {
                              print(self$pam_data)
                            }
                            
                          ),
                          
                          # private (not accessible outside of class)
                          private = list(
                            
                            # fields
                            filtered = FALSE,
                            grouped = FALSE,
                            summarized = FALSE,
                            mutated = FALSE
                            
                          )
                          
)