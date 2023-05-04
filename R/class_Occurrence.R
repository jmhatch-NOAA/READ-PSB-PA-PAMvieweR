#' Occurrence R6 Class
#'
#' @description An R6 class representing acoustic / vocal occurrence data from Passive Acoustic Monitoring (PAM).
#'
#' @field species_id A vector of species IDs, see data table `PAGROUP.S_SPECIES`.
#' @field pam_data The data.
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
                            #' @description Initializes the Makara R6 object.
                            #'
                            #' @param connection DB connection
                            #' @param table Table or view name
                            #' @param data `tibble` or `data.frame` object
                            #'
                            initialize = function(connection, table, data = NULL) { 
                              if (is.null(data)) {
                                super$initialize(connection, table)
                                private$access_db <- TRUE
                              } else {
                                self$pam_data <- private$data <- data
                              }
                            },
                            
                            #' @description Queries the DB using an SQL statement and fetches the data into R.
                            #'
                            get_data = function() {
                              if (private$access_db) {
                                super$get_data()
                                self$pam_data <- private$data
                              } else {
                                message("Data are already accessible.")
                              }
                            },
                            
                            #' @description Manually set data in an Occurrence R6 class.
                            #' 
                            #' @param new_data `tibble` or `data.frame` 
                            #' 
                            set_data = function(new_data) {
                              self$pam_data <- private$data <- new_data
                              private$ordered <- private$filtered <- private$mutated <- private$selected <- private$grouped <- private$aggregated <- FALSE
                              private$access_db <- FALSE
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
                              if (private$access_db) {
                                self$species_id <- species_id
                                if (is.null(self$species_id)) {
                                  self$sql <- paste0('SELECT * FROM ', self$table)
                                } else {
                                  self$sql <- paste0('SELECT * FROM ', self$table, ' where SPECIES_ID in (%s)')
                                  self$sql <- sprintf(self$sql, toString(sprintf("'%s'", self$species_id)))
                                }
                              } else {
                                message("You cannot set a species_id when data are loaded manually.")
                              }
                              invisible(self)
                            },
                            
                            #' @description Order rows using column values.
                            #' 
                            #' @param ... See \link[dplyr]{arrange}
                            #' 
                            #' @importFrom magrittr %<>%
                            #' 
                            order = function(...) {
                              self$pam_data %<>%
                                dplyr::arrange(...)
                              private$ordered <- TRUE
                              invisible(self)
                            },
                            
                            #' @description Keep rows that match a condition. 
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
                            
                            #' @description Create, modify, and delete columns.
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
                            
                            #' @description Select variables in a data frame.
                            #' 
                            #' @param ... See \link[dplyr]{select}
                            #' 
                            #' @importFrom magrittr %<>%
                            #' 
                            select = function(...) {
                              self$pam_data %<>%
                                dplyr::select(...)
                              private$selected <- TRUE
                              invisible(self)
                            },
                            
                            #' @description Group by one or more variables. 
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
                            
                            #' @description Summarise each group down to one row.
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
                            
                            #' @description Resets the data.
                            #' 
                            reset = function() {
                              if(any(c(private$ordered, private$filtered, private$mutated, private$selected, private$grouped, private$aggregated))) {
                                self$pam_data <- private$data
                                private$ordered <- private$filtered <- private$mutated <- private$selected <- private$grouped <- private$aggregated <- FALSE
                                message("Occurrence$pam_data has been reset.")
                              } else {
                                message("No need to reset Occurrence$pam_data.")
                              }
                              invisible(self)
                            },
                            
                            #' @description Prints the Occurrence R6 Object
                            print = function() {
                              print(self$pam_data)
                            },
                            
                            #' @description What to do when you remove the Occurrence R6 object.
                            #' 
                            finalize = function() {
                              if (private$access_db) {
                                super$finalize()
                              } 
                            }
                            
                          ),
                          
                          # private (not accessible outside of class)
                          private = list(
                            
                            # fields
                            ordered = FALSE,
                            filtered = FALSE,
                            mutated = FALSE,
                            selected = FALSE,
                            grouped = FALSE,
                            summarized = FALSE,
                            access_db = FALSE
                            
                          )
                          
)