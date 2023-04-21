#' Makara R6 Class
#'
#' @description An R6 class representing a connection to the Makara database.
#'
#' @field connection A database connection object, created using a package such as `DBI::dbConnect()`.
#' @field table The table or view name that contains the data.
#' @field data The dataset.
#'
Makara <- R6::R6Class(classname = "Makara",
                      public = list(
                        
                        # fields
                        connection = NULL,
                        table = NULL,
                        data = NULL,
                        
                        # methods
                        #' @description Initialize the R6 Object
                        #'
                        #' @param connection DB connection
                        #' @param table Table or view name
                        #'
                        initialize = function(connection, table) {
                          stopifnot(DBI::dbIsValid(dbObj = connection))
                          if (grepl('\\.', table)) {
                            table_split <- unlist(strsplit(x = table, split = '\\.'))
                            stopifnot(DBI::dbExistsTable(conn = connection, name = table_split[2], schema = table_split[1]))
                          } else {
                            stopifnot(DBI::dbExistsTable(conn = connection, name = table))
                          }
                          self$connection <- connection
                          self$table <- table
                        },
                        
                        #' @name get_data
                        #' 
                        #' @description Reads the data into R
                        #'
                        #' @returns The dataset
                        #'
                        get_data = function() {
                          self$data <- DBI::dbGetQuery(conn = self$connection, statement = paste0('SELECT * FROM ', self$table, ';'))
                        },
                        
                        #' @description Print the R6 Object
                        print = function() {
                          cat(paste0("You're currently connected to ", DBI::dbGetInfo(self$connection)$dbms.name, ".\n"))
                        }
                      )
)