#' Makara R6 Class
#'
#' @description An R6 class representing a connection to the Makara database.
#'
#' @field connection A database connection object, created using a package such as `DBI::dbConnect()`.
#' @field table The table or view name that contains the data.
#' @field sql The SQL statement to query the database (DB).
#' @field data The dataset.
#'
Makara <- R6::R6Class(classname = "Makara",
                      
                      # public
                      public = list(
                        
                        # fields
                        connection = NULL,
                        table = NULL,
                        sql = NULL,
                        data = NULL,
                        
                        # methods
                        #' @description Initializes the Makara R6 object.
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
                            stopifnot(DBI::dbExistsTable(conn = connection, name = table, schema = 'PAGROUP'))
                          }
                          self$connection <- connection
                          self$table <- table
                          self$sql <- paste0('SELECT * FROM ', self$table)
                        },
                        
                        #' @description Reads the data into R from the DB.
                        #'
                        get_data = function() {
                          self$data <- DBI::dbGetQuery(conn = self$connection, statement = self$sql)
                        },
                        
                        #' @description Prints the Makara R6 object.
                        #' 
                        print = function() {
                          if (DBI::dbIsValid(self$connection)) {
                            message(paste0("You're currently connected to ", DBI::dbGetInfo(self$connection)$dbms.name, " and pulling from ", self$table, ".\n"))
                          } else {
                            message("You're disconnected from the Makara database.\n")
                          }
                        },
                        
                        #' @description Removes the Makara R6 object.
                        #' 
                        finalize = function() {
                          message("Disconnecting from the Makara database.\n")
                          DBI::dbDisconnect(self$connection)
                        }
                      )
                      
)