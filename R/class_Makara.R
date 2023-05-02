#' Makara R6 Class
#'
#' @description An R6 class representing a connection to the Makara database.
#'
#' @field connection A database connection object, created using a package such as `ROracle::dbConnect()`.
#' @field table A table or view name that contains the data.
#' @field sql A SQL statement to query the database (DB).
#'
#' @export
#'
Makara <- R6::R6Class(classname = "Makara",
                      
                      # public
                      public = list(
                        
                        # fields
                        connection = NULL,
                        table = NULL,
                        sql = NULL,
                        
                        # methods
                        #' @description Initializes the Makara R6 object.
                        #'
                        #' @param connection DB connection
                        #' @param table Table or view name
                        #'
                        initialize = function(connection, table) {
                          stopifnot(private$dbIsValid())
                          if (grepl('\\.', table)) {
                            table_split <- unlist(strsplit(x = table, split = '\\.'))
                            stopifnot(ROracle::dbExistsTable(conn = connection, name = table_split[2], schema = table_split[1]))
                          } else {
                            stopifnot(ROracle::dbExistsTable(conn = connection, name = table, schema = 'PAGROUP'))
                          }
                          self$connection <- connection
                          self$table <- table
                          self$sql <- paste0('SELECT * FROM ', self$table)
                        },
                        
                        #' @description Prints the Makara R6 object.
                        #' 
                        print = function() {
                          if (private$dbIsValid()) {
                            message(paste0("You're currently connected to ", ROracle::dbGetInfo(self$connection)$dbname, " and pulling from ", self$table, "."))
                          } else {
                            message("You're disconnected from the Makara database.")
                          }
                        },
                        
                        #' @description What to do when you remove the Makara R6 object.
                        #' 
                        finalize = function() {
                          ROracle::dbDisconnect(self$connection)
                          message("Disconnecting from the Makara database.")
                        }
                        
                      ),
                      
                      # private (not accessible outside of class)
                      private = list(
                        
                        # fields
                        data = NULL,
                       
                        # methods
                        get_data = function() {
                          private$data <- ROracle::dbGetQuery(conn = self$connection, statement = self$sql)
                        },
                        
                        # https://stackoverflow.com/questions/54218194/how-does-one-determine-if-a-data-base-connection-is-open-or-close-using-the-r-or
                        dbIsValid = function() {
                          if (length(ROracle::dbListConnections(ROracle::Oracle())) > 0) {
                            TRUE
                          } else {
                            FALSE
                          }
                        }
                        
                      )
                      
)