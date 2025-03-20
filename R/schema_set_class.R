example = function() {
  # Define a schema using schema_obj
  schema <- schema_obj(list(
    name = schema_str("City name"),
    num = schema_int("Numeric value"),
    population = schema_num("Population count")
  ))

  # Sample data frame
  df <- data.frame(
    name = c("Paris", "Lyon"),
    num = c("1", "2"),
    population = c("2148327", "513275"),
    extra_col = c("ignore", "this"),
    stringsAsFactors = FALSE
  )

  # Convert df column classes based on the schema
  df_converted <- schema_set_df_col_class(df, schema)
  str(df_converted)
}

#' Set Data Frame Column Classes Based on Schema
#'
#' This function converts the columns of a data frame to the appropriate R data types based on the
#' provided schema. The schema can be either an object schema (created with `schema_obj`) or an array
#' schema (created with `schema_arr`). Only the columns listed in the schema's properties (or in the
#' items' properties for an array schema) are converted. If conversion fails (resulting in `NA`), any
#' warnings are suppressed.
#'
#' @param df A data frame whose columns are to be converted.
#' @param schema A schema definition, either a schema object (from `schema_obj`) or an array schema (from `schema_arr`).
#'
#' @return A data frame with columns converted to the types defined in the schema.
#'
#' @examples
#' # Define a schema using schema_obj
#' schema <- schema_obj(list(
#'   name = schema_str("City name"),
#'   num = schema_int("Numeric value"),
#'   population = schema_num("Population count")
#' ))
#'
#' # Sample data frame
#' df <- data.frame(
#'   name = c("Paris", "Lyon"),
#'   num = c("1", "2"),
#'   population = c("2148327", "513275"),
#'   extra_col = c("ignore", "this"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Convert df column classes based on the schema
#' df_converted <- schema_set_df_col_class(df, schema)
#' str(df_converted)
#'
#' @export
schema_set_df_col_class <- function(df, schema) {
  # Determine which schema properties to use:
  # If schema is a schema_arr then we use the properties from its "items" element.
  if (inherits(schema, "schema_arr")) {
    if (!is.null(schema$items$properties)) {
      properties <- schema$items$properties
    } else {
      stop("The schema_arr's items does not contain a properties field.")
    }
  } else if (inherits(schema, "schema_obj")) {
    properties <- schema$properties
  } else {
    stop("schema must be either a schema_obj or a schema_arr.")
  }

  # Loop over each property and convert the matching data frame column
  for (prop in names(properties)) {
    if (prop %in% names(df)) {
      target_type <- properties[[prop]]$type
      # Convert the column based on its target type with warnings suppressed
      if (target_type == "string") {
        df[[prop]] <- suppressWarnings(as.character(df[[prop]]))
      } else if (target_type == "integer") {
        df[[prop]] <- suppressWarnings(as.integer(df[[prop]]))
      } else if (target_type == "number") {
        df[[prop]] <- suppressWarnings(as.numeric(df[[prop]]))
      } else if (target_type == "bool") {
        df[[prop]] <- suppressWarnings(as.logical(df[[prop]]))
      } else if (target_type == "date") {
        df[[prop]] <- suppressWarnings(as.Date(df[[prop]]))
      } else if (target_type == "datetime") {
        df[[prop]] <- suppressWarnings(as.POSIXct(df[[prop]]))
      } else {
        # For types such as "object" or "array" (or any unrecognized type) we do not change the column.
        message(sprintf("No conversion defined for type '%s' in column '%s'. Column left unchanged.",
                        target_type, prop))
      }
    }
    # Columns in df not listed in the schema are ignored.
  }

  return(df)
}
