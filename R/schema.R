
example = function() {
  obj = schema_obj(list(name="", num=5))
  arr = schema_arr(schema_obj(list(name="", num=5)))
  arr = schema_arr(list(name="", num=5.2))

  schema_str("my_descr", parse=TRUE)

  schema = schema_arr(
    list(
      city = schema_str("Largest city in the country"),
      country = "France",
      famous_building="Eiffel Tower",
      population = schema_num("Population of the country in million inhabitants"),
      facts = schema_arr(list(name="fact1", descr="fact_descr"))
    )
  )
}

# sx_arr = function(...) {
#   x = schema_arr(items = sx_obj(...))
#   x
# }


# sx_obj = function(...) {
#   x = schema_obj(list(...))
#   x
# }



to_schema = function(x) {
  if (is_schema(x)) {
    return(x)
  }
  if (is.null(x)) return(NULL)

  # An example like "Paris", 5
  if (is.atomic(x)) {
    if (length(x) <= 1) {
      if (is.character(x)) return(schema_str())
      if (is.numeric(x)) {
        if (all(x == as.integer(x))) {
          return(schema_int())
        } else {
          return(schema_num())
        }
      }
      if (is.logical(x)) return(schema_bool())
      if (is(x, "Date")) return(schema_date())
      if (is(x, "POSIXct")) return(schema_datetime())
      restore.point("unknown atomic type")
      stop("Unknown atomic type")
    }
    if (length(x)>1) {
      items = to_schema(x[1])
      return(schema_arr(items))
    }
  }
  if (is.list(x)) {
    if (!is.null(names(x)) && any(names(x) != "")) {
      return(schema_obj(x))
    } else {
      return(schema_arr(x))
    }
  }
  restore.point("schema_conversion_error")
  stop("Cannot convert x to a schema")
}

#
# schema_field = function(type="object", descr=NULL, minimum=NULL, exclusiveMinimum=NULL, ...) {
#   if (type=="object") return(schema_object(descr=descr, ...))
#   if (type=="arr") return(schema_arr(descr=descr, ...))
#   x = nn_locals_to_list()
#   x
# }

schema_str = function(descr=NULL, allow_null=NULL, enum=NULL, pattern=NULL, minLength=NULL, maxLength=NULL,is_key=NULL, ...) {
  x = nn_locals_to_list(...)
  x = add_schema_type(x, "string")
  class(x) = c("schema_str",  "schema", "list")
  x
}

schema_int = function(descr=NULL, allow_null = NULL, enum=NULL, minimum=NULL, exclusiveMinimum=NULL, maximum=NULL, exclusiveMaximum=NULL,is_key=NULL,  ...) {
  x = nn_locals_to_list(...)
  x = add_schema_type(x, "integer")
  class(x) = c("schema_int","schema", "list")
  x
}



schema_num = function(descr=NULL, allow_null=NULL, enum=NULL, minimum=NULL, exclusiveMinimum=NULL, maximum=NULL, exclusiveMaximum=NULL,is_key=NULL,  ...) {
  x = nn_locals_to_list(...)
  x = add_schema_type(x, "number")
  class(x) = c("schema_num", "schema", "list")
  x
}


schema_bool = function(descr=NULL, allow_null = NULL,is_key=NULL,  ...) {
  x = nn_locals_to_list(...)
  x = add_schema_type(x, "bool")
  class(x) = c("schema_int","schema", "list")
  x
}


schema_arr = function(items, descr=NULL, minItems=NULL,maxItems=NULL, uniqueItems=NULL, ...) {
  x = nn_locals_to_list(...)
  restore.point("schema_arr")
  if (!is_schema(items) & is.list(items)) {
    x$items = schema_obj(items)
  } else {
    x$items = to_schema(x$items)
  }
  x = add_schema_type(x, "array")
  class(x) <- c("schema_arr", "schema", "list")
  x
}

schema_obj = function(properties, descr=NULL, ...) {
  x = nn_locals_to_list(...)
  restore.point("schema_obj")
  if (length(properties)==0) stop("An object schema needs at least one property.")
  x$properties = lapply(properties, to_schema)
  x = add_schema_type(x, "object")

  class(x) <- c("schema_obj", "schema", "list")
  x
}

#' Create an Object Response Template
#'
#' This function creates a template for a single JSON object response.
#' The response is expected to be a JSON object with the provided fields.
#'
#' @param ... Named elements that define the fields for the JSON object.
#'
#' @return A list with class `obj_resp` that serves as a template for an object response.
#'
#' @examples
#' # Create an object response template with three fields.
#' obj_template <- obj_resp(city = "Paris", country = "France", population = 5.2)
#' str(obj_template)
obj_resp <- function(...) {
  x <- list(...)
  class(x) <- c("obj_resp", "list")
  x
}

ex_to_schema_type <- function(x) {
  if (is.null(x)) {
    return("null")
  } else if (inherits(x, "schema_obj")) {
    return("object")
  } else if (inherits(x, "schema_arr")) {
    return("array")
  } else if (is.atomic(x)) {
    if (length(x) == 1) {
      if (is.character(x)) {
        return("string")
      } else if (is.numeric(x)) {
        if (all(x == as.integer(x))) {
          return("integer")
        } else {
          return("number")
        }
      } else if (is.logical(x)) {
        return("boolean")
      } else {
        return("string")
      }
    } else {
      # For an atomic vector with length > 1, treat it as an array.
      return("array")
    }
  } else if (is.list(x)) {
    if (!is.null(names(x)) && any(names(x) != "")) {
      return("object")
    } else {
      return("array")
    }
  } else {
    return("string")
  }
}
