
# To do: add recursion for nested objects...
schema_fields_to_rclasses = function(x, given_classes=NULL) {
  restore.point("schema_fields_to_rclasses")
  if (is_schema_arr(x)) {
    return(schema_fields_to_rclasses(x$items))
  }
  if (is_schema_obj(x)) {
    x = x$properties
  }
  restore.point("schema_fields_to_rclasses")
  if (is.list(x)) {
    types = sapply(x, function(x) x$type[1])
    class = schema_types_to_rclass(types)
    class[class[!names(class) %in% names(given_classes)]]
    return(c(given_classes,class))
  }
  return(NULL)
}

# converts string vector of json response schema types to a string vector of r classes
schema_types_to_rclass = function(types) {
  # Vectorized conversion using case_when
  r_classes <- case_when(
    tolower(types) == "string" ~ "character",
    tolower(types) == "number" ~ "numeric",
    tolower(types) == "integer" ~ "integer",
    tolower(types) == "boolean" ~ "logical",
    tolower(types) == "bool" ~ "logical",
    tolower(types) == "null" ~ "NULL",
    tolower(types) == "array" ~ "list",
    tolower(types) == "object" ~ "list",
    TRUE ~ "unknown"
  )
  if (!is.null(names(types)))
    names(r_classes) = names(types)
  return(r_classes)
}

valid_json_schema_fields = function(x) {
  if (is_schema_arr(x) | isTRUE(x=="array")) return(c("type", "items", "minItems","maxItems", "uniqueItems"))
  if (is_schema_obj(x) | isTRUE(x=="object")) return(c("type", "properties"))
  if (is_schema_str(x) | isTRUE(x=="string")) return(c("type", "enum", "pattern", "minLength", "maxLength"))
  if (is_schema_int(x) | isTRUE(x=="integer")) return(c("type", "enum", "minimum", "exclusiveMinimum", "maximum", "exclusiveMaximum"))
  if (is_schema_num(x) | isTRUE(x=="numeric")) return(c("type","enum", "minimum", "exclusiveMinimum", "maximum", "exclusiveMaximum"))
  if (is_schema_bool(x) | isTRUE(x=="boolean")) return(c("type"))
  restore.point("valid_json_schema_fields_err")
  stop("valid_json_schema_fields: Unknown x")
}

to_json_schema_obj = function(x, add_description=FALSE, allow_null_def=FALSE, exclude_allow_null_def = c("object","array")) {
  if (is.null(x)) return(NULL)
  restore.point("to_json_schema")
  if ("json_schema" %in% class(x)) return(x)
  if (isTRUE(x$allow_null)) {
    x$type = union(x$type,"null")
  } else if (allow_null_def & !isTRUE(x$allow_null==FALSE)) {
    if (!any(x$type %in% exclude_allow_null_def)) {
      x$type = union(x$type,"null")
    }
  }
  valid_fields = valid_json_schema_fields(x)
  if (add_description) {
    x = change_names(x,"descr","description")
    valid_fields = union(valid_fields, "description")
  }

  x = reduce_to_fields(x, valid_fields)
  if (is_schema_obj(x)) {
    x$properties = lapply(x$properties, to_json_schema_obj, add_description=add_description, allow_null_def=allow_null_def, exclude_allow_null_def = exclude_allow_null_def)
  } else if (is_schema_arr(x)) {
    x$items = to_json_schema_obj(x$items, add_description = add_description, allow_null_def=allow_null_def, exclude_allow_null_def = exclude_allow_null_def)
  }
  class(x) = union("json_schema", class(x))
  x
}

to_json_schema = function(x, add_description=FALSE, allow_null_def=FALSE, exclude_allow_null_def = c("object","array"), pretty=TRUE, auto_unbox=TRUE, ...) {
  if (is.null(x)) return(NULL)
  json_obj = to_json_schema_obj(x, add_description=add_description, allow_null_def = allow_null_def, exclude_allow_null_def = exclude_allow_null_def)
  json = jsonlite::toJSON(json_obj,pretty = pretty,auto_unbox = auto_unbox, ...)
  json
}

#' Select specific properties of a schema object
#' possibly rename
schema_reduce = function(schema, fields) {
  restore.point("schema_reduce")
  if (is_schema_arr(schema)) {
    schema$items = schema_reduce(schema$items, fields)
    return(schema)
  }
  if (!is_schema_obj(schema)) stop("schema_select only works for object or array schemas")

  schema$properties = schema$properties[fields]

  new_names = names(fields)
  if (is.null(new_names)) return(schema)

  empty = (new_names == "")
  new_names[empty] = fields[empty]
  names(schema$properties) = new_names
  schema
}

schema_merge = function(x,y) {
  if (is_schema_arr(x) & is_schema_arr(y)) {
    return(schema_merge(x$properties, y$properties))
  }
  if (!is_schema_obj(x)) stop("schema_merge only works if both x and y are objects or both arrays.")
  if (!is_schema_obj(y)) stop("schema_merge only works if both x and y are objects or both arrays.")

  xfields = names(x$properties)
  yfields = names(y$properties)
  new_fields = setdiff(yfields, xfields)
  x$properties[new_fields] = y$properties[new_fields]
  x
}
