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

to_json_schema = function(x, add_description=FALSE) {
  if (is.null(x)) return(NULL)
  restore.point("to_json_schema")
  if ("json_schema" %in% class(x)) return(x)
  if (isTRUE(x$allow_null)) x$type = union(x$type,"null")
  valid_fields = valid_json_schema_fields(x)
  if (add_description) {
    x = change_names(x,"descr","description")
    valid_fields = union(valid_fields, "description")
  }

  x = reduce_to_fields(x, valid_fields)
  if (is_schema_obj(x)) {
    x$properties = lapply(x$properties, to_json_schema, add_description=add_description)
  } else if (is_schema_arr(x)) {
    x$items = to_json_schema(x$items, add_description = add_description)
  }
  class(x) = union("json_schema", class(x))
  x
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
