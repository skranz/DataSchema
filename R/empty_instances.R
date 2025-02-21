
#' Build an empty instance of a schema
#'
#' In particular relevant for a schema_arr since
#' an empty tibble can be used with dplyr commands
#' while NULL typically throws errors.
schema_empty_instance = function(x) {
  if (is.null(x))  return(NULL)
  if (is_schema_obj(x)) {
    return(lapply(x$properties, schema_empty_instance))
  }
  if (is_schema_arr(x)) {
    empty_item = schema_empty_instance(x$items)
    if (is.list(empty_item)) empty_item = as_tibble(empty_item)
    return(empty_item)
  }
  if (is_schema_str(x)) return(character(0))
  if (is_schema_int(x)) return(integer(0))
  if (is_schema_num(x)) return(numeric(0))
  if (is_schema_bool(x)) return(logical(0))
  restore.point("schema_empty_instance_err")
  stop(no_schema_obj_msg(x))
}



schema_na_instance = function(x, len=1) {
  if (is.null(x))  return(NULL)
  if (is_schema_obj(x)) {
    return(lapply(x$properties, schema_na_instance, len=len))
  }
  if (is_schema_arr(x)) {
    na_item = schema_na_instance(x$items)
    if (is.list(na_item)) na_item = as_tibble(na_item)
    return(na_item)
  }
  if (is_schema_str(x)) return(rep(NA_character_,len))
  if (is_schema_int(x)) return(rep(NA_integer_,len))
  if (is_schema_num(x)) return(rep(NA_real_,len))
  if (is_schema_bool(x)) return(rep(NA,len))
  restore.point("schema_na_instance_err")
  stop(no_schema_obj_msg(x))
}
