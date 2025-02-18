nn_locals_to_list <- function(ignore_null=TRUE) {
  # Get the environment of the calling function
  caller_env <- parent.frame()

  # List all objects (including hidden ones) in the caller's environment
  object_names <- ls(envir = caller_env, all.names = TRUE)

  # Retrieve the objects as a named list without inheriting from parent environments
  object_list <- mget(object_names, envir = caller_env, inherits = FALSE)
  if (ignore_null) {
    is_null = sapply(object_list, is.null)
    object_list = object_list[!is_null]
  }

  return(object_list)
}


locals_to_list <- function(ignore_null=FALSE) {
  # Get the environment of the calling function
  caller_env <- parent.frame()

  # List all objects (including hidden ones) in the caller's environment
  object_names <- ls(envir = caller_env, all.names = TRUE)

  # Retrieve the objects as a named list without inheriting from parent environments
  object_list <- mget(object_names, envir = caller_env, inherits = FALSE)
  if (ignore_null) {
    is_null = sapply(object_list, is.null)
    object_list = object_list[!is_null]
  }

  return(object_list)
}
