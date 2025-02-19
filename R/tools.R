nn_locals_to_list <- function(...) {
  # Get the environment of the calling function
  caller_env <- parent.frame()

  restore.point("shkhsdfhk")
  # Retrieve all objects (including hidden ones) from the caller's environment
  local_objs <- mget(ls(envir = caller_env, all.names = FALSE),
                     envir = caller_env, inherits = FALSE)

  local_objs = local_objs[setdiff(names(local_objs), "...")]

  # Capture any objects passed via ... to locals_to_list
  extra <- list(...)
  object_list = c(local_objs, extra)


  is_null = sapply(object_list, is.null)
  object_list = object_list[!is_null]

  return(object_list)
}

