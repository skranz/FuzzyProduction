# Function to preserve a function call in a serializable format
preserve_call <- function(fun_name=NULL) {
  # Get the parent frame
  parent_frame <- parent.frame(1)
  
  # Get function from parent frame
  parent_function <- sys.function(-1)
  
  # Try to find the function name by matching in the global environment
  fun_name <- NULL
  if (is.null(fun_name)) {
    for (name in ls(.GlobalEnv)) {
      if (identical(parent_function, get(name, envir = .GlobalEnv))) {
        fun_name <- name
        break
      }
    }
  }
  
  # If we couldn't find it in the global environment, check the search path
  if (is.null(fun_name)) {
    for (pkg in search()) {
      if (pkg == ".GlobalEnv") next # Already checked
      pkg_env <- as.environment(pkg)
      for (name in ls(pkg_env)) {
        if (identical(parent_function, get(name, envir = pkg_env))) {
          fun_name <- name
          break
        }
      }
      if (!is.null(fun_name)) break
    }
  }
  
  # Throw error if function name could not be determined
  if (is.null(fun_name)) {
    stop("Could not determine function name. Function must be defined in the global environment or a loaded package.")
  }
  
  # Get formal arguments of the parent function to know what to capture
  fun_formals <- names(formals(parent_function))
  
  # Get the values of those arguments from the parent frame
  args_to_capture <- mget(fun_formals, envir = parent_frame, ifnotfound = list(NULL))
  
  # Create the preserved call object
  fun_call <- list(
    fun_name = fun_name,
    args = args_to_capture
  )
  
  class(fun_call) <- "preserved_call"
  return(fun_call)
}

# Function to run a preserved call
run_preserved_call <- function(fun_call) {
  if (!inherits(fun_call, "preserved_call")) {
    stop("Input must be a preserved_call object")
  }
  
  # Look up the function by name
  fun_to_call <- tryCatch({
    get(fun_call$fun_name, envir = .GlobalEnv)
  }, error = function(e) {
    # Try to find it in the search path
    for (pkg in search()) {
      if (exists(fun_call$fun_name, where = pkg, mode = "function")) {
        return(get(fun_call$fun_name, pos = pkg))
      }
    }
    stop(paste("Function", fun_call$fun_name, "not found. Make sure it exists in the global environment or a loaded package."))
  })
  
  # Execute the call with do.call
  do.call(fun_to_call, fun_call$args)
}
