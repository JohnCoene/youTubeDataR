checkToken <- function(token) {
  # check inputs
  if(missing(token)) {
    stop("token MUST be provided. See youOAuth", call. = FALSE)
  } else if (class(token)[1] != "Token2.0") {
    stop("Wrong token provided. See youOAuth", call. = FALSE)
  } else if (is.null(token)) {
    stop("token is NULL", call. = FALSE)
  }
}


checkQuery <- function(q) {
  if(missing(q)) {
    stop("query MUST be provided", call. = FALSE)
  } else if (is.null(q)) {
    stop("query is NULL", call. = FALSE)
  }
}

buildParam <- function(param, values) {
  
  # fetch valid values
  valid <- findParams(param = param)
  
  if(!length(values[values %in% valid]) && !is.null(values)) {
    
    vals <- paste0(valid, collapse = ", ")
    
    stop(paste0("invalid parameter, valid values are: ", vals),
         call. = FALSE)
    
  } else if (length(values[values %in% valid]) && !is.null(values)) {
    
    # index of first letter after ".
    index <- gregexpr("\\.", param)[[1]][[1]] + 1
    
    # capitalise
    substr(param , start = index, stop = index) <- toupper(substring(param, 
                                                                     index, 
                                                                     index))
    
    # remove dot
    param <- gsub("\\.", "", param)
    
    # concatenate string
    param <- paste0("&", param, "=", values)
    
  } else if (is.null(param)) {
    param <- NULL
  }
  
  return(param)
}

buildTime <- function(t) {
  
  if(class(t)[1] == "POSIXlt" || class(t)[1] == "POSIXct"){
    
    # to character
    t <- as.character(t)
    
    # add T
    t <- gsub(" ", "T", t)
    
    # add Z
    t <- paste0(t, "Z")
    
  } else if(is(t, "Date")) {
    
    t <- paste0(t, "T00:00:00Z")
    
  } else if (nchar(t) == "20" && !is.null(t)) {
    
    t <- t
    
  } else if (is.null(t)) {
    
    t <- NULL
    
  } else {
    
    stop("Wrong date/time format suplied", call. = FALSE)
    
  }
  
  return(t)
  
}
