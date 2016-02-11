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

buildParam <- function(param, values) {
  
  test.param <- tryCatch(findParams(param), error = function(e){})
  
  # test
  if(!is.null(values) && !is.null(test.param)) {
    
    
    # fetch valid values for parameter
    valid <- findParams(param = param)
    
    # if invalid stop
    if(!length(values[values %in% valid]) && !is.null(values)) {
      
      vals <- paste0(valid, collapse = ", ")
      
      stop(paste0("invalid parameter, valid values are: ", vals),
           call. = FALSE)
      
    } 
    
  }
  
  # build
  if (!is.null(values)) {
    
    # index of first letter after "."
    index <- gregexpr("\\.", param)[[1]][[1]] + 1
    
    # while "." present in string
    while(index != 0) {
      # capitalise
      substr(param , start = index, stop = index) <- toupper(substring(param, 
                                                                       index, 
                                                                       index))
      
      # replace with space because I can't subtr a f*cking "."
      substr(param, index-1, index-1) <- " "
      
      # remove space
      param <- gsub("[[:space:]]", "", param)
      
      # index of first letter after "."
      index <- gregexpr("\\.", param)[[1]][[1]] + 1
    }
    
    # concatenate string
    param <- paste0("&", param, "=", values)
    
    # else remain NULL
  } else if (is.null(values)) {
    
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

# named list #stackoverflow FTW
namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}

# buildTerms
buildTerms <- function(q) {
  
  # replace space with "+"
  q <- gsub("[[:space:]]", "+", q)
  
  q <- paste0("&q=",q)
  
  return(q)
}

# buildLocation
buildLocation <- function(location) {
  
  location <- paste0(location[1], ",",location[2])
  
  return(location)
  
}

# paginate
paginate <- function(response, n = 50, verbose = FALSE) {
  
  # parse
  json <- jsonlite::fromJSON(rawToChar(response$content),
                             simplifyDataFrame = FALSE)
  
  dat <- do.call(plyr::"rbind.fill", lapply(json$items, as.data.frame))
  
  # number of results 
  res <- json$pageInfo$resultsPerPage
  
  i <- 1
  
  while(res < n && length(json$nextPageToken)) {
    
    # rebuild url
    uri <- paste0(response$url, "&pageToken=", json$nextPageToken)
    
    # fetch
    response <- httr::GET(uri, config = (token = token))
    
    # parse
    json <- jsonlite::fromJSON(rawToChar(response$content),
                               simplifyDataFrame = F)
    
    next.dat <- do.call(plyr::"rbind.fill", lapply(json$items, as.data.frame))
    
    # bind
    dat <- plyr::rbind.fill(dat, next.dat)
    
    # number of results 
    res <- res + json$pageInfo$resultsPerPage
    
    i <- i + 1
    
    # vebose
    if(verbose == TRUE) {
      cat(paste0(res, " results\n"), fill = TRUE, 
          labels = paste0("Query #", i))
    }
    
    # don't hammer that server
    Sys.sleep(0.5)
    
  } 
  
  return(dat)
  
}


testPart <- function(FUN, values) {
  
  test.param <- findParts(FUN)
  
  
  # fetch valid values for parameter
  valid <- findParts(FUN)
  
  # if invalid stop
  if(!length(values[values %in% valid]) && !is.null(values)) {
    
    vals <- paste0(valid, collapse = ", ")
    
    stop(paste0("invalid parameter, valid values are: ", vals),
         call. = FALSE)
  }

}