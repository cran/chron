"parse.format"<-
function(format, year.abb = options("chron.year.abb")[[1]], ...)
{
  ## determine order of month, day, year or hour, min, secs
  abb <- TRUE	# short notation?
  mon.abb <- FALSE	# should month names be abbreviated?
  if(is.null(year.abb))
    year.abb <- T
  if((nf <- nchar(format)) == 5) {
    ## abbreviated dates/times
    sep <- substring(format, 2, 2)
    fmt <- substring(format, first = c(1, 3, 5), last = c(1, 3, 5))
  }
  else if(nf == 3) {
    sep <- ""	# no sep
    fmt <- substring(format, first = 1:3, last = 1:3)
  }
  else {
    ## full format (month names)
    abb <- FALSE
    sep <- " "
    fmt <- unlist(unpaste(format, sep = sep))
    mon.abb <- if(any(fmt == "month")) FALSE else TRUE
  }
  periods <- substring(lower.case(fmt), 1, 1)	# m, d, & y in right order
  return(list(abb = abb, sep=sep, periods = periods, 
	      mon.abb = mon.abb, year.abb = year.abb))
}
"convert.dates"<-
function(dates. = NULL, format = "m/d/y", origin., length. = 0, ...)
{
  ## returns a julian vector given various types of input
  if(is.null(dates.) || !length(dates.)) 
    return(numeric(length = length.))
  if(is.numeric(dates.))
    return(dates.)	# assume julian format
  if(!is.character(dates.) && all(!is.na(dates.)))
    stop(paste("object", deparse(substitute(dates.)), 
	       "must be numeric or character"))
  if(!is.character(format)) {
    ## format may be a function or fun name
    FUN <- switch(mode(format),
		  name = get(format, mode = "function"),
		  "function" = format,
		  stop(paste("unrecognized date format",
                             as.character(format))))
    return(FUN(dates., ...))
  }
  if(missing(origin.) && is.null(origin. <- .Options$chron.origin)) 
    origin. <- c(month = 1, day = 1, year = 1970)	
  ## determine sep, order of month, day, year, etc.
  fmt <- parse.format(format)
  out <- unpaste(dates., sep = fmt$sep, fnames = fmt$periods, nfields = 3
		 )
  if(fmt$abb)
    mo <- as.numeric(out$m)
  else mo <- match(lower.case(substring(out$m, 1, 3)),
                   lower.case(month.abb), nomatch = NA)
  yy <- as.numeric(out$y)
  dy <- as.numeric(out$d)
  if(all(is.na(yy) | is.na(dy) | is.na(mo)))
    if(any(as.character(dates.) != "NA"))
      stop(paste("format", format, "may be incorrect"))
    else 
      return(rep(NA, length(dates.)))
  if(any(!is.na(yy)) && fmt$year.abb){
    fun <- options("chron.year.expand")[[1]]
    fun <- switch(mode(fun), 
		  "character" = get(fun, mode = "function"),
		  "name" = eval(fun),
		  "function" = fun,
		  stop(paste("cannot expand 2-digit year abbreviation",
                             "--you must specify \"chron.year.expand\"",
                             "through options()")))
    yy <- fun(yy, ...)
  }
  non.na <- !is.na(mo)	# all months between 1 and 12?
  bad <- seq(along = mo)[non.na][mo[non.na] < 1 | mo[non.na] > 12]
  if(n.bad <- length(bad)) {
    if(n.bad > 10)
      msg <- paste(n.bad, "months out of range set to NA")
    else msg <- paste("month(s) out of range in positions",
                      paste(bad, collapse = ","), "set to NA")
    warning(msg)
    mo[bad] <- NA
    non.na[bad] <- F
  }
  non.na <- non.na & !is.na(dy)
  mon.len <- month.length[mo[non.na]]
  mon.len[leap.year(yy[non.na]) & mo[non.na] == 2] <- 29# leap years!
  ## all days in the proper range (including leap years)?
  bad <- seq(along = dy)[non.na][dy[non.na] < 1 | dy[non.na] > mon.len]
  if(n.bad <- length(bad)) {
    if(n.bad > 10)
      msg <- paste(n.bad, "days out of range set to NA")
    else msg <- paste("days(s) out of range in positions", 
		      paste(bad, collapse = ","), "set to NA")
    warning(msg)
    dy[bad] <- NA
    non.na[bad] <- F
  }
  return(julian(mo, dy, yy, origin = origin.))
}
"format.dates"<-
function(x, format = "m/d/y", origin., simplify = F, ...)
{
  if(!all(is.na(x)) && !is.numeric(x))
    stop(paste("couldn't extract julian dates from object", 
	       deparse(substitute(x))))
  if(is.null(default.orig <- .Options$chron.origin))
    default.orig <- c(month = 1, day = 1, year = 1970)
  att <- attributes(x)
  if(inherits(x, "dates")) {
    if(missing(format))
      format <- switch(mode(att$format),
		       character = ,
		       list = att$format[[1]],
		       name = ,
		       "function" = att$format,
		       NULL = format,
		       stop("invalid output format for dates"))
    if(missing(origin.))
      origin. <- att$origin
  }
  else if(missing(origin.))
    origin. <- default.orig
  if(!is.character(format)) {
    ## format may be a function
    FUN <- switch(mode(format),
		  "function" = format,
		  name = eval(format),
		  stop(paste("unknown date format", as.character(format))
		       ))
    return(FUN(unclass(x), ...))
  }
  v <- month.day.year(trunc(unclass(x)), origin = origin.)
  v$day <- substring(paste("0", v$day, sep = ""), 
		     first = nchar(paste(v$day)))
  if(simplify) {
    drop.year <- length(unique(v$year[!is.na(v$year)])) <= 1
    drop.mon <- simplify > 1 && drop.year && length(unique(v$mon)) <= 
      1
    if(!drop.mon && !drop.year)
      drop.day <- T
  }
  fmt <- parse.format(format[1])
  perm <- fmt$periods
  if(fmt$abb) {
    v$month <- substring(paste("0", v$mon, sep = ""), 
			 first = nchar(paste(v$mon)))
    if(fmt$year.abb){
      v$year <- v$year %% 100
      v$year <- substring(paste("0", v$year, sep=""),
			  first = nchar(paste(v$year)))
    }
  }
  else v$month <- if(fmt$mon.abb) month.abb[v$mon] else month.name[v$mon]
  sep <- fmt$sep
  y <- character(length = length(x))
  if(!simplify)
    y[] <- paste(v[[perm[1]]], v[[perm[2]]], v[[perm[3]]], sep = 
		 sep)
  else {
    ## simplify (drop year/month when all equal)
    if(drop.mon) y[] <- v$day else if(drop.year) {
      perm <- perm[perm != "y"]	# drop years
      y[] <- paste(v[[perm[1]]], v[[perm[2]]], sep = sep)
    }
    else {
      perm <- perm[perm != "d"]	# drop days
      y[] <- paste(v[[perm[1]]], v[[perm[2]]], sep = sep)
    }
  }
  y[is.na(x)] <- NA
  y[x == Inf] <- "Inf"
  y[x ==  - Inf] <- "-Inf"
  att$format <- att$origin <- att$class <- NULL
  attributes(y) <- att
  y
}

"year.strict" <- 
function(...)
  stop("you must expand 2-digit year abbreviations")

"year.expand" <-
function(y, cut.off = 30, century = c(1900, 2000), ...)
{
  ## cut.off specifies year for rounding up/down
  if(!is.numeric(y) || any(y < 0 | y > 99))
    stop("must be 2-digit (numeric) year specification")
  ifelse(y < cut.off, y + century[2], y + century[1])
}
