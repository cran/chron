".Holidays"<-
structure(.Data = c(8035, 8180, 8220, 8285, 8365, 8394), format = structure(
	.Data = "m/d/y", .Names = "dates"), origin = structure(.Data = c(1, 1, 
	1970), .Names = c("month", "day", "year")), class = c("dates", "times"),
	.Names = c("New Year's Day", "Memorial Day", "Independence Day", 
	"Labor Day", "Thanksgiving", "Christmas"))
"Math.dates"<-
function(x, ...)
{
	ok <- switch(.Generic,
		trunc = ,
		round = ,
		signif = ,
		ceiling = ,
		floor = TRUE,
		FALSE)
	if(!ok)
		stop(paste(.Generic, "not defined for dates objects"))
	cl <- class(x)
	class(x) <- NULL
	out <- NextMethod(.Generic)
	class(out) <- cl
	out
}
"Math.times"<-
function(x, ...)
{
	cl <- class(x)
	class(x) <- NULL
	out <- NextMethod(.Generic)
	class(out) <- cl
	out
}
"Ops.dates"<-
function(e1, e2)
{
	ok <- switch(.Generic,
		"+" = ,
		"-" = ,
		"<" = ,
		">" = ,
		"==" = ,
		"!=" = ,
		"<=" = ,
		">=" = TRUE,
		FALSE)
	if(nargs() == 1) {
# unary operators (only + is valid)
		if(.Generic == "+") return(e1) else stop(paste("unary", 
				.Generic, "not defined for chron objects"))
	}
	if(!ok)
		stop(paste(.Generic, "not defined for chron objects"))
	dates.flg <- nchar(.Method)
	if(is.character(e1)) {
		e1 <- chron(e1, format = attr(e2, "format"), origin = origin(e2
			))
		dates.flg[1] <- T
	}
	if(is.character(e2)) {
		e2 <- chron(e2, format = attr(e1, "format"), origin = origin(e1
			))
		dates.flg[2] <- T
	}
	scalar <- !all(dates.flg)	# scalar operand?
	o1 <- origin(e1)
	o2 <- origin(e2)
	if(!scalar) {
		if(.Generic == "+")
			stop("chron objects may not be added together")
		if(any(o1 - o2)) {
			warning("different origin in dates arithmetic")
			origin(e2) <- o2 <- o1
		}
	}
	val <- NextMethod(.Generic)
	boolean <- match(.Generic, c("==", "!=", ">", ">=", "<", "<="), nomatch
		 = 0)
	if(boolean)
		return(val)	# make sure origin wasn't dropped
	if(!inherits(val, "dates")) {
		attr(val, "origin") <- if(dates.flg[1]) o1 else o2
		class(val) <- c(.Class, class(val))
	}
	tms <- as.vector(val)
	tmp <- tms - trunc(tms)	
	# If a fractional scalar operand, then dates become chrons
	if(scalar && length(tmp <- tmp[!is.na(tmp)]) && any(tmp != 0)) {
		if(length(fmt.val <- attr(val, "format")) < 2)
			attr(val, "format") <- c(fmt.val, "h:m:s")
		class(val) <- c("chron", "dates", "times")
	}
# dates - dates is days
	if(!scalar && inherits(val, "dates")) {
		if(length(fmt.val <- attr(val, "format")) < 2)
			attr(val, "format") <- "h:m:s"
		else attr(val, "format") <- rev(attr(val, "format"))[[1]]
		attr(val, "origin") <- NULL
		val <- times(val)
	}
	val
}
"Ops.times"<-
function(e1, e2)
{
	if(nargs() == 1) {
# unary operators
		val <- switch(.Generic,
			"-" = -1 * e1,
			"+" = e1,
			"!" = !as.logical(e1))
		return(val)
	}
	if(is.character(e1))
		e1 <- chron(times = e1, format = attr(e2, "format"))
	if(is.character(e2))
		e2 <- chron(times = e2, format = attr(e1, "format"))
	val <- NextMethod(.Generic)
	boolean <- match(.Generic, c("==", "!=", ">", ">=", "<", "<="), nomatch
		 = 0)
	if(boolean) return(as.logical(val))	
	# make sure the format attribute wasn't dropped by NextMethod (p.144 blue book)
	if(is.null(attr(val, "format"))) {
		if(is.null(fmt <- attr(e1, "format")))
			fmt <- attr(e2, "format")
		attr(val, "format") <- fmt
	}
	if(!inherits(val, .Class))
		class(val) <- c(.Class, class(val))
	val
}
"Summary.times"<-
function(x, ...)
{
	val <- NextMethod(.Generic)
	if(.Generic == "all" || .Generic == "any")
		return(as.logical(val))
	attr(val, "format") <- attr(x, "format")
	class(val) <- class(x)
	val
}
"[.times"<-
function(x, ..., drop = T)
{
	cl <- class(x)
	class(x) <- NULL
	val <- NextMethod("[")
	attr(val, "format") <- attr(x, "format")
	attr(val, "origin") <- attr(x, "origin")
	class(val) <- cl
	val
}
"[<-.dates"<-
function(x, ..., value)
{
    if(!as.logical(length(value)))
        return(x)                       # as per p.104 in the blue book
    if(!is.numeric(value) && !is.character(value) && !all(is.na(value)))
        stop("replacement of/with chron objects must be with times objects"
             )
    ox <- origin(x)
    fmt <- attr(x, "format")
    if(!inherits(value, "dates"))
        value <- chron(value, format = fmt, origin = ox)
    else if(any(ox != origin(value)))
        origin(value) <- ox
    cl <- class(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    attr(x, "format") <- fmt
    attr(x, "origin") <- ox
    class(x) <- cl
    x
}
"[<-.times"<-
function(x, ..., value)
{
    if(!as.logical(length(value)))
        return(x)                       # as per p.104 in the blue book
    if(!is.numeric(value) && !is.character(value) && !all(is.na(value)))
        stop("replacement of/with times objects must be with times objects"
             )
    fmt <- attr(x, "format")
    if(!inherits(value, "times"))
        value <- chron(times = value, format = rev(fmt)[[1]])
    cl <- class(x)                      # ensure that dates objects have
                                        # equal origins
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    attr(x, "format") <- fmt
    class(x) <- cl
    x
}
"[[.times"<-
function(x, ..., drop = T)
{
	cl <- class(x)
	class(x) <- NULL
	val <- NextMethod("[[")
	attr(val, "format") <- attr(x, "format")
	attr(val, "origin") <- attr(x, "origin")
	class(val) <- cl
	val
}
"all.equal.dates"<-
function(..., tolerance = 1/(10 * 24 * 60 * 60))
NextMethod("all.equal", ..., tolerance = tolerance)
"as.character.times"<-
function(x, ...)
format(x, ...)
"as.chron"<-
function(x, ...)
{
	if(inherits(x, "chron"))
		return(x)
	if(is.character(x) || is.numeric(x))
		return(chron(x, ...))
	if(all(is.na(x)))
		return(x)	# all na's
}
"axis.times"<-
function(n, x, add = T, labels, simplify = T, ...)
{
	if(!inherits(x, "times"))
		x <- chron(x)
	bad <- is.na(x) | abs(as.vector(x)) == Inf
	rng <- if(n == 1 || n == 3) par("usr")[1:2] else par("usr")[3:4]
	tmp <- pretty(c(rng, as.numeric(x[!bad])))
	att <- attributes(x)
	at.x <- structure(tmp[tmp >= rng[1] & tmp <= rng[2]], format = att$
		format, origin = att$origin, class = att$class)
	if(inherits(at.x, "chron")) class(at.x) <- class(at.x)[-1]	
	# chrons put dates labels only
# force in data ends for times objects (the next block is a kludge!)
# if plotting times only, fake the time 1.0 to print as midnight
	if(missing(labels) || (is.logical(labels) && labels)) {
		if(!inherits(x, "dates")) {
			at.x[c(1, length(at.x))] <- range(x)
			if(max(at.x) == 1)
				labels <- format(at.x - trunc(at.x), simplify
				   = simplify)
			else labels <- format(at.x, simplify = simplify)
		}
		else labels <- format(at.x, simplify = simplify)
	}
	if(add)
		axis(n, at = at.x, labels = labels, ...)
	invisible(list(n = n, at = at.x, labels = labels))
}
"c.dates"<-
function(..., recursive = FALSE)
{
# output will have the format and origin corresponding to the
# argument with earliest origin
	dots <- list(...)
	is.dts <- unlist(lapply(dots, inherits, "dates"))
	o <- matrix(unlist(lapply(dots, origin)), nrow = 3)
	all.orig <- julian(o[1,  ], o[2,  ], o[3,  ], origin = c(0, 0, 0))
	earliest <- min(all.orig)
	mdy <- month.day.year(earliest, origin = c(0, 0, 0))
	orig <- c(mdy$month, mdy$day, mdy$year)
	n <- length(dots)
	fmt <- attr(dots[[(1:n)[is.dts][match(earliest, all.orig)]]], "format")
	out <- vector("list", length = n)
	for(i in 1:n) {
		x <- dots[[i]]	
	# note that NA's don't need any further processing
		if(!all(is.na(x))) {
			if(is.dts[i]) {
				if(any(origin(x) != orig))
				  origin(x) <- orig
			}
			else x <- chron(x, format = fmt, origin = orig)
		}
		out[i] <- list(x)
	}
	out <- chron(unlist(out, use.names = F), origin = orig, format = fmt)
	out
}
"c.times"<-
function(..., recursive = FALSE)
{
	dots <- list(...)
	is.tms <- unlist(lapply(dots, inherits, "times"))
	n <- length(dots)
	fmt <- attr(dots[[(1:n)[is.tms][1]]], "format")
	if(is.null(fmt))
		fmt <- "h:m:s"
	out <- vector("list", length = n)
	for(i in 1:n) {
		x <- dots[[i]]
		if(!all(is.na(x)))
			x <- convert.times(x)
		out[i] <- list(x)
	}
	out <- times(unlist(out, use.names = F), format = fmt)
	out
}
"chron"<-
function(dates. = NULL, times. = NULL, format = c(dates = "m/d/y", times = 
	"h:m:s"), out.format = format, origin.)
{
	given <- c(dates = !missing(dates.), times = !missing(times.))
	if(is.null(default.origin <- options()$chron.origin))
		default.origin <- c(month = 1, day = 1, year = 1970)
	if(all(!given))
		return(structure(numeric(0)	# dates and times missing
		, format = format, origin = default.origin, class = c("chron", 
			"dates", "times")))
	if(inherits(dates., "dates")) {
		if(missing(origin.))
			origin. <- origin(dates.)
		else origin(dates.) <- origin.
	}
	else if(missing(origin.))
		origin. <- default.origin
	if(given["dates"] && !given["times"]) {
# presumably only dates
		if(missing(format) && inherits(dates., "dates")) format <- attr(
				dates., "format")
		fmt <- switch(mode(format),
			character = ,
			list = format[[1]],
			name = ,
			"function" = format,
			NULL = c(dates = "m/d/y"),
			stop("unrecognized format"))
		dts <- convert.dates(dates., format = fmt, origin = origin.)
		tms <- dts - trunc(dts)	
	# if dates include fractions of days create a full chron
		if(!all(is.na(tms)) && any(tms[!is.na(tms)] != 0))
			return(chron(dates. = trunc(dts), times. = tms, format
				 = format, out.format = out.format, origin = 
				origin.))
		ofmt <- switch(mode(out.format),
			character = ,
			list = out.format[[1]],
			name = ,
			"function" = out.format,
			NULL = c(dates = "m/d/y"),
			stop("invalid output format"))
		attr(dts, "format") <- ofmt
		attr(dts, "origin") <- origin.
		class(dts) <- c("dates", "times")
		names(dts) <- names(dates.)
		return(dts)
	}
	if(given["times"] && !given["dates"]) {
# only times
		if(missing(format) && inherits(times., "times")) {
			format <- attr(times., "format")
			if(!is.name(format))
				format <- rev(format)[[1]]
		}
		fmt <- switch(mode(format),
			character = ,
			list = rev(format)[[1]],
			name = ,
			"function" = format,
			NULL = c(times = "h:m:s"),
			stop("invalid times input format"))
		tms <- convert.times(times., fmt)
		ofmt <- switch(mode(out.format),
			character = ,
			list = rev(out.format)[[1]],
			name = ,
			"function" = out.format,
			NULL = c(dates = "m/d/y"),
			stop("invalid times output format"))
		attr(tms, "format") <- ofmt
		class(tms) <- "times"
		names(tms) <- names(times.)
		return(tms)
	}
# both dates and times 
	if(length(times.) != length(dates.))
		stop(paste(deparse(substitute(dates.)), "and", deparse(
			substitute(times.)), "must have equal lengths"))
	if(missing(format)) {
		if(is.null(fmt.d <- attr(dates., "format")))
			fmt.d <- format[1]
		if(is.null(fmt.t <- attr(times., "format")))
			fmt.t <- format[2]
		if(mode(fmt.d) == "character" && mode(fmt.t) == "character")
			format <- structure(c(fmt.d, fmt.t), names = c("dates", 
				"times"))
		else {
			fmt.d <- if(is.name(fmt.d)) fmt.d else fmt.d[[1]]
			fmt.t <- if(is.name(fmt.t)) fmt.t else rev(fmt.t)[[1]]
			format <- list(dates = fmt.d, times = fmt.t)
		}
	}
	if(any(length(format) != 2, length(out.format) != 2))
		stop("misspecified chron format(s) length")
	if(all(mode(format) != c("character", "list")))
		stop("misspecified input format(s)")
	if(all(mode(out.format) != c("list", "character")))
		stop("misspecified output format(s)")
	dts <- convert.dates(dates., format = format[[1]], origin = origin.)
	tms <- convert.times(times., format = format[[2]])
	x <- unclass(dts) + unclass(tms)
	attr(x, "format") <- out.format
	attr(x, "origin") <- origin.
	class(x) <- c("chron", "dates", "times")
	nms <- paste(names(dates.), names(times.))
	if(length(nms) && any(nms != ""))
		names(x) <- nms
	return(x)
}
"clock2frac"<-
function(str)
{
	h <- as.numeric(substring(str, 1, 2))
	m <- as.numeric(substring(str, 4, 5))
	w <- substring(str, 6, 7)
	if(any(h < 0, h > 12, m < 0, m > 59))
		stop("misspecified time")
	pm <- w == "pm" | w == "PM"
	h[pm] <- h[pm] + 12
	f <- (h * 3600 + m * 60)/(24 * 3600)
	f
}
"convert.chron"<-
function(x, format = c(dates = "m/d/y", times = "h:m:s"), origin., sep = " ", 
	enclose = c("(", ")"), ...)
{
	if(is.null(x) || !as.logical(length(x)))
		return(numeric(length = 0))
	if(is.numeric(x))
		return(x)
	if(!is.character(x) && all(!is.na(x)))
		stop(paste("objects", deparse(substitute(x)), 
			"must be numeric or character"))
	if(length(format) != 2)
		stop("format must have length==2")
	if(missing(origin.) && is.null(origin. <- options()$chron.origin))
		origin. <- c(month = 1, day = 1, year = 1970)
	if(any(enclose != ""))
		x <- substring(x, first = 2, last = nchar(x) - 1)
	str <- unpaste(x, sep = sep)
	dts <- convert.dates(str[[1]], format = format[[1]], origin = origin., 
		...)
	tms <- convert.times(str[[2]], format = format[[2]], ...)
	dts + tms
}
"convert.dates"<-
function(dates. = NULL, format = "m/d/y", origin., length. = 0, ...)
{
# returns a julian vector given various types of input
	if(is.null(dates.) || !as.logical(length(dates.))) return(numeric(length = length.)
			)
	if(is.numeric(dates.))
		return(dates.)	# assume julian format
	if(!is.character(dates.) && all(!is.na(dates.)))
		stop(paste("object", deparse(substitute(dates.)), 
			"must be numeric or character"))
	if(!is.character(format)) {
# format may be a function or fun name
		FUN <- switch(mode(format),
			name = get(format, mode = "function"),
			"function" = format,
			stop(paste("unrecognized date format", as.character(
				format))))
		return(FUN(dates., ...))
	}
	if(missing(origin.) && is.null(origin. <- options()$chron.origin)) 
			origin. <- c(month = 1, day = 1, year = 1970)	
	# determine sep, order of month, day, year, etc.
	fmt <- parse.format(format)
	out <- unpaste(dates., sep = fmt$sep, fnames = fmt$periods, nfields = 3
		)
	if(fmt$abb)
		mo <- as.numeric(out$m)
	else mo <- match(lower.case(substring(out$m, 1, 3)), lower.case(
			month.abb), nomatch = NA)
	yy <- as.numeric(out$y)
	dy <- as.numeric(out$d)
	if(all(is.na(yy) | is.na(dy) | is.na(mo)))
		if(any(as.character(dates.) != "NA"))
			stop(paste("format", format, "may be incorrect"))
		else return(rep(NA, length(dates.)))
	if(!all(is.na(yy)) && all(yy[!is.na(yy)] < 100))
		yy <- yy + 1900
	non.na <- !is.na(mo)	# all months between 1 and 12?
	bad <- seq(along = mo)[non.na][mo[non.na] < 1 | mo[non.na] > 12]
	if(n.bad <- length(bad)) {
		if(n.bad > 10)
			msg <- paste(n.bad, "months out of range set to NA")
		else msg <- paste("month(s) out of range in positions", paste(
				bad, collapse = ","), "set to NA")
		warning(msg)
		mo[bad] <- NA
		non.na[bad] <- F
	}
	non.na <- non.na & !is.na(dy)
	mon.len <- month.length[mo[non.na]]
	mon.len[leap.year(yy[non.na]) & mo[non.na] == 2] <- 29	# leap years!
# all days in the proper range (including leap years)?
	bad <- seq(along = dy)[non.na][dy[non.na] < 1 | dy[non.na] > mon.len]
	if(n.bad <- length(bad)) {
		if(n.bad > 10)
			msg <- paste(n.bad, "days out of range set to NA")
		else msg <- paste("days(s) out of range in positions", paste(
				bad, collapse = ","), "set to NA")
		warning(msg)
		dy[bad] <- NA
		non.na[bad] <- F
	}
	return(julian(mo, dy, yy, origin = origin.))
}
"convert.times"<-
function(times = NULL, format = "h:m:s", length. = 0, ...)
{
# convert time in hours, min and secs into fraction of days
	if(is.null(times) || !as.logical(length(times))) return(numeric(length = length.))
	if(is.numeric(times))
		return(times)
	if(!is.character(format)) {
# format may be a function
		FUN <- switch(mode(format),
			name = get(format, mode = "function"),
			functions = format,
			stop(paste("unrecognized format mode", as.character(
				format))))
		return(FUN(times, ...))
	}
	fmt <- parse.format(format)
	out <- unpaste(times, sep = fmt$sep, fnames = fmt$periods, nfields = 3)
	hh <- mm <- ss <- as.numeric(rep(NA, length(out$h)))
	ok <- out$h != "NA" & out$m != "NA" & out$s != "NA"
	hh[ok] <- as.numeric(out$h[ok])
	mm[ok] <- as.numeric(out$m[ok])
	ss[ok] <- as.numeric(out$s[ok])
	if(all(is.na(hh) | is.na(mm) | is.na(ss)))
		if(any(times != "NA"))
			stop(paste("format", format, "may be incorrect"))
		else return(rep(NA, length(times)))
	i <- hh[ok] < 0 | hh[ok] > 23 | mm[ok] < 0 | mm[ok] > 59 | ss[ok] < 0 | 
		ss[ok] > 59
	bad <- seq(along = hh)[ok][i]
	if(n.bad <- length(bad)) {
		if(n.bad > 10)
			msg <- paste(n.bad, 
				"time-of-day entries out of range set to NA")
		else msg <- paste("time-of-day entries out of range in positions",
				paste(bad, collapse = ","), "set to NA")
		warning(msg)
		hh[bad] <- mm[bad] <- ss[bad] <- NA
		ok[bad] <- F
	}
	out <- 3600 * hh + 60 * mm + ss
	out/(24 * 3600)	# return days and fraction of days
}
"count.events"<-
function(x, by)
table(cut(x, breaks = by))
"count.fields.str"<-
function(str, sep = "")
{
	n <- length(str)
	white.space <- missing(sep) || sep == ""
	## load.if.needed("chron_strs.o")
	.C(NAME = "cnt_flds_str",
		strings = as.character(str),
		nstrings = as.integer(n),
		sep = as.character(sep),
		white.space = as.integer(white.space),
		counts = integer(n))$count
}
####"cut"<-
####function(x, ...)
####  ##KH UseMethod("cut", x, ...)
####  UseMethod("cut")
"cut.dates"<-
function(x, breaks, labels, start.on.monday = T)
{
	if(!inherits(x, "dates"))
		x <- chron(x)
	n <- length(breaks)	# dates breaks may be either numeric of character
	if(n > 1) {
		if(!inherits(breaks, "dates")) breaks <- sort(chron(dates = 
				breaks))	
	# make sure x and breaks have same origin
		org <- origin(x)
		if(!is.null(o <- origin(breaks)) && any(o != org))
			origin(breaks) <- org
		breaks <- as.numeric(breaks)
		if(missing(labels))
			labels <- paste("Range", seq(along = breaks[-1]))
		out <- cut.default(x, breaks = breaks, labels = labels)
		out <- ordered(as.character(out), levels = levels(out), labels
			 = labels)
		return(out)
	}
	if(n < 1) stop(paste(deparse(substitute(breaks)), 
			"must have length > 0"))	
	# breaks is either number or a string
	if(is.numeric(breaks)) {
		x <- as.numeric(x)
		if(inherits(breaks, "times"))
			breaks <- unclass(breaks)
		out <- NextMethod("cut")
		return(ordered(out))
	}
# we have a character string 
	valid <- c("days", "weeks", "months", "years")
	if(!as.logical(i <- pmatch(breaks[1], valid, 0)))
		stop(paste("unrecognized time period (", breaks, 
			"), must be one of", paste(valid, collapse = ","), 
			collapse = " "))
	by <- valid[i]
	bump <- c(1, 7, 31, 365)[i]	# force a full period for last obs.
	from <- min(x)
	orig <- origin(x)
	mdy <- month.day.year(as.numeric(from), origin = orig)
	from <- switch(by,
		days = from,
		weeks = from - day.of.week(mdy$m, mdy$d, mdy$y) + as.numeric(
			start.on.monday),
		months = chron(paste(mdy$month, "01", mdy$year - 1900, sep = 
			"/"), format = "m/d/y", origin = orig),
		years = chron(paste("01", "01", mdy$year - 1900, sep = "/"), 
			format = "m/d/y", origin = orig))
        if(from == min(x))
          from <- from - .Machine$double.eps
	breaks <- brk <- seq(from = from, to = max(x) + bump, by = by)
	breaks <- as.numeric(breaks)
	n <- length(breaks)
	x <- as.numeric(x)
	if(missing(labels)) {
		labels <- switch(by,
			days = paste("day", seq(along = breaks[ - n] + 1)),
			weeks = paste("week", seq(along = breaks[ - n] + 1)),
			months = paste(as.character(months(brk[ - n] + 1)), 
				substring(as.character(years(brk[ - n] + 1)), 3,
				4)),
			years = substring(as.character(years(brk[ - n] + 1)), 3,
				4))
	}
	##KH out <- cut.default(x, breaks = breaks, labels = labels)
        out <- cut.default(x, breaks = breaks, labels = labels, right = FALSE)
	ordered(as.character(out), levels = levels(out), labels = labels)
}
####"cut.default"<-
####function(x, breaks, labels)
####{
####	if(length(breaks) == 1) {
####		if(breaks < 1)
####			stop("Must specify at least one interval")
####		if(missing(labels))
####			labels <- paste("Range", seq(length = breaks))
####		else if(length(labels) != breaks)
####			stop("Number of labels must equal number of intervals")
####		r <- range(x[!is.na(x)])
####		r[is.na(r)] <- 1
####		if((d <- diff(r)) == 0) {
####			r[2] <- r[1] + 1
####			d <- 1
####		}
####		breaks <- seq(r[1] - 0.01 * d, r[2] + 0.01 * d, length = breaks +
####			1)
####	}
####	else {
####		breaks <- sort(breaks)
####		if(missing(labels))
####			labels <- paste(format(breaks[ - length(breaks)]), 
####				"+ thru ", format(breaks[-1]), sep = "")
####		else if(length(labels) != length(breaks) - 1)
####			stop("Number of labels must be 1 less than number of break points"
####				)
####	}
####	ans <- .C("binning",
####		x = as.double(x),
####		length(x),
####		as.double(breaks),
####		length(breaks),
####		NAOK = TRUE)$x
####	ans <- ceiling(ans) - 1
####	storage.mode(ans) <- "integer"
####	ans[ans <= 0 | ans >= length(breaks)] <- NA
####	attr(ans, "levels") <- labels
####	ans
####}
"dates"<-
function(x, ...)
trunc(chron(dates. = x, ...))
"day.abb"<-
c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
"day.name"<-
c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
"day.of.week"<-
function(month, day, year)
{
	ix <- year + trunc((month - 14)/12)
	jx <- trunc((13 * (month + 10 - (month + 10) %/% 13 * 12) - 1)/5) + day +
		77 + (5 * (ix - (ix %/% 100) * 100)) %/% 4 + ix %/% 400 - (ix %/% 
		100) * 2
	jx %% 7
}
"days"<-
function(x)
{
	if(!inherits(x, "dates"))
		if((is.character(x) || is.numeric(x)))
			x <- chron(x)
		else return(NULL)
	d <- month.day.year(trunc(as.numeric(x)), origin = origin(x))$day	
	# use paste to avoid bug in ordered() as in beta release 8/92
	d <- ordered(paste(d), paste(1:31))
	d
}
####"diff"<-
####function(x, ...)
####  ##KH UseMethod("diff", x, ...)
####  UseMethod("diff")
####"diff.default"<-
####function(x, lag = 1, differences = 1)
####{
####	if(lag < 1 | differences < 1)
####		stop("Bad value for lag or differences")
####	if(lag * differences >= length(x))
####		return(x[0])
####	r <- x
####	s <- 1:lag
####	for(i in 1:differences)
####		r <- r[ - s] - r[ - (length(r) + 1 - s)]
####	tspx <- tsp(x)
####	if(length(tspx) > 0)
####		ts(as.vector(r), start = tspx[1] + (lag * differences)/tspx[3], 
####			frequency = tspx[3])
####	else as.vector(r)
####}
"diff.times"<-
function(x, lag = 1, differences = 1)
{
# delete references to time-series
	if(lag < 1 | differences < 1) stop("Bad value for lag or differences")
	if(lag * differences >= length(x))
		return(x[0])
	r <- x
	s <- 1:lag
	for(i in 1:differences)
		r <- r[ - s] - r[ - (length(r) + 1 - s)]
	r
}
####"format"<-
####function(x, ...)
####UseMethod("format")
"format.chron"<-
function(x, format = att$format, origin. = att$origin, sep = " ", simplify, 
	enclosed = c("(", ")"))
{
	att <- attributes(x)
	if(missing(simplify))
		if(is.null(simplify <- options("chron.simplify")[[1]]))
			simplify <- F
	dts <- format.dates(x, format[[1]], origin = origin., simplify = 
		simplify)
	tms <- format.times(x - trunc(x), format[[2]], simplify = simplify)
	x <- paste(enclosed[1], dts, sep, tms, enclosed[2], sep = "")	
	# output is a character object w.o class
	att$class <- att$format <- att$origin <- NULL
	attributes(x) <- att
	x
}
"format.dates"<-
function(x, format = "m/d/y", origin., simplify = F, ...)
{
	if(!all(is.na(x)) && !is.numeric(x))
		stop(paste("couldn't extract julian dates from object", deparse(
			substitute(x))))
	if(is.null(default.orig <- options()$chron.origin))
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
# format may be a function
		FUN <- switch(mode(format),
			"function" = format,
			name = eval(format),
			stop(paste("unknown date format", as.character(format))
				))
		return(FUN(unclass(x), ...))
	}
	v <- month.day.year(trunc(unclass(x)), origin = origin.)
	v$day <- substring(paste("0", v$day, sep = ""), first = nchar(paste(v$
		day)))
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
		v$month <- substring(paste("0", v$mon, sep = ""), first = nchar(
			paste(v$mon)))
		v$year <- v$year - 1900
	}
	else v$month <- if(fmt$mon.abb) month.abb[v$mon] else month.name[v$mon]
	sep <- fmt$sep
	##KH y <- character(length = length(x))
	y <- character(length(x))
	if(!simplify)
		y[] <- paste(v[[perm[1]]], v[[perm[2]]], v[[perm[3]]], sep = 
			sep)
	else {
# simplify (drop year/month when all equal)
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
####"format.default"<-
####function(x)
####{
####	z <- .C("pratom",
####		list(as.vector(x)),
####		value = list(character(length(x))),
####		quote = F)$value[[1]]
####	attributes(z) <- attributes(x)
####	z
####}
"format.times"<-
function(x, format. = "h:m:s", simplify = F, ...)
{
	if(!as.logical(length(x)))
		return("")
	if(all(is.na(x)))
		return(rep("NA", length = length(x)))
	if(!is.numeric(x))
		stop(paste(deparse(substitute(x)), "must be numeric"))
	att <- attributes(x)
	if(inherits(x, "times")) {
		if(missing(format.))
			format. <- switch(mode(att$format),
				character = ,
				list = rev(att$format)[[1]],
				name = ,
				"function" = att$format,
				NULL = format.,
				stop("invalid output times format"))
		class(x) <- NULL
	}
	if(!is.character(format.)) {
# format may be a function or name
		FUN <- switch(mode(format.),
			"function" = format.,
			name = eval(format.),
			stop(paste("unrecognized time format", deparse(
				substitute(format.)))))
		return(FUN(unclass(x), ...))
	}
	else format. <- rev(format.)[1]	
	# times greater than 1 day  should format like numerics
	nas <- is.na(x)
	days <- abs(trunc(x))
	att$class <- att$format <- att$origin <- NULL
	if(any(days[!nas] > 0)) {
		attributes(x) <- att
		return(format(x))
	}
	sec <- 24 * 3600 * abs(x)
	hh <- sec %/% 3600
	mm <- (sec - hh * 3600) %/% 60
	ss <- trunc(sec - hh * 3600 - 60 * mm)
	out <- list(h = substring(paste("0", hh, sep = ""), nchar(paste(hh))), 
		m = substring(paste("0", mm, sep = ""), nchar(paste(mm))), s = 
		substring(paste("0", ss, sep = ""), nchar(paste(ss))))
	style <- parse.format(format.)
	o <- style$periods
	if(!simplify)
		out <- paste(out[[o[1]]], out[[o[2]]], out[[o[3]]], sep = style$
			sep)
	else {
		if(simplify == 1) {
# no secs
			o <- o[o != "s"]
			out <- paste(out[[o[1]]], out[[o[2]]], sep = style$sep)
		}
		else out <- out$h
	}
	if(any(x[!nas] < 0))
		out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
	out[nas] <- NA
	out[x == Inf] <- "Inf"
	out[x ==  - Inf] <- "-Inf"
	attributes(out) <- att
	out
}
"format<-"<-
function(x, ..., value)
UseMethod("format<-")
"format<-.times"<-
function(x, ..., value)
{
    ok <- switch(mode(value),
                 character = ,
                 name = ,
                 "function" = ,
                 list = TRUE,
                 FALSE)
    if(!ok)
        stop(paste("invalid format \"", as.character(value), 
                   "\" in format replacement", sep = ""))
    attr(x, "format") <- value
    x
}
"frac2clock"<-
function(f)
{
	sec.per.day <- 24 * 3600
	secs <- f * sec.per.day
	h <- secs %/% 3600
	m <- round((secs - h * 3600)/60, 0)
	i <- h >= 13
	h[i] <- h[i] - 12
	pm <- rep("am", length(f))
	i <- f > 0.5
	pm[i] <- "pm"
	m <- paste(m)
	i <- nchar(m) == 1
	m[i] <- paste("0", m[i], sep = "")
	h <- paste(h)
	i <- nchar(h) == 1
	h[i] <- paste("0", h[i], sep = "")
	paste(h, ":", m, pm, sep = "")
}
"fraction"<-
function(x)
x - trunc(x)
####"hist"<-
####function(x, ...)
####  ##KH UseMethod("hist", x, ...)
####  UseMethod("hist")
####"hist.default"<-
####function(x, nclass, breaks, plot = TRUE, probability = FALSE, ..., xlab = 
####	deparse(substitute(x)))
####{
####	x <- x[!is.na(x)]
####	if(missing(breaks)) {
####		if(missing(nclass))
####			nclass <- log(length(x), base = 2) + 1
####		if(length(x) == 1)
####			breaks <- x + c(-1, 1)
####		else breaks <- pretty(x, nclass)
####		if(any(x <= breaks[1]))
####			breaks <- c(breaks[1] - diff(breaks)[1], breaks)
####		x[x > max(breaks)] <- max(breaks)	#roundoff
####	}
####	bin <- cut(x, breaks)
####	if(any(is.na(bin)))
####		stop("breaks do not span the range of x")
####	counts <- tabulate(bin, length(levels(bin)))
####	if(probability) {
####		binw <- diff(breaks)
####		if(min(binw) <= 0)
####			stop("zero width or inverted breaks")
####		counts <- counts/sum(counts)/binw
####	}
####	if(plot)
####		invisible(barplot(counts, width = breaks, histo = T, ..., xlab
####			 = xlab))
####	else list(breaks = breaks, counts = counts)
####}
"hist.times"<-
function(x, nclass, breaks, plot = TRUE, probability = FALSE, ..., xlab = 
	deparse(substitute(x)), simplify = T)
{
	if(!inherits(x, "times"))
		stop(paste(deparse(substitute(x)), "must be of class chron"))
	cl <- class(x)
	x <- as.numeric(x)
	tt <- NextMethod("hist", plot = FALSE)
	dots <- list(...)
	if(plot) {
		old <- par("xaxt", "yaxt")
		on.exit(old)
		out <- barplot(tt$counts, width = tt$breaks, histo = T, ..., 
			xlab = xlab, axes = F)
		if(any(cl == "dates"))
			lbl <- format(chron(dates = tt$breaks), simplify = 
				simplify)
		else lbl <- format(chron(times = tt$breaks), simplify = 
				simplify)
		if(is.null(adj <- dots$adj))
			adj <- par("adj")
		if(is.null(cex <- dots$cex))
			cex <- par("cex")
		if(is.null(font <- dots$font))
			font <- par("font")
		if(is.null(las <- dots$las))
			las <- par("las")
		if(is.null(lab <- dots$lab))
			lab <- par("lab")
		if(is.null(mgp <- dots$mgp))
			mgp <- par("mgp")
		if(is.null(tck <- dots$tck)) tck <- par("tck")	
	# do we plot x axis
		if(is.null(axes <- dots$axes))
			axes <- TRUE
		if(is.null(xaxt <- dots$xaxt))
			xaxt <- par("xaxt")
		if(is.null(yaxt <- dots$yaxt))
			yaxt <- par("yaxt")
		if(is.null(horiz <- dots$horiz))
			horiz <- FALSE
		if(axes) {
			if(horiz) {
				if(xaxt != "n")
				  axis(1, adj = adj, cex = cex, font = font, 
				    las = las, lab = lab, mgp = mgp, tck = tck)
			}
			else if(yaxt != "n")
				axis(2, adj = adj, cex = cex, font = font, las
				   = las, lab = lab, mgp = mgp, tck = tck)
			axis(horiz + 1, at = tt$breaks, labels = lbl, adj = adj,
				cex = cex, font = font, las = las, lab = lab, 
				mgp = mgp, tck = tck)
		}
	}
	invisible(tt)
}
"hours"<-
function(x)
{
	if(!inherits(x, "times"))
		return(NULL)
	x <- as.numeric(x)
	h <- trunc(24 * (x - trunc(x)))
	h
}
"identify.times"<-
function(x, y, ...)
{
	if(inherits(x, "times"))
		x <- as.numeric(x)
	if(!missing(y) && inherits(y, "times"))
		y <- as.numeric(y)
	NextMethod("identify", ...)
}
"is.chron"<-
function(x)
inherits(x, "chron")
"is.holiday"<-
function(x, holidays)
{
	if(!inherits(x, "dates"))
		if(is.character(x) || is.numeric(x))
			x <- dates(x)
		else stop("x must inherit from dates")
	if(missing(holidays))
		if(exists(".Holidays"))
			holidays <- .Holidays
		else holidays <- NULL
	orig.x <- origin(x)
	if(!is.null(orig.h <- origin(holidays)) && any(orig.x != orig.h))
		origin(holidays) <- orig.x
	out <- match(trunc(x), trunc(holidays), 0)
	as.logical(out)
}
"is.na.times"<-
function(x, ...)
{
	x <- as.numeric(x)
	NextMethod("is.na")
}
"is.weekend"<-
function(x)
{
	if(!inherits(x, "dates"))
		if(is.character(x) || is.numeric(x))
			x <- chron(x)
		else stop("x must inherit from dates")
	v <- month.day.year(as.numeric(x), origin = origin(x))
	out <- day.of.week(v$month, v$day, v$year) + 1	
	# recall out is between 1 (Sunday) and 7 (Saturday)
	out == 1 | out == 7
}
"julian"<-
function(m, d, y, origin.)
{
	only.origin <- all(missing(m), missing(d), missing(y))
	if(only.origin) m <- d <- y <- NULL	# return days since origin
	if(missing(origin.) || is.null(origin.))
		if(is.null(origin. <- options()$chron.origin))
			origin. <- c(month = 1, day = 1, year = 1970)
	nms <- names(d)
	m <- c(origin.[1], m)	# prepend month of new origin
	d <- c(origin.[2], d)	# prepend day of new origin
	y <- c(origin.[3], y)	# prepend year of new origin
#
# code from julian date in the S book (p.269)
#
	y <- y + ifelse(m > 2, 0, -1)
	m <- m + ifelse(m > 2, -3, 9)
	c <- y %/% 100
	ya <- y - 100 * c
	out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/% 5 + d +
		1721119	#
# now subtract the new origin from all dates
#
	if(!only.origin) {
		if(all(origin. == 0)) out <- out[-1] else out <- out[-1] - out[
				1]	# orig according to S algorithm
	}
	names(out) <- nms
	out
}
"julian2mine"<-
function(x)
{
	v <- month.day.year(x)
	d <- as.character(v$day)
	i <- nchar(d) == 1
	d[i] <- paste("0", d[i], sep = "")
	m <- substring(month.name[v$month], 1, 3)
	y <- v$year - 1900
	out <- paste(d, m, y, sep = "")
	out
}
"leap.year"<-
function(y)
{
	if(inherits(y, "dates"))
		y <- month.day.year(as.numeric(y), origin = origin(y))$year
        ##KH y %% 4 == 0 & (y %% 100 != 0 | y %% 4 == 0)
	y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
}
"lines.times"<-
function(x, y, ...)
{
	nas <- is.na(x)
	xtmp <- x <- x[!nas]
	ytmp <- y <- y[!nas]
	o <- order(x)
	x <- as.numeric(x[o])	# as.numeric ensures times are computed
	y <- as.numeric(y[o])
	NextMethod("lines", ...)
	invisible(list(x = xtmp, y = ytmp))
}
####"load.if.needed"<-
####function(file, fun.name, FUN = dyn.load, ...)
####{
####	if(exists(file, where = 0)) return(invisible(get(file, where = 0)))
####	# get name (if any) of calling function
####	if(missing(fun.name))
####		fun.name <- as.character(sys.call(sys.parent())[[1]])
####	if(!as.logical(length(fun.name)))
####		path <- paste(".", 	# try current dir
####		file, sep = "/")
####	else {
####		dir <- unix(paste("dirname", find(fun.name[1])[1]))
####		path <- paste(dir, file, sep = "/")
####	}
####	symbols <- FUN(path, ...)
####	assign(file, symbols, where = 0)
####	invisible(symbols)
####}
"lower.case"<-
function(str)
{
	n <- length(str)
	## load.if.needed("chron_strs.o")
	str[] <- .C(NAME = "to_lower",
		strings = as.character(str),
		nstrings = as.integer(n))$strings
	str
}
####"mean"<-
####function(x, ...)
####UseMethod("mean")
####"mean.default"<-
####function(x, trim = 0)
####{
####	if(any(is.na(x)))
####		return(NA)
####	if(trim > 0) {
####		if(trim >= 0.5)
####			return(median(x))
####		n <- length(x)
####		i1 <- floor(trim * n) + 1
####		i2 <- n - i1 + 1
####		x <- sort(x, unique(c(i1, i2)))[i1:i2]
####	}
####	sum(x + 0)/length(x)
####}
"mean.times"<-
function(x, trim = 0, weight = rep(1, length(x)), na.ok = T)
{
	if(!missing(weight) && length(weight) != length(x))
		stop(paste("weights must have same length as", deparse(
			substitute(x))))
	att <- attributes(x)[c("format", "origin", "class")]
	nas <- is.na(x)
	if(!na.ok && any(nas, is.na(weight)))
		return(structure(NA, format = att$format, origin = att$origin, 
			class = att$class))
	if(na.ok) {
		x <- x[!nas]
		if(!missing(weight))
			weight <- weight[!nas]
	}
	if(trim > 0) {
		if(trim >= 0.5)
			return(median(x))
		n <- length(x)
		i1 <- floor(trim * n) + 1
		i2 <- n - i1 + 1
		i <- sort.list(x, unique(c(i1, i2)))[i1:i2]
		weight <- weight[i]	# lazy eval makes order of assignment
		x <- x[i]	# important!
	}
	if(any(weight < 0))
		stop("weights must be non-negative")
	if(sm <- sum(weight))
		out <- sum(unclass(x) * (weight/sm))
	else out <- rep(0, length(x))
	structure(out, format = att$format, origin = att$origin, class = att$
		class)
}
"mine2julian"<-
function(str)
{
	d <- substring(str, 1, 2)
	m <- substring(str, 3, 5)
	y <- substring(str, 6, 7)
	m <- match(m, substring(month.name, 1, 3), nomatch = NA)
	julian(m, as.numeric(d), as.numeric(y) + 1900)
}
"minutes"<-
function(x)
{
	if(!inherits(x, "times"))
		return(NULL)
	x <- as.numeric(x)
	secs <- 24 * 60 * (x - trunc(x))
	m <- trunc(secs) %% 60
	m
}
"month.day.year"<-
function(jul, origin.)
{
	if(missing(origin.) || is.null(origin.))
		if(is.null(origin. <- options()$chron.origin))
			origin. <- c(month = 1, day = 1, year = 1970)
	if(all(origin. == 0)) shift <- 0 else shift <- julian(origin = origin.)
		# relative origin
# "absolute" origin
	j <- jul + shift
	j <- j - 1721119
	y <- (4 * j - 1) %/% 146097
	j <- 4 * j - 1 - 146097 * y
	d <- j %/% 4
	j <- (4 * d + 3) %/% 1461
	d <- 4 * d + 3 - 1461 * j
	d <- (d + 4) %/% 4
	m <- (5 * d - 3) %/% 153
	d <- 5 * d - 3 - 153 * m
	d <- (d + 5) %/% 5
	y <- 100 * y + j
	y <- y + ifelse(m < 10, 0, 1)
	m <- m + ifelse(m < 10, 3, -9)
	list(month = m, day = d, year = y)
}
"month.length"<-
c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
"months"<-
function(x, abb = T)
{
	if(!inherits(x, "dates"))
		if((is.character(x) || is.numeric(x)))
			x <- chron(x)
		else return(NULL)
	out <- month.day.year(as.numeric(x), origin = origin(x))$month
	lbl <- if(abb) month.abb else month.name
	out <- lbl[out]
	ordered(out, levels = lbl, labels = lbl)
}
"my.axis"<-
function(x, simplify = T, ...)
{
#put date labels in one line plus time lables on second line
	px <- pretty(x)
	xx <- chron(px, format = attr(x, "format"), origin = origin(x))
	lbls <- format(xx, enclose = c("", ""), sep = "\n", simplify = simplify
		)
	axis(1, at = px, labels = lbls, ...)
	invisible(list(at = px, labels = lbls))
}
"new.chron"<-
function(x, new.origin = c(1, 1, 1970), shift = julian(new.origin[1], 
	new.origin[2], new.origin[3], c(0, 0, 0)))
{
	cl <- class(x)
	class(x) <- NULL	# get rid of "delim" attribute
	del <- attr(x, "delim")
	attr(x, "delim") <- NULL	# map formats
	format <- attr(x, "format")
	format[1] <- switch(format[1],
		abb.usa = paste("m", "d", "y", sep = del[1]),
		abb.world = paste("d", "m", "y", sep = del[1]),
		abb.ansi = "ymd",
		full.usa = "month day year",
		full.world = "day month year",
		full.ansi = "year month year",
		format[1])
	if(length(format) == 2)
		format[2] <- switch(format[2],
			military = "h:m:s",
			format[2])
	attr(x, "format") <- format
	orig <- attr(x, "origin")
	if(is.null(orig)) {
		x <- x - shift
		attr(x, "origin") <- new.origin
	}
# (update origin after we assign the proper class!)
# deal with times as attributes 
	tms <- attr(x, "times")
	if(!is.null(tms)) {
		if(all(tms[!is.na(tms)] >= 1))
			tms <- tms/(24 * 3600)
		x <- x + tms
		class(x) <- c("chron", "dates", "times")
	}
	else class(x) <- c("dates", "times")
	x
}
"origin"<-
function(x)
attr(x, "origin")
"origin<-"<-
function(x, value)
{
    if (length(value) != 3 || any(is.na(value)))
        stop("origin must be a month, day, year vector")
    if (value[1] < 1 || value[1] > 12)
        stop("month out of range in origin")
    n <- month.length[value[1]] +
        as.numeric(value[1] == 2 && leap.year(value[3]))
    if (value[2] < 1 || value[2] > n)
        stop("day out of range in origin")
    cl <- class(x)
    class(x) <- NULL
    jval <- julian(value[1], value[2], value[3], origin = c(0, 0, 0))	
    ## adjust days for new origin (new.x + new.o == old.x + old.o)
    if (!is.null(ox <- attr(x, "origin")))
        x <- x - jval + julian(ox[1], ox[2], ox[3], origin = c(0, 0, 0))
    new.origin <- unlist(month.day.year(jval, origin = c(0, 0, 0)))
    attr(x, "origin") <-
        structure(new.origin, names = c("month", "day", "year"))
    class(x) <- cl
    x
}
"parse.format"<-
function(format)
{
# determine order of month, day, year or hour, min, secs
	abb <- TRUE	# short notation?
	mon.abb <- FALSE	# should month names be abbreviated?
	if((nf <- nchar(format)) == 5) {
# abbreviated dates/times
		sep <- substring(format, 2, 2)
		fmt <- substring(format, first = c(1, 3, 5), last = c(1, 3, 5))
	}
	else if(nf == 3) {
		sep <- ""	# no sep
		fmt <- substring(format, first = 1:3, last = 1:3)
	}
	else {
# full format (month names)
		abb <- FALSE
		sep <- " "
		fmt <- unlist(unpaste(format, sep = sep))
		mon.abb <- if(any(fmt == "month")) FALSE else TRUE
	}
	periods <- substring(lower.case(fmt), 1, 1)	# m, d, & y in right order
	return(abb, sep, periods, mon.abb)
}
"plot.times"<-
function(x, y, ..., xlab = deparse(substitute(x)), ylab = deparse(substitute(y)
	), simplify)
{
	if(missing(simplify))
		if(is.null(simplify <- options("chron.simplify")[[1]]))
			simplify <- TRUE
	x.times <- inherits(x, "times")	# is x a times?
	if(missing(y)) {
		x <- sort(x)	# NA's will be ignored
		y <- seq(along = as.vector(x))
		if(missing(ylab))
			ylab <- "Counts"
	}
	y.times <- inherits(y, "times")	# is y a times?
	dots <- list(...)
	if(is.null(axes <- dots$axes)) axes <- T	# do we draw axes? 
# only xaxt="n" or yaxt="n" requests in ... are honored!
	if(is.null(req.xaxt <- dots$xaxt) || req.xaxt != "n")
		req.xaxt <- "s"
	if(is.null(req.yaxt <- dots$yaxt) || req.yaxt != "n")
		req.yaxt <- "s"
	old <- par("xaxt", "yaxt")
	on.exit(par(old))	#
# trap graphical pars in ... that affect axis() in addition to plot()
#
	if(is.null(adj <- dots$adj))
		adj <- par("adj")
	if(is.null(cex <- dots$cex))
		cex <- par("cex")
	if(is.null(col <- dots$col))
		col <- par("col")
	if(is.null(font <- dots$font))
		font <- par("font")
	if(is.null(las <- dots$las))
		las <- par("las")
	if(is.null(lab <- dots$lab))
		lab <- par("lab")
	if(is.null(mgp <- dots$mgp))
		mgp <- par("mgp")
	if(is.null(tck <- dots$tck)) tck <- par("tck")	
	# for some plot types we need to sort according to x
	if(!is.null(type <- dots$type))
		if(any(type == c("l", "b", "o"))) {
			nas <- is.na(x)
			o <- order(x[!nas])
			x <- x[!nas][o]
			y <- y[!nas][o]
		}
	xx <- unclass(x)
	yy <- unclass(y)
	if(x.times)
		xaxt <- "n"
	else xaxt <- req.xaxt
	if(y.times)
		yaxt <- "n"
	else yaxt <- req.yaxt
	if(!is.null(l <- dots$log)) {
		if(inherits(x, "dates") && any(l == c("x", "xy", "yx")))
			stop("cannot do logarithmic plot of a dates object")
		if(inherits(y, "dates") && any(l == c("y", "xy", "yx")))
			stop("cannot do logarithmic plot of a chron object")
	}
# unfortunately we can't use (easily) NextMethod when y is missing!
	plot.default(xx, yy, xlab = xlab, ylab = ylab, ..., xaxt = xaxt, yaxt
		 = yaxt)
	if(axes) {
		if(req.xaxt == "n")
			par(xaxt = "n")
		else if(x.times)
			axis.times(1, x, simplify = simplify, labels = T, adj
				 = adj, col = col, cex = cex, font = font, las
				 = las, lab = lab, mgp = mgp, tck = tck)
		if(req.yaxt == "n")
			par(yaxt = "n")
		else if(y.times)
			axis.times(2, y, simplify = simplify, srt = 90, labels
				 = T, adj = adj, col = col, cex = cex, font = 
				font, las = las, lab = lab, mgp = mgp, tck = 
				tck)
	}
	invisible(list(x = x, y = y))
}
"points.times"<-
function(x, y, ...)
{
	xtmp <- x
	ytmp <- y
	x <- as.numeric(x)
	y <- as.numeric(y)
	NextMethod("points", ...)
	invisible(list(x = xtmp, y = ytmp))
}
"print.chron"<-
function(x, digits = NULL, quote = F, prefix = "", sep = " ", enclosed = c("(", 
	")"), simplify)
{
	if(!as.logical(length(x))) {
		cat("chron(0)\n")
		return(invisible(x))
	}
	if(missing(simplify))
		if(is.null(simplify <- options("chron.simplify")[[1]]))
			simplify <- F
	x <- format.chron(x, sep = sep, enclosed = enclosed, simplify = 
		simplify)
	out <- print.default(x, quote = quote)
	invisible(out)
}
"print.dates"<-
function(x, digits = NULL, quote = F, prefix = "", simplify)
{
	if(!as.logical(length(x))) {
		cat("dates(0)\n")
		return(invisible(x))
	}
	if(missing(simplify))
		if(is.null(simplify <- options("chron.simplify")[[1]]))
			simplify <- F
	out <- format.dates(x, simplify = simplify)
	out <- print.default(out, quote = quote)
	invisible(out)
}
"print.times"<-
function(x, digits, quote = F, prefix = "", simplify)
{
	if(!as.logical(length(x))) {
		cat("times(0)\n")
		return(invisible(x))
	}
	if(missing(simplify) && is.null(simplify <- options()$chron.simplify)) 
			simplify <- F	
	# print whole days (no fraction) as regular integers
	if(all(is.na(x)) || any(x[!is.na(x)] > 1))
		cat("Time in days:\n")
	x <- format.times(x, simplify = simplify)
	out <- NextMethod("print", quote = quote)
	invisible(out)
}
####"quantile"<-
####function(x, ...)
####UseMethod("quantile")
####"quantile.default"<-
####function(x, probs = seq(0, 1, 0.25))
####{
####	if(any(probs < 0 | probs > 1))
####		stop("Probabilities must be between 0 and 1 inclusive")
####	if(any(is.na(x)))
####		return(NA)
####	names(x) <- NULL
####	n <- length(x)
####	order <- 1 + (n - 1) * probs
####	low <- pmax(floor(order), 1)
####	high <- pmin(low + 1, n)
####	x <- sort(x, unique(sort(c(low, high))))
####	order <- order %% 1
####	(1 - order) * x[low] + order * x[high]
####}
"quantile.times"<-
function(x, ...)
{
	fmt <- attr(x, "format")
	orig <- attr(x, "origin")
	cl <- class(x)
	x <- unclass(x)
	out <- structure(NextMethod("quantile"), format = fmt, origin = orig, 
		class = cl)
	out
}
"quarters"<-
function(x, abb = T)
{
	if(!inherits(x, "dates"))
		if((is.character(x) || is.numeric(x)))
			x <- chron(x)
		else return(NULL)
	v <- month.day.year(trunc(as.numeric(x)))$month
	out <- (v - 1) %/% 3 + 1
	lbl <- if(abb) c("1Q", "2Q", "3Q", "4Q") else c("I", "II", "III", "IV")
	out <- lbl[out]
	ordered(out, levels = lbl, labels = lbl)
}
"seconds"<-
function(x)
{
	if(!inherits(x, "times"))
		return(NULL)
	x <- as.numeric(x)
	secs <- 24 * 3600 * (x - trunc(x))
	trunc(secs) %% 60
}
####"seq"<-
####function(...)
####  ##KH UseMethod("seq", ...)
####  UseMethod("seq")
"seq.dates"<-
function(from, to, by = "days", length.)
{
	if(missing(from))
		stop("argument \"from\" must be specified")
	if(!inherits(from, "dates")) from <- chron(from[1])	
	# the output will have same format and origin as "from"
	fmt <- attr(from, "format")	# dates format 
	org <- origin(from)	# dates origin
	if(is.numeric(by)) {
		cl <- class(from)
		from <- as.numeric(from)
		if(!missing(to)) {
			if(!is.null(to.org <- origin(to)) && any(to.org != org)
				)
				origin(to) <- org
			to <- as.numeric(to)
		}
		x <- NextMethod("seq")	
	# preserve full chrons (i.e., don't round x)
		if(all(cl != "chron"))
			x <- round(x, 0)
		return(chron(x, format = fmt, origin = org))
	}
	if(!is.character(by) || length(by) != 1)
		stop("\"by\" must be a number or string (days, weeks, months, or years)"
			)
	valid <- c("days", "weeks", "months", "years")
	if(!as.logical(i <- pmatch(by, valid, 0)))
		stop("\"by\" must be one of days, weeks, months, or years")
	by <- valid[i]	# coerced "to" to a dates object
	if(missing(to)) {
		if(missing(length.))
			stop("must specify \"length\" when \"to\" is missing")
		to <- from + (length. - 1) * c(1, 7, 31, 366)[i]	
	# possibly BUGGY!!!
	}
	else {
		if(!missing(by) && !missing(length.))
			stop("Too many arguments")
		if(!inherits(to, "dates"))
			to <- chron(to)
		if(!missing(length.))
			by <- if(from < to) as.numeric(to - from)/(length. - 1)
				 else 0
	}
# make sure "from" and "to" have the same origin
	if(!is.null(to.org <- origin(to)) && any(to.org != org))
		origin(to) <- org
	if(from > to)
		stop("\"from\" must be a date before \"to\"")
	frm <- as.numeric(from)
	t0 <- as.numeric(to)
	frm.mdy <- month.day.year(frm, origin = org)	
	# the idea is to generate all days between "form" and "to", 
# subset out the dates we need, and finally chron them.
	x <- seq.default(from = frm, to = t0)
	if(by == "days")
		return(chron(x, format = fmt, origin = org))
	if(by == "weeks") {
		mdy <- month.day.year(x, origin = org)
		mdy.dow <- day.of.week(mdy$month, mdy$day, mdy$year)
		frm.dow <- day.of.week(frm.mdy$month, frm.mdy$day, frm.mdy$year
			)
		x <- x[mdy.dow == frm.dow]
		return(chron(x, format = fmt, origin = org))
	}
	if(by == "months") {
# be careful when "from" is in the tail of the month!
		nxt.day <- month.day.year(as.numeric(from + 1))$month
		end.of.the.month <- frm.mdy$month != nxt.day
		mdy <- month.day.year(x, origin = org)
		dys <- mdy$day
		if(frm.mdy$day <= 28) x <- x[dys == frm.mdy$day] else if(
			end.of.the.month)
			x <- x[dys == 1] - 1
		else {
# 29th or 30th of one of the 31-day months
			x1 <- x[dys == frm.mdy$day]	# all but Feb!
			x2 <- x[mdy$month == 3 & dys == 1] - 1	# Feb
			x <- sort(c(x1, x2))
		}
# simple case
		return(chron(x, format = fmt, origin = org))
	}
	if(by == "years") {
# be careful when "from" is Feb 29 of a leap year
		mdy <- month.day.year(x, org)
		if(leap.year(frm.mdy$year) && frm.mdy$day == 29)
			x <- x[mdy$day == 1 & mdy$month == 3] - 1
		else x <- x[mdy$day == frm.mdy$day & mdy$month == frm.mdy$month
				]
		return(chron(x, format = fmt, origin = org))
	}
}
####"seq.default"<-
####function(from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = 
####	NULL, along.with = NULL)
####{
####	if(!missing(along.with))
####		length.out <- length(along.with)
####	if(nargs() == 1 && !missing(from)) {
####		if(mode(from) == "numeric" && length(from) == 1)
####			1:from
####		else seq(along.with = from)
####	}
####	else if(is.null(length.out))
####		if(missing(by))
####			from:to
####		else {
####			n <- (to - from)/by
####			if(n < 0)
####				stop("Wrong sign in by= argument")
####			from + (0:n) * by
####		}
####	else if(length.out < 0)
####		stop("Length cannot be negative")
####	else if(length.out == 0)
####		numeric(0)
####	else if(missing(by)) {
####		if(from == to || length.out < 2)
####			by <- 1
####		if(missing(to))
####			to <- from + length.out - 1
####		if(missing(from))
####			from <- to - length.out + 1
####		if(length.out > 2)
####			if(from == to)
####				rep(from, length.out)
####			else as.vector(c(from, from + (1:(length.out - 2)) * by,
####				  to))
####		else as.vector(c(from, to))[1:length.out]
####	}
####	else if(missing(to))
####		from + (0:(length.out - 1)) * by
####	else if(missing(from))
####		to - ((length.out - 1):0) * by
####	else stop("Too many arguments")
####}
"summary.times"<-
function(x, digits = 12, ...)
{
	if(!as.logical(length(x)))
		return(x)
	att <- attributes(x)
	class(x) <- NULL
	y <- as.numeric(x)
	z <- unclass(summary.default(y, digits = digits, ...))
	tmp <- structure(z[1:6], format = att$format, origin = att$origin, 
		class = att$class)
	z[1:6] <- format(tmp)
	class(z) <- "table"
	z
}
"times"<-
function(x, ...)
chron(times. = x, ...)
"trunc.dates"<-
function(x)
{
	cl <- class(x)
	class(x) <- NULL
	out <- NextMethod("trunc")
	class(out) <- cl[!as.logical(match(cl, "chron", 0))]
	out
}
"unpaste"<-
function(str, sep = "/", fnames = NULL, nfields = NULL, first = c(1, 3, 5), 
	width = 2)
{
# split str into fields separated by sep or by fiels specified
# by start positions and field widths; output a list
	str <- as.character(str)
	nas <- str == "NA" | str == ""
	if(sep != "") {
		if(is.null(nfields)) {
# use a simple heuristic
			nf <- count.fields.str(str[!nas], sep = sep)
			cnt <- table(nf)
			nfields <- sort(unique(nf))[cnt == max(cnt)]
		}
		str[nas] <- paste(rep(NA, nfields), collapse = sep)
		nf <- count.fields.str(str, sep = sep)
		bad <- seq(along = str)[nf != nfields]
		if(n.bad <- length(bad)) {
			if(n.bad > 10)
				msg <- paste(n.bad, 
				  "entries set to NA due to wrong number of fields"
				  )
			else msg <- paste(
				  "wrong number of fields in entry(ies)", paste(
				  bad, collapse = ", "))
			warning(msg)
			nas[bad] <- T
			str[nas] <- paste(rep(NA, nfields), collapse = sep)
		}
		n <- length(str)
		white.space <- F
		## load.if.needed("chron_strs.o")
		out <- .C(NAME = "unpaste",
			strings = as.character(str),
			nstrings = as.integer(n),
			sep = as.character(sep),
			white.space = as.integer(white.space),
			nfields = as.integer(nfields),
			output = vector("list", length = nfields))$output
	}
	else {
		last <- first + width - 1
		out <- vector("list", length = length(first))
		for(i in seq(along = first)) {
			out[[i]] <- substring(str, first[i], last[i])
			out[[i]][nas] <- "NA"
		}
	}
	names(out) <- fnames
	return(out)
}
"weekdays"<-
function(x, abb = T)
{
	if(!inherits(x, "dates"))
		if((is.character(x) || is.numeric(x)))
			x <- chron(x)
		else stop("x must inherit from dates")
	v <- month.day.year(as.numeric(x), origin = origin(x))
	out <- day.of.week(v$month, v$day, v$year) + 1
	lbl <- if(abb) day.abb else day.name
	out <- lbl[out]
	ordered(out, levels = lbl, labels = lbl)
}
"years"<-
function(x)
{
	if(!inherits(x, "dates"))
		if((is.character(x) || is.numeric(x)))
			x <- chron(x)
		else return(NULL)
	y <- month.day.year(as.numeric(x), origin = origin(x))$year
	y <- ordered(y)
	y
}
"Summary.dates"<-
function(x, ...)
{
	ok <- switch(.Generic,
		max = ,
		min = ,
		range = T,
		FALSE)
	if(!ok)
		stop(paste(.Generic, 
			"not defined for objects that inherit from dates"))
	val <- NextMethod(.Generic)
	attr(val, "origin") <- origin(x)
	class(val) <- class(x)
	val
}
