# NOTE: This file constains versions of 
#	unpaste()  and  lower.case() 
# that use the unix() interface instead of C functions (calling C
# instead of unix() typically results in faster routines). These
# functions are primarily for those that DO NOT want to bother 
# compiling C functions in order to install chron objects.

unpaste <- function(str, sep="/", fnames=NULL, nfields = NULL,
		first=c(1,3,5), width=2)
# split str into fields separated by sep or by fiels specified
# by start positions and field widths; output a list
{
	str <- as.character(str)
	nas <- substring(str,1,2) =="NA" | str==""	# for chron stuff's sake
	if(missing(first) && sep!=""){
		if( is.null(nfields) ){	# heuristic: nfields the largest nf
			nf <- count.fields.str(str[!nas], sep=sep)
			cnt <- table(nf)
			nfields <- sort(unique(nf))[ cnt == max(cnt) ]
		}
		str[nas] <- paste(rep(NA, nfields), collapse=sep)
		nf <- count.fields.str(str, sep = sep)
		bad <- seq(along=str)[nf!=nfields]
		if( n.bad <- length(bad) ){
			if(n.bad>10)
				msg <- paste(n.bad, 
				"entries set to NA due to wrong number of fields")
			else	msg <- paste("wrong number of fields in entry(ies)",
				paste(bad, collapse=", "))
			warning(msg)
			nas[bad] <- T
			str[nas] <- paste(rep(NA, nfields), collapse=sep)
		}
		tmp <- tempfile("unpaste")
		on.exit(unlink(tmp))
		write(str[!nas], tmp, ncol=1)
		out <- scan(tmp, what = rep(list(""),nfields), sep=sep) 
		nstr <- length(str)
		for( i in seq(along=out) ){
			oi <- out[[i]]
			out[i] <- list(rep("NA", nstr))
			out[[i]][!nas] <- oi
		}
	} else {
		last <- first+width-1
		out <- vector("list", length=length(first))
		for( i in seq(along=first)){
			out[[i]] <- substring(str, first[i], last[i])
			out[[i]][nas] <- "NA"
		}
	}
	names(out) <- fnames
	return(out)
}
