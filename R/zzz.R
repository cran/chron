.onLoad <-
function(libname, pkgname) {

    ## The following controls the behavior when faced w. 2-digit years.
    ##
    ## To have 2-digit years actually refer to the first century
    ##    options(chron.year.abb = FALSE)
    ##
    ## To flag all 2-digit years as error:
    ##    options(chron.year.abb = TRUE,
    ##            chron.year.expand = "year.strict")
    ##
    ## To allow 2-digit year abbreviations and guess(?) actual year:
    ##    options(chron.year.abb = TRUE,
    ##            chron.year.expand = "year.expand")

    if(is.null(getOption("chron.year.abb")))
        options(chron.year.abb = TRUE)
    if(is.null(getOption("chron.year.expand")))
        options(chron.year.expand = "year.expand")
}

.onAttach <-
function(libname, pkgname) {
    msg <- c("NOTE: The default cutoff when expanding a 2-digit year",
             "to a 4-digit year will change from 30 to 69 by Aug 2020",
             "(as for Date and POSIXct in base R.)")
    packageStartupMessage(paste(msg, collapse = "\n"))
}
        
