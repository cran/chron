".First.lib" <-
function (lib, pkg) {
    library.dynam("chron", pkg, lib)

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

    options(chron.year.abb = TRUE,
            chron.year.expand = "year.expand")
}

## The following are generic in R 1.3.0.
if(R.version$major <= 1 && R.version$minor < 3) {
    cat("I am here\n")
    julian <- function(x, ...) UseMethod("julian")
    months <- function(x, abbreviate) UseMethod("months")
    quarters <- function(x, abbreviate) UseMethod("quarters")
    weekdays <- function(x, abbreviate) UseMethod("weekdays")
}
