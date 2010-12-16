# assign verbose = TRUE/FALSE outside the time_f() function
time_f <- function(f, name = deparse(substitute(f))) {
    force(f)
    function(...) {
        if (!verbose)
            return(f(...))

        message("Timing ", name)
        timing <- system.time(f(...))
        message("Took: ", formatC(timing[3]), "s")
    }
}
