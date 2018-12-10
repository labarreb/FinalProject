##' Function that rolls a 6-sided die (Description)
##'
##' By typing the function, without parameters, a randomly
##' simulated number from 1-6 is returned, simulating rolling
##' a 6-sided die.
##' @title Roll a Die
##' @return integer between 1-6
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' rdie()


rdie <- function() {
    return(sample.int(6, 1))
}
