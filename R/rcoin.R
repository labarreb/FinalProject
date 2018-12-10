##' Function that flips a two-sided coin (Description)
##'
##' By typing the function, without parameters, a randomly
##' simulated choice between 'Heads' or 'Tails' is returned, 
##' simulating flipping a coin.
##' @title Flip a Coin
##' @return String: Either 'Heads' or 'Tails'
##' @author c(Brian LaBarre, Silvia Jakubski)
##' @export
##' @examples
##' rcoin()


rcoin <- function() {
    return(sample(c("Heads", "Tails"), 1))
}
