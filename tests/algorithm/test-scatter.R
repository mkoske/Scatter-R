testCanRunTestsThatAreOk <- function() {
    checkEquals(1, sin(pi/2))
}

testTestsThatAreNotOkFails <- function() {
    checkEquals(T, F)
}

