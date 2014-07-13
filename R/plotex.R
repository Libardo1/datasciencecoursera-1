#x <- rnorm(100)
#y <- x + rnorm(100)
#z <- rpois(100, 2)
par(mar = c(4,4,2,2))
##par(mfrow = c(1, 1))
#plot(x, y, xlab = "weight", ylab = "height", pch = 20)
#title("Scatterplot")
#text(-2, -2, "Label")
#legend("topright", legend = "Data", pch = 20)
#fit <- lm(y ~ x)
#abline(fit, lwd = 3, col = "blue")
##plot(x, y, pch = 20)
##plot(x, z, pch = 19)
##par(mar = c(2,2,1,1))

## gl function: generates grouping varible, 2 levels, each 50 items
g = gl(2, 50, labels = c("Male", "Female"))
str(g)
#plot(x, y)

## type n hold the points
plot(x, y, type = "n")

points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue")
