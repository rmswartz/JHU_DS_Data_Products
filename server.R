library(shiny)
shinyServer(function(input, output) {
## determine baseline payment and interest total
n <- term * 12
int <- apr / 12
base.monthly.payment <- (int * price * (1 - per.down) * ((1 + int)^n)) / (((1 + int)^n) - 1)
base.total.interest <- (base.monthly.payment * n) - (price * (1 - per.down))
## create dataframe to populate with increments of additional payment
schedule <- data.frame(matrix(data = NA, nrow = 41, ncol = 6, 
                         dimnames = list(1:41, c("add", "add.n",
                                                "prin", "add.total.interest", 
                                                "savings", "early"))))
## initialize 'for' loop to populate possible amoritization totals
c <- 1
for (i in seq(0, 1000, 25)) {
      schedule$add[c] <- i
      schedule$add.n[c] <- log(((base.monthly.payment + i) / int) / (((base.monthly.payment + i) / int) - (price * (1 - per.down)))) / log(1 + int)
      schedule$prin[c] <- round(price * (1 - per.down), digits = 2)
      schedule$add.total.interest[c] <- round(((base.monthly.payment + i) * schedule$add.n[c]) - schedule$prin[c], digits = 2)
      schedule$savings[c] <- round(base.total.interest - schedule$add.total.interest[c], digits = 2)
      schedule$early[c] <- round(n - schedule$add.n[c], digits = 0)
      c <- c + 1
}
## create data.frame suitable for plotting
graph.data <- data.frame(matrix(data = NA, nrow = 82, ncol = 3, 
                                dimnames = list(1:82, c("add", "amount", "type"))))
c <- 1
for (i in seq(0, 1000, 25)) {
      graph.data$add[c] <- i
      graph.data$add[c + 1] <- i
      graph.data$amount[c] <- schedule$prin[which(schedule$add == i)]
      graph.data$amount[c + 1] <- schedule$add.total.interest[which(schedule$add == i)]
      graph.data$type[c] <- "Principal" 
      graph.data$type[c + 1] <- "Interest"
      c <- c + 2
}
})
## create plot of amoritization
library(rCharts)
plot <- nPlot(amount ~ add, group = 'type', data = graph.data, type = 'line')