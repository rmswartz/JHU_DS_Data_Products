## determine baseline payment and interest total
n <- term * 12
int <- apr / 12
base.monthly.payment <- (int * price * (1 - per.down) * ((1 + int)^n)) / (((1 + int)^n) - 1)
base.total.interest <- (base.monthly.payment * n) - (price * (1 - per.down))
## determine number of periods for payback when paying additional principal
n.add <- log(((base.monthly.payment + add) / int) / (((base.monthly.payment + add) / int) - (price * (1 - per.down)))) / log(1 + int)
## determine total interest over the life of the loan with the new payment
add.total.interest <- ((base.monthly.payment + add) * n.add) - (price * (1 - per.down))
interest.savings <- base.total.interest - add.total.interest
early <- n - n.add
## create dataframe and populate with increments of additional payment
visual <- matrix(data = NA, nrow = 41, ncol = 4, 
                         dimnames = list(1:41, c("graph.add", "graph.n",
                                                "graph.prin", "graph.interest")))
visual <- data.frame(visual)
c <- 1
for (i in seq(0, 1000, 25)) {
      visual$graph.add[c] <- i
      visual$graph.n[c] <- log(((base.monthly.payment + i) / int) / (((base.monthly.payment + i) / int) - (price * (1 - per.down)))) / log(1 + int)
      visual$graph.prin[c] <- price * (1 - per.down)
      visual$graph.interest[c] <- ((base.monthly.payment + i) * visual$graph.n[c]) - visual$graph.prin[c]
      c <- c + 1
}