n <- term * 12
int <- apr / 12
monthly.payment <- (int * price * (1 - per.down) * ((1 + int)^n)) / (((1 + int)^n) - 1)
base.schedule <- matrix(data = NA, nrow = n, ncol = 5, 
                        dimnames = list(1:n, c("payment.number", "remaining.prin",
                                               "int.comp", "prin.comp", "payment")))
base.schedule <- data.frame(base.schedule)
base.schedule$payment.number[1] <- 1
base.schedule$remaining.prin[1] <- price * (1 - per.down)
base.schedule$int.comp[1] <- base.schedule$remaining.prin[1] * int
base.schedule$prin.comp[1] <- monthly.payment - base.schedule$int.comp[1]
base.schedule$payment[1] <- base.schedule$prin.comp[1] + base.schedule$int.comp[1]
for (i in 2:n) {
      base.schedule$payment.number[i] <- i
      base.schedule$remaining.prin[i] <- base.schedule$remaining.prin[i - 1] - base.schedule$prin.comp[i - 1]
      base.schedule$int.comp[i] <- base.schedule$remaining.prin[i] * int
      base.schedule$prin.comp[i] <- monthly.payment - base.schedule$int.comp[i]
      base.schedule$payment[i] <- base.schedule$prin.comp[i] + base.schedule$int.comp[i]
}
base.int.total <- sum(base.schedule$int.comp)