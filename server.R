library(shiny)
library(ggplot2)
library(scales)
shinyServer(function(input, output) {
## determine baseline payment and interest total
price <- as.numeric(input$price)
per.down <- input$per.down / 100
int <- input$apr / 1200
n <- input$term * 12
base.monthly.payment <- (int * price * (1 - per.down) * ((1 + int)^n)) / (((1 + int)^n) - 1)
output$base.monthly.payment <- renderprint({base.monthly.payment})
base.total.interest <- (base.monthly.payment * n) - (price * (1 - per.down))
output$base.total.interest <- renderprint({base.total.interest})
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
add <- input$add
output$savings <- renderprint({schedule$savings[which(schedule$add = add)]})
output$early <- renderprint({schedule$early[which(schedule$add = add)]})
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
## create plot of amoritization with line for additional principal amount
output$plot <- rednerPlot({
add <- input$add
ggplot(graph.data, aes(x = add, y = amount), color = type)
+ geom_area(aes(fill = type), position = 'stack', alpha = 0.75)
+ geom_vline(xintercept = add, color="black", linetype = "longdash", size = 1)
+ labs(x = "Additional Principal/Month", y = "Total Cost")
+ scale_fill_manual(values=c("firebrick3", "dodgerblue3"), name = "Payment Component")
+ theme(axis.title.x = element_text(face = "bold", vjust = -0.7, size = 16), 
        axis.title.y = element_text(face = "bold", vjust = 2, size = 16),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        panel.margin = unit(c(5, 5, 5, 5), "mm"),
        plot.margin = unit(c(5, 5, 5, 5), "mm"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "gray"),
        panel.grid.minor.y = element_line(colour = "gray86"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
+ scale_x_continuous(labels = dollar)
+ scale_y_continuous(labels = dollar)
})