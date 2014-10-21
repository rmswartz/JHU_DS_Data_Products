library(shiny)
library(ggplot2)
library(scales)
shinyServer(
function(input, output) {
## determine baseline payment and interest total
price <- reactive({as.numeric(input$price)})
per.down <- reactive({as.numeric(input$per.down / 100)})
rate <- reactive({as.numeric(input$apr / 1200)})
n <- reactive({as.numeric(input$term * 12)})
base.monthly.payment <- reactive({
      (rate() * price() * (1 - per.down()) * ((1 + rate())^n())) / (((1 + rate())^n()) - 1)
})
output$base.monthly.payment <- renderPrint({round(base.monthly.payment(), digits = 2)})
base.total.interest <- reactive({
      (base.monthly.payment() * n()) - (price() * (1 - per.down()))
})
output$base.total.interest <- renderPrint({round(base.total.interest(), digits = 2)})
## create dataframe to populate with increments of additional payment
schedule <- data.frame(matrix(data = NA, nrow = 41, ncol = 6, dimnames = list(1:41, c("add.amt", "add.n", "prin", "add.total.interest", "savings", "early"))))
## initialize 'for' loop to populate possible amoritization totals
c <- 1
reactive({
for (i in seq(0, 1000, 25)) {
      schedule$add.amt[c] <- i
      schedule$add.n[c] <- reactive({
            log(((base.monthly.payment() + i) / rate()) / (((base.monthly.payment() + i) / rate()) - (price() * (1 - per.down())))) / log(1 + rate())
      })
      schedule$prin[c] <- reactive({
            round(price() * (1 - per.down()), digits = 2)
      })
      schedule$add.total.interest[c] <- reactive({
            round(((base.monthly.payment() + i) * schedule()$add.n[c]) - schedule()$prin[c], digits = 2)
      })
      schedule$savings[c] <- reactive({
            round(base.total.interest() - schedule()$add.total.interest[c], digits = 2)
      })
      schedule$early[c] <- reactive({
            round(n() - schedule()$add.n[c], digits = 0)
      })
      c <- c + 1
}
})
add <- reactive({as.numeric(input$add)})
new.n <- reactive({
      log(((base.monthly.payment() + add()) / rate()) / (((base.monthly.payment() + add()) / rate()) - (price() * (1 - per.down())))) / log(1 + rate())
})
prin <- reactive({
      round(price() * (1 - per.down()), digits = 2)
})
new.total.int <- reactive({
      round(((base.monthly.payment() + add()) * new.n()) - prin(), digits = 2)
})
savings <- reactive({
      round(base.total.interest() - new.total.int(), digits = 2)
})
early <- reactive({
      round(n() - new.n(), digits = 0)
})
output$savings <- renderPrint({savings()})
output$early <- renderPrint({early()})
# ## create data.frame suitable for plotting
# graph.data <- data.frame(matrix(data = NA, nrow = 82, ncol = 3, dimnames = list(1:82, c("extra.prin", "amount", "type"))))
# c <- 1
# reactive({
# for (i in seq(0, 1000, 25)) {
#       graph.data$extra.prin[c] <- i
#       graph.data$extra.prin[c + 1] <- i
#       graph.data$amount[c] <- schedule$prin[which(schedule$add.amt == i)]
#       graph.data$amount[c + 1] <- schedule$add.total.interest[which(schedule$add.amt == i)]
#       graph.data$type[c] <- "Principal" 
#       graph.data$type[c + 1] <- "Interest"
#       c <- c + 2
# }
# })
# ## create plot of amoritization with line for additional principal amount
# output$plot <- renderPlot({
# plot <- ggplot(graph.data(), aes(x = extra.prin, y = amount), color = type)
# + geom_area(aes(fill = type), position = 'stack', alpha = 0.75)
# + geom_vline(xintercept = add(), color="black", linetype = "longdash", size = 1)
# + labs(x = "Additional Principal/Month", y = "Total Cost")
# + scale_fill_manual(values=c("firebrick3", "dodgerblue3"), name = "Payment Component")
# + theme(axis.title.x = element_text(face = "bold", vjust = -0.7, size = 16), 
#         axis.title.y = element_text(face = "bold", vjust = 2, size = 16),
#         axis.text.x = element_text(size = 14), 
#         axis.text.y = element_text(size = 14), 
#         panel.margin = unit(c(5, 5, 5, 5), "mm"),
#         plot.margin = unit(c(5, 5, 5, 5), "mm"),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(colour = "gray"),
#         panel.grid.minor.y = element_line(colour = "gray86"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank())
# + scale_x_continuous(labels = dollar)
# + scale_y_continuous(labels = dollar)
# print(plot)
# })
})