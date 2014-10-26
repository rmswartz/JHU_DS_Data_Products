library(shiny)
library(ggplot2)
library(scales)
library(grid)
shinyServer(
function(input, output, clientData, session) {
observe({
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
add.amt <- seq(0, 1000, 25)
add.n <- reactive({
      log(((base.monthly.payment() + add.amt) / rate()) / (((base.monthly.payment() + add.amt) / rate()) - (price() * (1 - per.down())))) / log(1 + rate())
})
prin <- reactive({
      sapply(add.amt, function(x) round(price() * (1 - per.down()), digits = 2))
})
add.total.interest <- reactive({
      round(((base.monthly.payment() + add.amt) * add.n()) - prin(), digits = 2)
})
savings <- reactive({
      round(base.total.interest() - add.total.interest(), digits = 2)
})
early <- reactive({
      round(n() - add.n(), digits = 0)
})
schedule <- reactive({
      data.frame(add.amt, add.n(), prin(), add.total.interest(), savings(), early())
})
add <- reactive({as.numeric(input$add)})
output$savings <- renderPrint({
      schedule()$savings[which(schedule()$add.amt == add())]
})
output$early <- renderPrint({
      schedule()$early[which(schedule()$add.amt == add())]
})
# ## create data.frame suitable for plotting
# extra.prin <- rep(seq(0, 1000, 25), 2)
# amount <- reactive({
#       as.vector(c(schedule()$prin, schedule()$add.total.interest), mode = "numeric")
# })
# type <- rep(c("Principal", "Interest"), each = 41)
# graph.data <- reactive({
#       data.frame(extra.prin, amount(), type)
# })
# ## create plot of amoritization with line for additional principal amount
# output$plot <- renderPlot({
# plot <- reactive({
#       ggplot(data = graph.data(), aes(x = extra.prin, y = amount), color = type) +
#       geom_area(aes(fill = type), position = 'stack', alpha = 0.75) +
#       geom_vline(xintercept = add(), color="black", linetype = "longdash", size = 1) +
#       labs(x = "Additional Principal/Month", y = "Total Cost") +
#       scale_fill_manual(values=c("firebrick3", "dodgerblue3"), name = "Payment Component") +
#       theme(axis.title.x = element_text(face = "bold", vjust = 0.7, size = 16), 
#         axis.title.y = element_text(face = "bold", vjust = 1, size = 16),
#         axis.text.x = element_text(size = 14), 
#         axis.text.y = element_text(size = 14), 
#         panel.margin = unit(c(5, 5, 5, 5), "mm"),
#         plot.margin = unit(c(5, 5, 5, 5), "mm"),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(colour = "gray"),
#         panel.grid.minor.y = element_line(colour = "gray86"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#       scale_x_continuous(labels = dollar) +
#       scale_y_continuous(labels = dollar)
# })
# print(plot())
# })
})
})