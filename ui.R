library(shiny)
shinyUI(
      pageWithSidebar(
      headerPanel(
            h1('Amoritization Simulator for Home Mortgages'), 
            windowTitle = "Amoritization Simulator"
      ),
      sidebarPanel(
            h3('Mortgage Information'),
            h4('Purchase Price'),
            p('Enter the total sale price of the home'),
            textInput('price', "Sale Price ($USD)", value = ""),
            h4('Percent Down Payment'),
            p('Use the slider to select the percent of the purchase price you 
              intend to pay as a down payment at the time of purchase'),
            sliderInput('per.down', "% Down Payment", value = 20, min = 0, max = 30, step = 1),
            h4('Interest Rate (APR)'),
            p('Use the slider to select the interest rate of the loan expressed
              as an Annual Percentage Rate (APR)'),
            sliderInput('apr', "APR", value = 4, min = 0, max = 8, step = 0.125),
            h4('Term Length (Years)'),
            p('Use the slider to define the term of the loan'),
            sliderInput('term', "Loan Term Years", value = 30, min = 15, max = 30, step = 1),
            submitButton("Calculate")
            ),
      mainPanel(
            h3('Payment and Amoritization Simulation'),
            p('Use this tool to determine your monthly mortgage payment, 
              how much interest you will owe over the life of the loan, and how 
              you can reduce that amount with additional payment'),
            h4('Monthly Payment (Principal and Interest)'),
            p('This is the amount (in $USD) you would pay each month for a 
              mortgage under the terms you defined'),
            verbatimTextOutput("base.monthly.payment"),
            h4('Total Interest Over Life of Loan'),
            p('If paying just that amount per month, this is the total amount 
              in $USD you will spend on interest for that loan'),
            verbatimTextOutput("base.total.interest"),
            h4('Additional Principal Simulation'),
            p('One way to reduce the interest expense is to pay more principal 
              each month. Use the slider below to select an additional amount to
              include with your payment and see the reduction in interest expense
              for the life of the loan.'),
            sliderInput('add', "Additional Principal ($USD)", value = 250, min = 0, max = 1000, step = 25),
            p('Interest costs saved with this additional principal (in $USD)'),
            verbatimTextOutput("savings"),
            p('You will also pay the loan off the loan this many months early'),
            verbatimTextOutput("early"),
            plotOutput('plot')
            )
      )
)