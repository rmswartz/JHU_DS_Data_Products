library(shiny)
shinyUI(
      pageWithSidebar(
      headerPanel(
            h1('Amoritization Simulator for Home Mortgages'),
            p('Use this tool to determine your monthly mortgage payment, 
              how much interest you will owe over the life of the loan, and how 
              you can reduce that amount with additional payment')
      ),
      sidbarPanel(
            h3('Mortgage Information'),
            h4('Purchase Price'),
            p('Enter the total sale price of the home in $USD'),
            h4('Percent Down Payment')
            p('Use the slider to select the percent of the purchase price you 
              intend to pay as a down payment at the time of purchase'),
            sliderInput('per.down', value = 20, min = 0, max = 30, step = 1),
            h4('Interest Rate (APR)'),
            p('Use the slider to select the interest rate of the loan expressed
              as an Annual Percentage Rate (APR)'),
            sliderInput('apr', value = 5, min = 0, max = 15, step = 1),
            h4('Term Length (Years)'),
            p('Use the buttons to define the term of the loan in years'),
            radioButtons('term', choices = c(15, 30), selected = 30)
            submitButton('Calculate')
            )
      mainPanel(
            h3('Payment and Amoritization Simulation'),
            h4('Monthly Payment (Principal and Interest)'),
            p('This is the amount you would pay each month for a mortgage under 
              the terms you defined'),
            h4('Total Interest Over Life of Loan'),
            p('If paying just that amount per month, this is the total amount 
              you will spend on interest for that loan'),
            h4('Additional Principal'),
            p('One way to reduce the interest expense is to pay more principal 
              each month. Use the slider below to select an additional amount to
              include with your payment and see the reduction in interest expense
              for the life of the loan.'),
            sliderInput('add', value = 250, min = 0, max = 1000, step = 25)
            )
      )
)