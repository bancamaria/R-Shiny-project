#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# n_0 = 100
# a_0 = 20220
# lambda = 0.01


lambda <- 0.01
lambda_function <- function(t) {
    if(0 <= t <= 6){
        return( 0.01 * t^4 + 0.03 * t^3 + 0.05 * t^2 - 0.25 * t + 20) 
    }
    else{
        return(15)
    }
}

# ?????
# posibil ca profa sa doreasca sa scriem noi functiile de repartitie

# VARIABILE DE OUTPUT

A_1 <- c()

# 1) INITIALIZARE

t <- 0 # variaabila de timp t

n1 <- 0 # nr de clienti de la serverul 1
n2 <- 0 # nr de clienti de la serverul 2

ss <- tuple(n1, n2) # starea sistemului

N_A<- 0 # nr sosiri pana la momentul t
N_D <- 0 # nr de plecari pana la momentul t

# Generare T_s

generare_T_s <- function(s, lambda){
    
    T_s <- 0
    
    repeat{
        # 1. t = s
        t <- s
        
        # 2. generam U_1 , U_2
        U_1 <- runif(1)
        U_2 <- runif(1)
        
        # 3.
        t <- t - (1 / lambda) * log(U_1)
        
        # 4.
        if(U_2 <= lambda_function(t)){
            T_s <- t
            break;
        }
    }
    
    return(T_s)
}

T_0 <- generare_T_s(0, lambda)
t_A <- T_0
t_1 <- Inf
t_2 <- Inf

# 2) CAZUL 1

generare_gamma <- function(lambda) {
    U <- runif(1)
    scale <- lambda
    return ((-1 / scale) * sum(log(U)))
}

cazul_1 <- function(){ # vede variabilele din afara ???
    t_A = min(t_A, t_1, t_2)
    t <- t_A
    N_A = N_A + 1
    n1 = n1 + 1
    
    T_t <- generare_T_s(t, lambda)
    t_A = T_t
    
    if(n1 == 1){
        Y_1 = generare_gamma(lambda)
        t_1 = t + Y_1
    }
    
    # Output cazul 1)
    A_1 <- c(A_1, t)
}

cazul_1()
# ------------------------------------------------------------------------------

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
