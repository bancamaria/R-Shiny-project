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

library(R.utils)

lambda <- 0.01
lambda_function <- function(t) {
    if(0 <= t && t <= 6){
        return( 0.01 * t^4 + 0.03 * t^3 + 0.05 * t^2 - 0.25 * t + 20) 
    }
    else{
        return(15)
    }
}

# ?????
# posibil ca profa sa doreasca sa scriem noi functiile de repartitie

# VARIABILE DE OUTPUT

A_1 <- c() # momentul sosirii clientului la serverul 1
A_2 <- c() # momentul sosirii clientului la serverul 2
D <- c() # momentul plecarii clientului din sistem

# 1) INITIALIZARE

t <- 0 # variaabila de timp t

n1 <- 0 # nr de clienti de la serverul 1
n2 <- 0 # nr de clienti de la serverul 2

SS <- c(n1, n2) # starea sistemului

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

# 2: CAZUL 1

generare_gamma <- function(lambda) {
    U <- runif(1)
    scale <- lambda
    return ((-1 / scale) * sum(log(U)))
}

cazul_1 <- function(t_A){ # vede variabilele din afara ???
    # caz: t_A = min(t_A, t_1, t_2)
    t <- t_A
    N_A <- N_A + 1
    n1 <- n1 + 1
    
    T_t <- generare_T_s(t, lambda)
    t_A <- T_t
    
    if(n1 == 1){
        Y_1 <- generare_gamma(lambda)
        t_1 <- t + Y_1
    }
    
    # Output cazul 1)
    A_1_temp <- A_1
    A_1 <- append(A_1_temp, t, after = N_A)
}

cazul_1(20)

# 3: CAZUL 2

cazul_2 <- function (){
    # caz: t_1 < t_A && t_1<= t_2
    t <- t_1
    n1 <- n1 - 1
    n2 <- n2 + 1
    
    if(n1 == 0){
        t_1 <- Inf
    } else{
        Y_1 <- generare_gamma(lambda)
        t_1 <- t + Y_1
    }
    
    if(n2 == 1){
        Y_2 <- generare_gamma(lambda)
        t_2 <- t + Y_2
    }
    
    # Output cazul 2)
    N_temp <- N_A - n1
    A_2_temp <- A_2
    A_2 <- append(A_2_temp, t, after = N_temp)
}

cazul_2()

# 4: CAZUL 3

cazul_3 <- function() {
    # t_2 < t_A && t_2 < t_1
    t <- t_2
    N_D <- N_D + 1
    n2 <- n2 - 1
    if(n2 == 0) {
        t_2 <- Inf
    }
    
    if(n2 == 1) {
        Y_2 <- generare_gamma(lambda)
        t_2 <- t + Y_2
    }
    
    # Output cazul 3)
    D_temp <- D
    D <- append(D_temp, t, after=N_D)
}

cazul_3()

# ------------------------------------------------------------------------------

main <- function() {
    
}

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