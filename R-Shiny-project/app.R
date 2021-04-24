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

q1 <- matrix(ncol=2)
q2 <- matrix(ncol=2)

# 1)CONSTANTE DE MEDIU
#A_1[i] - t > rabdarea -> LEAVE
#Cum accesam i?
N_prog <- 0.5 # numarul de ore in care centrul primeste clienti
N_extra <- 0.5 # numarul de ore peste program  
N_max2 <- 5 #numarul maxim de scaune in sala de asteptare la serverul 2.
N_max1 <- 4 #numarul maxim de scaune in sala de asteptare la serverul 1. Dimensiuna maxima a cozii 
N_dif <- 1 #numarul maxim de scaune libere care ar putea fi ocupate de clienti
            #serviti de la serverul 1 si cand serverul 2 este plin
N_rabdare <- 2 * 60  #minute dupa care un client paraseste serverul - isi pierde rabdarea
                    #TODO: RBinom()
# 2) INITIALIZARE

t <- 0 # variaabila de timp t
n1 <- 0 # nr de clienti de la serverul 1
n2 <- 0 # nr de clienti de la serverul 2

SS <- c(n1, n2) # starea sistemului

N_A<- 0 # nr sosiri pana la momentul t
N_D <- 0 # nr de plecari pana la momentul t
N_loss <- 0 #nr de clienti care au parasit sistemul fara sa fie serviti
infoServ <- matrix(nrow=2, ncol=6) #vector ce va retine informatiile generate in urma unei simulari 

#infoServ[i,1] -> timpul minim de asteptare la serverul i DONE
#infoServ[i,2] -> timpul maxim de asteptare la serverul i DONE
#infoServ[i,3] -> timpul mediu de asteptare la serverul i DONE
#infoServ[i,4] -> nr de clienti pierduti la serverul i DONE
#infoServ[i,5] -> nr total de clienti serviti la serverul i DONE

#infoServ[i,6] -> primul moment de timp la care se pierde un client la serverul i

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


#Generare initiala
T_0 <- generare_T_s(0, lambda)
t_A <<- T_0
t_1 <<- Inf
t_2 <<- Inf

#-------------------------------------------------------------------------------

# 2: CAZUL 1

generare_gamma <- function(lambda) {
    U <- runif(1)
    scale <- lambda
    return ((-1 / scale) * sum(log(U)))
}

cazul_1 <- function(){ # vede variabilele din afara 
    # caz: t_A = min(t_A, t_1, t_2)

    t <<- t_A
    T_t <<- generare_T_s(t, lambda)
    t_A <<- T_t
    
    #Daca persoanele aflate in asteptare la serverul 2 ocupa un nr max de scaune
    # trebuie sa lasam libere scaune in caz ca serverul 1 serveste mai repede
    # ca sa nu riscam ca mai multi oameni care vin din serverul 1 la serverul 2
    # sa "ramana fara scaun"
    if(n1 >= N_max1 || (n1 > 1 && n2 >= N_max2 - N_dif)){
        #Daca totusi se intampla, pierdem
        #ASK_COJO : Unde il punem?
        N_loss <- N_loss + 1
        
        message("Client respins la caz1")
        message(c("n1=",n1))
        message(c("n2=",n2))
        
    }else{
        if(n1 == 0){
            n1 <<- n1 + 1
        }else{
            #Generam cu rbinom timpul lui de astptare
            n_rabdare <- rbinom(1, 100, 0.2)
            q1 <<- rbind(q1, c(t, n_rabdare))
            n1 <<- n1 + 1 
        }
            
        if(n1 == 1){
            Y_1 <- generare_gamma(lambda)
            if( Y_1 < infoServ[1,1]){
                infoServ[1,1] = Y_1
            }else if(Y_1 > infoServ[1,2]){
                infoServ[1,2] = Y_1
            }
            print(c("Y_1 generat la caz1:",Y_1))
            infoServ[1,3] <<- infoServ[1,3] + Y_1
            t_1 <<- t + Y_1
        }
        
        # Output cazul 1)
        A_1_temp <- A_1
        A_1 <<-  c(A_1, t)
    }
}



# 3: CAZUL 2

cazul_2 <- function (){
    # caz: t_1 < t_A && t_1<= t_2
    t <<- t_1
    if(n1 > 0){
        n1 <<- n1 - 1
    }
    #Eliminam prima persoana din coada, deoarece este uramtoarea ce va fi servita
    curent <<- q1[1]
    q1 <<- q1[-1: -2, ]
    #Adaugam la numarul total de clienti serviti de catre serverul 1
    infoServ[1,5] <<- infoServ[1,5] + 1
    #Daca nu este niciun client in asteptare la serverul 2, trimitem noul client 
    #Direct la procesarea de catre serverul 2
    if(n2 == 0){
        n2 <<- n2 + 1
    
    }else{
        #Altfel, insamna ca exista un client care este servit in acest moment 
        #Deci il adaugam pe acest nou client in coada. 
        #Regeneram constanta de rabdare pentru clientul care acum va intra in serverul 2
        n_rabdare <- rbinom(1,100,0.2)
        #Introducem clientul in serverul2
        q2 <<- rbind(q2, c(t, n_rabdare))
        n2 <<- n2 + 1 
    }
    #Daca este singurul client la serverul 2, generam timpul de procesare al acestuia
    if(n2 == 1){
        Y_2 <<- generare_gamma(lambda)
        if( Y_2 < infoServ[2,1]){
            infoServ[2, 1] = Y_2
        }else if(Y_2 > infoServ[2,2]){
            infoServ[2, 2] = Y_2
        }
        #Actualizam timpul mediu  petrecut la serverul 2
        infoServ[2,3] <- infoServ[2,3] + Y_2
        print(c("Y_2 generat la caz2:",Y_2))
        t_2 <<- t + Y_2
    }
    
    if(n1 == 0){
        t_1 <<- Inf
    } else{
        Y_1 <<- generare_gamma(lambda)
        if( Y_1 + t - curent[1] < infoServ[1,1]){
            infoServ[1,1] = Y_1 + t - curent[1]
        }else if(Y_1 + t - curent[1]> infoServ[1,2]){
            infoServ[1,2] = Y_1 + t - curent[1]
        }
        #Actualizam la timpul total de servire server 1
        infoServ[1,3] <- infoServ[1,3] + Y_1 + t - curent[1]
        print(c("Y_1 generat la caz2:",Y_1))
        t_1 <<- t + Y_1
    }
    
    
    
    # Output cazul 2)
    # Nobody cares
    N_temp <- N_A - n1
    A_2_temp <- A_2
    if(N_temp <= length(A_2))
        A_2[N_temp] <<- t
    else
        print("N_temp > length (A_2)")
        A_2 <<- c(A_2, t)
}



# 4: CAZUL 3

cazul_3 <- function() {
    # t_2 < t_A && t_2 < t_1
    t <<- t_2
    N_D <<- N_D + 1
    infoServ[2,5] <<- N_D
    if(n2 > 0 ){
        n2 <<- n2 - 1
    }
    #Adaugam timpul petrecut de clientul din coada la timpii de aste
    curent <- q2[1]
    #Eliminam din coada clientul servit.
    q2 <<- q2[-1:-2,]
    if(n2 == 0) {
        t_2 <<- Inf
    }
    
    if(n2 > 0) {
        Y_2 <- generare_gamma(lambda)
        #Actualizam timpul minim, maxim  petrecut la serverul 2
        if( Y_2 + t - curent[1]< infoServ[2,1]){
            infoServ[2,1] = Y_2 + t - curent[1]
        }else if(Y_2 > infoServ[2,2]){
            infoServ[2,2] = Y_2 + t - curent[1]
        }
        
        #Actualizam timpul mediu  petrecut la serverul 2
        infoServ[2, 3] <- infoServ[2,3] + Y_2 + t - curent[1]
        print(c("Y_2 generat la caz3:",Y_2))
        t_2 <<- t + Y_2
    }
    
    # Output cazul 3)
    # Nobody cares
    D_temp <- D
    D <<- append(D_temp, t, after=N_D)
}
#-------------------------------------------------------------------------------
resetare_variabile <- function(){
    #O noua zi, resetam masura de timp
    t <<- 0
    #O noua zi, niciun client de procesat, asteptam clientii
    n1 <<- 0
    n2 <<- 0
    
    #Generam T_0 prin care setam momentul de timp al sosirii primului client
    T_0 <- generare_T_s(0, lambda)
    
    t_A <<- T_0
    t_1 <<- Inf
    t_2 <<- Inf
    
    
    A_1 <<- c()
    A_2 <- c() 
    D <<- c() 
    N_A <<- 0
    N_D <<- 0
    
    q1 <<- c()
    q2 <<- c()
    
    #Timp minim petrecut
    infoServ[1,1] <<- Inf
    infoServ[2,1] <<- Inf
    
    #Timp maxim petrecut
    infoServ[1,2] <<- 0
    infoServ[2,2] <<- 0
    
    #Timp petrecut 
    infoServ[1,3] <<- 0
    infoServ[2,3] <<- 0
    
    #N clienti pierduti
    infoServ[1,4] <<- 0
    infoServ[2,4] <<- 0
    
    #N clienti serviti
    infoServ[1,5] <<- 0
    infoServ[2,5] <<- 0
    
    #Primul mom de timp la care se pierde un client
    infoServ[1,6] <<- 0
    infoServ[2,6] <<- 0
    
}
# ------------------------------------------------------------------------------


simulare_zi <- function(dummy = 1) {
    
    #De fiecare data cand apelam simulare_zi, ptc folosim variabile globale
    #Avem nevoie sa dam reset
    resetare_variabile()
    #Initial, consideram ca centrul isi incepe activitatea la timpul 0.
    #Consideram ca fiecare variabila de timp reprezinta numarul de secunde 
    #De la deschiderea centrului.
    #Astfel, este nevoie sa ne oprim atunci cand t depaseste N_prog * 3600 = 43200
    
    while(TRUE){
        #message(c("N_D: ",N_D))
        print("-----------------------")
       
        
        
        #Centrul se va inchide daca una din cele 2 conditii este indeplinita:
        #1. Fie au trecut si numarul de ore extra pe care le-am alocat servirii
        #2. Fie programul de primire s-a terminat si am reusit sa servim toti clientii
        if(t >= (N_prog + N_extra) * 3600 || 
           t >= N_prog * 3600 && n1 == 0 && n2 == 0){
            #TODO:
            #   return metrics -> N_loss, length(A_2) numar doze,
            print(c("Ramasi in n1", n1))
            print(c("Ramasi in n2", n2))
            print(c("t_A=",t_A))
            print(c("t_1=",t_1))
            print(c("t_2=",t_2))
            print(c("Doze folosite=",length(A_2)))
            
            #Media timpului de astepare pentru cele 2 servere
            infoServ[1,3] <- infoServ[1,3] / infoServ[1,5]
            infoServ[2,3] <- infoServ[2,3] / infoServ[2,5]
            
            break
        }
        if(t_A == min(t_A, t_1, t_2)){
            print("Intra cazul 1")
            #Primim clientii doar daca se indeplinesc simultan:
            #1. Timpul curent nu a depasit programul de munca
            #2. Coada serverului 2 nu este la capacitate maxima
            #3. Coada serverului 1 nu este la capacitate maxima
            if(t < N_prog * 3600){
                cazul_1()
            }
            else{
                t_A <- Inf
                #print("Au trecut orele de primire clienti. Overtime.")
            }
        }
        else if(t_1 < t_A && t_1 <= t_2){
            print("Intra cazul 2")
            cazul_2()
        }
        else if(t_2 < t_A && t_2 < t_1){
            print("Intra cazul 3")
            cazul_3()
        }
        #Dat fiind t momentul de timp actual, eliminam din cele 2 cozi toti clientii
        #care si-au pierdut rabdarea
        len_q1_inainte <- length(q1[,1])
        len_q2_inainte <- length(q2[,1])
        if(length(q1) > 0 ){
            if(infoServ[1,6] == 0){
                q1_de_elim <- subset(q1, q1[, 1] + q1[, 2] <= t)
                q1 <<- subset(q1, q1[, 1] + q1[, 2] > t)
                
                if(length(q1_de_elim) > 0){
                    infoServ[1,6] = q1_de_elim[1, 1] + q1_de_elim[1, 2]
                }
            }else{
                q1 <<- subset(q1, q1[, 1] + q1[, 2] > t)
            }
            
        }
        if(length(q2) > 0){
            if(infoServ[2,6] == 0){
                q2_de_elim <- subset(q2, q2[, 1] + q2[, 2] <= t)
                q2 <<- subset(q2, q2[, 1] + q2[, 2] > t)
                
                if(length(q2_de_elim) > 0){
                    infoServ[2,6] = q2_de_elim[1, 1] + q2_de_elim[1, 2]
                }
            }else{
                q2 <<- subset(q2, q2[, 1] + q2[, 2] > t)
            }
           
        }
        n1 <<- n1 + len_q1_inainte - length(q1[,1])
        n2 <<- n2 + len_q2_inainte - length(q2[,1])
        print("Q1 and Q2:")
        print(q1)
        print(q2)
        print(c("t_A=",t_A))
        print(c("t_1=",t_1))
        print(c("t_2=",t_2))
        print(c("n1=",n1))
        print(c("n2=",n2))
        
        #Numarul de clienti pierduti per fiecare sever este diferenta dintre lungimea dinainte
        #de eliminare si lungimea curenta
        if(len_q1_inainte > length(q1))
            infoServ[1, 4] <<- infoServ[1, 4] + (len_q1_inainte - length(q1))
        if(len_q2_inainte > length(q2))
            infoServ[2, 4] <<- infoServ[2, 4] + (len_q2_inainte - length(q2))
        
    }
    print(c("We're closed. Fuck off.Last t:", t))
}
#castig[3,1] 
#Rulam simularea pentru 7 zile ->castig normal aka castig[1,1]
#Rulam simularea pentru 7 zile, dar N_prog ++ ->castig[2,1]
#Rulam simularea pentru 7 zile, dar N_prog -- ->castig[3,1]
#castig[2,1] - castig[1,1] pentur N_prog ++
#castig[3,1] - castig[1,1] pentur N_prog --

#Simulare_zi va fi metoda apelata pentru a obtine metricele aferente unei 
#zile lucratoare
#for i in 1:7{
    #In fiecare zi din perioada specificata,e.g. 1:7 o saptamana
    #Rulam 10^6 scenarii posibile
#    for sim in 1:1e6
        #metrics = simulare_zi()
        
    #Pentru fiecare dintre cele 10^6 valori ale metricelor, 
    #Calculam o valoare medie orientativa pentru ziua respectiva. 
    #Adaugam in grafice
    #pe Ox numarul zilei si pe Oy una din metrice. 
    #Cate un grafic per metrica. 
#}

#-------------------------------------------------------------------------------
simulare_zi()
main <- function(){
    
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
#shinyApp(ui = ui, server = server)
