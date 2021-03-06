
# n_0 = 100
# a_0 = 20220
# lambda = 0.01

library(R.utils)

#FUNCTII PENTRU GENERAREA DE VARIABILE ALEATOARE CU DISTRIBUTIILE DATE
lambda <- 0.01
#Generarea functiei de intensitate a procesului Poisson omogen de parametru t
#Parametrul t va fi exprimat in secunde de la inceperea unei simulari, 
#Prin urmare, imparti cu 3600 pentru a obtine ora respectiva 
gen_intensitate <- function(t) {
    h <-  t / 3600 
    if(0 <= h && h <= 2){
        return(23) 
    }else if(2 < h && h <= 6){
        return(15 * t ^ 2 + 3 * t -2)
    }
    else if(6 < h && h <= 10){
        return(3*log(t) + 15 * t^2 + -1)
    }
    return(12)
}

#Generarea lui Y1, o v.a. discreta cu urmatoarea distributie
#Valorile posibile ale lui Y1, sortate descrescator in functie de probabilitate
Y1_val <- c(5, 7, 11, 12, 23, 24)
Y1_prob <- c(20/60, 30/60, 2/60, 5/60, 2/60, 1/60)
sum_prob <- cumsum(Y1_prob) 
gen_y1 <- function(){
    #Generam uniforma
    u <-  runif(1,0,1)
    return(Y1_val[which(u < sum_prob)][1])
    
}

#Generarea unei variabile din distributia Exponentiala de parametru lambda
gen_exp <- function(n, lambda){
    U <- runif(n,0,1)
    return( -1/lambda * log(U))
}

#Generarea lui Y2, conform densitatii de probabilitate :
#f(x) = 2 /5 * x * exp((-1 * x ^2) / 25)
#Estimat prin metoda respingerii 
gen_y2 <- function(){
    while(TRUE){
        Y <- gen_exp(1,lambda = 1/5)
        u <- runif(1,0,1)
        if(u <=  1/5 * Y * exp((Y/5) * (1 - Y/5))){
            return(Y)
        }
    }
}

#Generarea Constantelor de rabdare printr-o repartitie Binomiala cu size=100, p=0.2
gen_binom <- function(size, p){
    #Initializam
    c <- p / (1-p)
    i <- 0
    prob <- (1-p) ^ size
    f <- prob
    while(TRUE){
        #Generam u -> Unif(0,1)
        u <- runif(1,0,1)
        if(u <= f)
            return(i)
        prob <- c * (size - i ) / (i +1) * prob
        f <-  f + prob 
        i <- i + 1
    }
    
}
# VARIABILE DE OUTPUT

A_1 <- c() # momentul sosirii clientului la serverul 1
A_2 <- c() # momentul sosirii clientului la serverul 2
D <- c() # momentul plecarii clientului din sistem

q1 <- matrix(nrow=0,ncol=2)
q2 <- matrix(nrow=0,ncol=2)

# 1)CONSTANTE DE MEDIU

#A_1[i] - t > rabdarea -> LEAVE

N_prog <- 12 # numarul de ore in care centrul primeste clienti
N_extra <- 0.5 # numarul de ore peste program  
N_max2 <- 5 #numarul maxim de scaune in sala de asteptare la serverul 2.
N_max1 <- 4 #numarul maxim de scaune in sala de asteptare la serverul 1. Dimensiuna maxima a cozii 
N_dif <- 1 #numarul maxim de scaune libere care ar putea fi ocupate de clienti
            #serviti de la serverul 1 si cand serverul 2 este plin
N_doze <- 100 #numarul de doze disponibile in fiecare zi. Profitul va reprezenta
N_zile <- 7 #numarul de zile asupra caruia facem observarea

# 2) INITIALIZARE

t <- 0 # variabila de timp t
n1 <- 0 # nr de clienti de la serverul 1
n2 <- 0 # nr de clienti de la serverul 2

SS <- c(n1, n2) # starea sistemului

N_A<- 0 # nr sosiri pana la momentul t
N_D <- 0 # nr de plecari pana la momentul t
N_loss <- 0 #nr de clienti care au parasit sistemul fara sa fie serviti
infoServ <- matrix(nrow=2, ncol=6) #vector ce va retine informatiile generate in urma unei simulari 

#infoServ[i,1] -> timpul minim de asteptare la serverul i 
#infoServ[i,2] -> timpul maxim de asteptare la serverul i 
#infoServ[i,3] -> timpul mediu de asteptare la serverul i 
#infoServ[i,4] -> nr de clienti pierduti la serverul i 
#infoServ[i,5] -> nr total de clienti serviti la serverul i 
#infoServ[i,6] -> primul moment de timp la care se pierde un client la serverul i

# Algoritm de generare T_s
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
        if(U_2 <= gen_intensitate(t) / lambda){
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
    
    # Daca persoanele aflate in asteptare la serverul 2 ocupa un nr max de scaune
    # trebuie sa lasam libere scaune in caz ca serverul 1 serveste mai repede
    # ca sa nu riscam ca mai multi oameni care vin din serverul 1 la serverul 2
    # sa "ramana fara scaun"
    if(n1 >= N_max1 || (n1 > 1 && n2 >= N_max2 - N_dif)){
        #Daca totusi se intampla, pierdem
        N_loss <- N_loss + 1
        
    }else{
        if(n1 == 0){
            n1 <<- n1 + 1
        }else{
            #Generam cu rbinom timpul lui de astptare
            n_rabdare <- gen_binom(100, 0.2)
            q1 <<- rbind(q1, c(t, n_rabdare))
            n1 <<- n1 + 1 
        }
            
        if(n1 == 1){
            #Functiile de generare dupa datele primite genereaza valori mici
            #In comparatie cu t. Ne vom asuma ca aceste valori sunt, de fapt, minute
            Y_1 <- gen_y1() * 60
            if( Y_1 < infoServ[1,1]){
                infoServ[1,1] <<- Y_1
            }else if(Y_1 > infoServ[1,2]){
                infoServ[1,2] <<- Y_1
            }
            #print(c("Y_1 generat la caz1:",Y_1))
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
        n_rabdare <- gen_binom(100,0.2)
        #Introducem clientul in serverul2
        q2 <<- rbind(q2, c(t, n_rabdare))
        n2 <<- n2 + 1 
    }
    #Daca este singurul client la serverul 2, generam timpul de procesare al acestuia
    if(n2 == 1){
        #Generarea variabilelor conform repartitiei date de f2(x) genereaza date prea mici
        #in comparatie cu t, motiv pentru care ne asumam ca aceste valori ar fi, 
        #de fapt, minute. Asadar, le vom converti in secunde
        Y_2 <<- gen_y2() * 60 
        if( Y_2 < infoServ[2,1]){
            infoServ[2, 1] <<- Y_2
        }else if(Y_2 > infoServ[2,2]){
            infoServ[2, 2] <<- Y_2
        }
        #Actualizam timpul mediu  petrecut la serverul 2
        infoServ[2,3] <<- infoServ[2,3] + Y_2
        #print(c("Y_2 generat la caz2:",Y_2))
        t_2 <<- t + Y_2
    }
    
    if(n1 == 0){
        t_1 <<- Inf
    } else{
        Y_1 <<- gen_y1() * 60
        #print(c("Y_1 generat la caz2:",Y_1))
        t_1 <<- t + Y_1
        
        #Eliminam prima persoana din coada, deoarece este uramtoarea ce va fi servita,
        #Daca exista perosoane in coada
        if( nrow(q1) > 0){
            #print("before Pop:")
            #print(q1)
            curent <- q1[1, ]
            q1 <<- q1[-1,,drop=FALSE]
            #print("after Pop:")
           # print(dim(q1))
           # print(q1)
            if( Y_1 + t - curent[1] < infoServ[1, 1]){
                infoServ[1,1] <<- Y_1 + t - curent[1]
            }else if(Y_1 + t - curent[1]> infoServ[1,2]){
                infoServ[1,2] <<- Y_1 + t - curent[1]
            }
            #Actualizam la timpul total de servire server 1
            infoServ[1,3] <<- infoServ[1,3] + Y_1 + t - curent[1]
        }
        
    }
    
    # Output cazul 2)
    N_temp <- N_A - n1
    A_2_temp <- A_2
    if(N_temp <= length(A_2))
        A_2[N_temp] <<- t
    else
        #print("N_temp > length (A_2)")
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
    
    if(n2 == 0) {
        t_2 <<- Inf
    }else {
        Y_2 <- gen_y2() * 60 
        #print(c("Y_2 generat la caz3:",Y_2))
        t_2 <<- t + Y_2
        
        if(nrow(q2) > 0){
            #Adaugam timpul petrecut de clientul din coada la timpii de asteptare
            curent <- q2[1, ]
            #Eliminam din coada clientul servit.
            q2 <<- q2[-1,,drop=FALSE]
            #q2 <<- q2[-1,]

            #Actualizam timpul minim, maxim  petrecut la serverul 2
            if( Y_2 + t - curent[1]< infoServ[2,1]){
                infoServ[2,1] <<- Y_2 + t - curent[1]
            }else if(Y_2 > infoServ[2,2]){
                infoServ[2,2] <<- Y_2 + t - curent[1]
            }
            #Actualizam timpul mediu  petrecut la serverul 2
            infoServ[2, 3] <<- infoServ[2,3] + Y_2 + t - curent[1]
            
        }
        
        
    }
    
    # Output cazul 3)
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
    
    q1 <<- matrix(nrow=0,ncol=2)
    q2 <<- matrix(nrow=0,ncol=2)
    
    #Fiecare zi incepe cu 100 de doze
    N_doze <<- 100 
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

simulare_zi <- function(dummy = 1,n_sim, n_zi) {
    
    #De fiecare data cand apelam simulare_zi, ptc folosim variabile globale
    #Avem nevoie sa dam reset
    resetare_variabile()
    #Initial, consideram ca centrul isi incepe activitatea la timpul 0.
    #Consideram ca fiecare variabila de timp reprezinta numarul de secunde 
    #De la deschiderea centrului.
    #Astfel, este nevoie sa ne oprim atunci cand t depaseste N_prog * 3600 = 43200
    while(TRUE){
       # print("-----------------------")
        
        #Centrul se va inchide daca una din cele 2 conditii este indeplinita:
        #1. Fie au trecut si numarul de ore extra pe care le-am alocat servirii
        #2. Fie programul de primire s-a terminat si am reusit sa servim toti clientii
        if(t >= (N_prog + N_extra) * 3600 || 
           t >= N_prog * 3600 && n1 == 0 && n2 == 0){
            #   return metrics -> N_loss, length(A_2) numar doze,
            #print(c("Ramasi in n1", n1))
            #print(c("Ramasi in n2", n2))
            #print(c("t_A=",t_A))
            #print(c("t_1=",t_1))
            #print(c("t_2=",t_2))
            #print(c("Doze folosite=",N_D))
            
            #Media timpului de astepare pentru cele 2 servere
            infoServ[1,3] <<- infoServ[1,3] / infoServ[1,5]
            infoServ[2,3] <<- infoServ[2,3] / infoServ[2,5]
            break
        }
        if(t_A == min(t_A, t_1, t_2)){
            #print("Intra cazul 1")
            #Primim clientii doar daca se indeplinesc simultan:
            #1. Timpul curent nu a depasit programul de munca
            #2. Coada serverului 2 nu este la capacitate maxima
            #3. Coada serverului 1 nu este la capacitate maxima
            if(t < N_prog * 3600){
                #print("Cazul 1")
                cazul_1()
            }
            else{
                t_A <<- Inf
                #print("Au trecut orele de primire clienti. Overtime.")
            }
        }
        else if(t_1 < t_A && t_1 <= t_2){
            #print("Intra cazul 2")
            cazul_2()
        }
        else if(t_2 < t_A && t_2 < t_1){
           # print("Intra cazul 3")
            cazul_3()
        }
        #Dat fiind t momentul de timp actual, eliminam din cele 2 cozi toti clientii
        #care si-au pierdut rabdarea
        len_q1_inainte <- nrow(q1)
        len_q2_inainte <- nrow(q2)
        if(nrow(q1) > 0){
            if(infoServ[1,6] == 0){
                q1_de_elim <- subset(q1, q1[, 1] + q1[, 2] <= t)
            
                q1 <<- subset(q1, q1[, 1] + q1[, 2] > t)
                if(nrow(q1_de_elim) > 0){
                    infoServ[1,6] <<- q1_de_elim[1, 1] + q1_de_elim[1, 2]
                }
            }else{
                q1 <<- subset(q1, q1[, 1] + q1[, 2] > t)
            }
            
        }
        if(nrow(q2) > 0){
            if(infoServ[2,6] == 0){
                q2_de_elim <- subset(q2, q2[, 1] + q2[, 2] <= t)
                q2 <<- subset(q2, q2[, 1] + q2[, 2] > t)
                
                if(nrow(q2_de_elim) > 0){
                    infoServ[2,6] <<- q2_de_elim[1, 1] + q2_de_elim[1, 2]
                }
            }else{
                q2 <<- subset(q2, q2[, 1] + q2[, 2] > t)
            }
           
        }
        n1 <<- n1 + nrow(q1) - len_q1_inainte
        n2 <<- n2 + nrow(q2) - len_q2_inainte  
        #print(c(length(q1[,1]), len_q1_inainte))
        #print(c(length(q2[,2]), len_q2_inainte))
        #print("After:Q1 and Q2:")
        #print(q1)
        #print(q2)
        #print(c("t_A=",t_A))
        #print(c("t_1=",t_1))
        #print(c("t_2=",t_2))
        #print(c("n1=",n1))
        #print(c("n2=",n2))
        
        #Numarul de clienti pierduti per fiecare sever este diferenta dintre lungimea dinainte
        #de eliminare si lungimea curenta
        if(len_q1_inainte > nrow(q1)){
           
            infoServ[1, 4] <<- infoServ[1, 4] + (len_q1_inainte - nrow(q1))
        }
        if(len_q2_inainte > nrow(q2))
            infoServ[2, 4] <<- infoServ[2, 4] + (len_q2_inainte - nrow(q2))
        
    }
    #print("Metrics:")
    #Odata obtinute valorile pentru o simulare pentru o zi, acutalizam matricea agregatoare
    #print(infoServ)
    for (i in 0:1){
        #Actualizarea timpului minim petrecut la serverul i din toate simularile
        agregator[2 * n_zi - i, 1] <<- min(agregator[2 * n_zi - i, 1], infoServ[2 - i, 1])
        #Actualizarea timpului maxim petrecut la serverul i din toate simularile
        agregator[2 * n_zi - i,2] <<- max(agregator[2 * n_zi - i ,2], infoServ[2 - i,2])
        #Actualizarea timpului mediu petrecut la serverul i din toate simularile
        agregator[2 * n_zi - i,3] <<- (1 / n_sim) * infoServ[2 - i , 3] + agregator[2 * n_zi - i, 3]
        #Actualizarea numarului mediu de clienti pierduti la serverul i
        agregator[2 * n_zi - i,4] <<- (1 / n_sim) * infoServ[2 - i, 4] + agregator[2 * n_zi - i, 4]
        #Actualizarea numarului mediu de clienti serviti la serverul i
        agregator[2 * n_zi - i,5] <<- (1 / n_sim) * infoServ[2 - i, 5] + agregator[2 * n_zi - i, 5]
        #Actualizarea numarului mediu de clienti serviti la serverul i
        agregator[2 * n_zi - i,6] <<- (1 / n_sim) * infoServ[2 - i, 6] + agregator[2 * n_zi - i, 6]
        
    }
    #print(c("Program incheiat.Last t:", t))
    #print(agregator)
    #Adunam la profitul mediu pentru aceasta zi, profitul obtinut pentru aceasta simulare
    
    profit <<- (1 / n_sim) * (N_doze - N_D) + profit
    #print(c("PROFIT:", profit))
    #print("-------------------------")
    
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

agregator <- matrix(nrow=2 * N_zile , ncol=6)
profit <- 0
profituri <- c()

main <- function(offset_ore_de_lucru) { # offset-ul este un string = "-1", "0" si "1" corespunzator scurtarii/prelungirii programului de lucru cu clientii
    n <- 100 # numarul de simulari pe zi
    saptamana <- c()
    profit <<- 0
    profituri <<- c()
    agregator <<- matrix(nrow=2 * N_zile , ncol=6)
    
    for(i in 1:N_zile){
        #Pentru fiecare zi rulam n simulari 
        agregator[2 * i - 1, 2:6] <<- 0
        agregator[2 * i, 2:6] <<- 0
        agregator[2 * i - 1, 1] <<- Inf
        agregator[2 * i, 1] <<- Inf
        profit <<- 0 
        sapply(1:n, simulare_zi, n_sim=n, n_zi=i)
        profituri <<- c(profituri, profit)
        #print(c("Sfarist de simulare pentru ziua: ",i))
        #print(agregator)
    }
    #Dupa finalizarea simularilor pentru N_zile, in agregator vom avea 
    #Pe fiecare rand de indice 2 * i - 1 informatii de la serverul 1 pentru ziua i
    #Pe fiecare rand de indice 2 * i  informatii de la serverul 2 pentru ziua i
    #par(mfrow=2, mfcol=2)
    ylabs <-  c("Timp minim de asteptare", 
                "Timp maxim de asteptare", 
                "Timp mediu de astepare ", 
                "Clienti pierduti", 
                "Clienti serviti",
                "Primul Moment La care se pierde un client")
    plot_grafic <- function(i){
        ymin <- min(agregator[, i])
        ymax <- max(agregator[, i])
        plot(seq(1,N_zile), agregator[seq(1,2*N_zile,2), i], col='red', xlab='Zilele Saptamanii',
             type='b',
             ylab=ylabs[i],
             ylim = c(ymin, ymax),
             main = paste(ylabs[i], offset_ore_de_lucru, sep = " "))
        points(seq(1,N_zile), agregator[seq(2,2*N_zile,2), i], col='blue', xlab='Zilele Saptamanii')
        lines(seq(1,N_zile), agregator[seq(2,2*N_zile,2), i], col='blue', xlab='Zilele Saptamanii',lty=1)
        legend("topright",
               legend = c("Serveul 1", "Serverul 2"), 
               col = c("red","blue"),
               bty="n",
               pch = "o",
               lty =1)
        }
    sapply(1:6, plot_grafic)
    
    #Plotare profituri
    plot(seq(1:N_zile), profituri,type='b',col="green",xlab="Zi", ylab="Profit", main = paste("Profit", offset_ore_de_lucru, sep = " "))
    
    return (mean(profituri));
}

analiza_3_simulari <- function() {
    N_prog_initial <- N_prog
    
    N_prog <<- N_prog_initial - 1
    medie_program_prescurtat <- main("-1")
    print(paste("Numar de ore program de lucru: ", N_prog))
    print(medie_program_prescurtat)

    N_prog <<- N_prog_initial
    medie_program_normal <- main("0")
    print(paste("Numar de ore program de lucru: ", N_prog))
    print(medie_program_normal)
    
    N_prog <<- N_prog_initial + 1
    medie_program_prelungit <- main("+1")
    print(paste("Numar de ore program de lucru: ", N_prog))
    print(medie_program_prelungit)
}

analiza_3_simulari()

