
library(shiny)
library(shinyalert)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    library(readxl)
    library(writexl)
    library(openxlsx)
    library('ggplot2')
    library('forecast')
    library('tseries')
    
    data <- read.csv('/Users/haritha/Documents/BTP1/rad.csv', header=TRUE, stringsAsFactors=FALSE)
    irra <- data$irradiation
    irr <- irra[1:8736]
    
    plot(irr)
    plot(diff(irr))
    lines(diff(irr))
    
    acf(irr)
    acf(diff(irr))
    pacf(diff(irr))
    
    (fit <- arima(irr, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 24)))
    pred <- predict(fit, n.ahead = 24)
    
    f <- unlist(pred, use.names=FALSE)
    f1 <- f[1:24]
    k1 <- append(irr,f1,length(irr))
    plot(irra)
    plot(k1)
    lines(k1)
    real <- irra[8737:8760]
    error <- real-f1
    
    plot(real, type="l", col = "green")
    plot(f1)
    
    
    
    price <- data$price
    pri <- price[1:8736]
    plot(pri)
    plot(diff(pri))
    lines(diff(pri))
    
    acf(pri)
    acf(diff(pri))
    pacf(diff(pri))
    
    (fit <- arima(pri, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 24)))
    pred1 <- predict(fit, n.ahead = 24)
    
    f2 <- unlist(pred1, use.names=FALSE)
    f3 <- f2[1:24]
    k2 <- append(pri,f3,length(pri))
    plot(price)
    plot(k2)
    lines(k2)
    real1 <- price[8737:8760]
    error1 <- real1-f3
    
    plot(real1, type="l", col = "green")
    plot(f3)
    
    
    prediction_data <- read_excel('/Users/haritha/Documents/BTP1/prediction data.xlsx')
    prediction_data$Grid_Price_per_kWh <- c((f3*0.71)[7:24],(f3*0.71)[1:6])
    prediction_data$Solar_Generation <- c((f1*0.71)[7:24],(f1*0.71)[1:6])
    status_table <- 1*(prediction_data[1:24,3:11] > 0)
    status_table[1:24,1:3] <- matrix(0,24,3)
    status_table <- as.data.frame(status_table)
    cl_table <- 1*(prediction_data[1:24,3:5] >0)
    cl_table <- as.data.frame(cl_table)
    
    reschedule <- function(status_table, cl_table, Solar_Generation,Grid_Price_per_kWh,c)
    {
        q <- 24-c+1
        prediction_data <- status_table[c:24, 1:9]
        prediction_data[1:q, 1:3] <- cl_table[c:24, 1:3]
        prediction_data[1:q,1:9] <- as.matrix(prediction_data) %*% as.matrix(diag(c(240,40,50,500,150,600,200,600,800)))
        #---------Loop from here--------#
        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
        prediction_EL <- prediction_load-Solar_Generation[c:24]
        #plot(prediction_data$`Start Time`,prediction_data$Solar_Generation, type = 's',ylim = c(0,1200),ylab = 'Generation in Watt',xlab = 'Time', main = 'Day-Ahead Forecasted Solar Generation (6AM to 6AM)', col = 'red')
        #plot(prediction_data$`Start Time`,prediction_data$Grid_Price_per_kWh, type = 's',ylim = c(0,40),ylab = 'Cost per kWh usage from Grid',xlab = 'Time', main = 'Day-Ahead Grid Price(6AM to 6AM)', col = 'red')
        #plot(prediction_data$`Start Time`,prediction_load, type = 's',ylim = c(0,1200),ylab = 'Watt',xlab = 'Time', main = 'Forecasted Load Profile', col = 'orange')
        #lines(prediction_data$`Start Time`,prediction_CL,type ='s',lty = 2, col = 'green')
        #lines(prediction_data$`Start Time`,prediction_ML,type ='s',lty = 5, col = 'blue')
        #lines(prediction_data$`Start Time`,prediction_NCL,type ='s',lty = 3, col = 'red')
        #legend(xpd = TRUE,'topright',legend = c('CL','ML','NCL','Total'),col = c('green','blue','red','orange'),lty = c(2,5,3,1),cex = 0.6,horiz = TRUE)
        NCL_hourly_cost <- c()
        slot_price <- c()
        for (i in 1:q) {
            if (prediction_EL[i]>0){
                if(prediction_NCL[i]<=prediction_EL[i]){
                    NCL_hourly_cost[i] <- prediction_NCL[i]*Grid_Price_per_kWh[i+c-1]/1000
                }
                else{
                    NCL_hourly_cost[i] <- prediction_EL[i]*Grid_Price_per_kWh[i+c-1]/1000
                }
            }
            else{
                NCL_hourly_cost[i] <- 0
            }
        }
        NCL_rank <- rank(-NCL_hourly_cost,ties.method = 'first')
        g <- sum(NCL_hourly_cost>0)
        if(g>0)
        {
            for (k in 1:g) {
                Mpos <- match(k,NCL_rank)
                if(prediction_EL[Mpos]<0){
                    next
                }
                if(prediction_data$Vaccum[Mpos]>0){
                    app_load <- prediction_data$Vaccum[Mpos]
                    prediction_data$Vaccum[Mpos] <- 0
                    prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                    prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                    prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                    prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                    prediction_EL <- prediction_load-Solar_Generation[c:24]
                    for(i in 1:q) {
                        if(prediction_data$Vaccum[i]==0){
                            if (prediction_EL[i]>0){
                                slot_price[i]<- app_load*Grid_Price_per_kWh[i+c-1]/1000
                            }
                            else{
                                if(app_load+prediction_EL[i]>0){
                                    slot_price[i] <- (app_load+prediction_EL[i])*Grid_Price_per_kWh[i+c-1]/1000
                                }
                                else{
                                    slot_price[i] <- 0
                                }
                            }
                        }
                        else{
                            slot_price[i]<- Inf
                        }
                    }
                    m <- which.min(slot_price)
                    prediction_data$Vaccum[m]<- app_load
                    prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                    prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                    prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                    prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                    prediction_EL <- prediction_load-Solar_Generation[c:24]
                }
                if(prediction_EL[Mpos]<=0){
                    next
                }
                if(prediction_data$cloth_washer[Mpos]>0){
                    app_load <- prediction_data$cloth_washer[Mpos]
                    prediction_data$cloth_washer[Mpos] <- 0
                    prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                    prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                    prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                    prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                    prediction_EL <- prediction_load-Solar_Generation[c:24]
                    for(i in 1:q) {
                        if(prediction_data$cloth_washer[i]==0){
                            if (prediction_EL[i]>0){
                                slot_price[i]<- app_load*Grid_Price_per_kWh[i+c-1]/1000
                            }
                            else{
                                if(app_load+prediction_EL[i]>0){
                                    slot_price[i] <- (app_load+prediction_EL[i])*Grid_Price_per_kWh[i+c-1]/1000
                                }
                                else{
                                    slot_price[i] <- 0
                                }
                            }
                        }
                        else{
                            slot_price[i]<- Inf
                        }
                    }
                    m <- which.min(slot_price)
                    prediction_data$cloth_washer[m]<- app_load
                    prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                    prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                    prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                    prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                    prediction_EL <- prediction_load-Solar_Generation[c:24]
                }
                if(prediction_EL[Mpos]<0){
                    next
                }
                if(prediction_data$Mixer[Mpos]>0){
                    app_load <- prediction_data$Mixer[Mpos]
                    prediction_data$Mixer[Mpos] <- 0
                    prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                    prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                    prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                    prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                    prediction_EL <- prediction_load-Solar_Generation[c:24]
                    for(i in 1:q) {
                        if(prediction_data$Mixer[i]==0){
                            if (prediction_EL[i]>0){
                                slot_price[i]<- app_load*Grid_Price_per_kWh[i+c-1]/1000
                            }
                            else{
                                if(app_load+prediction_EL[i]>0){
                                    slot_price[i] <- (app_load+prediction_EL[i])*Grid_Price_per_kWh[i+c-1]/1000
                                }
                                else{
                                    slot_price[i] <- 0
                                }
                            }
                        }
                        else{
                            slot_price[i]<- Inf
                        }
                    }
                    m <- which.min(slot_price)
                    prediction_data$Mixer[m]<- app_load
                    prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                    prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                    prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                    prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                    prediction_EL <- prediction_load-Solar_Generation[c:24]
                }
            }
        }
        
        #write_xlsx(prediction_data, path = "G:/7thSemester/BTP/DataSheets/shift.xlsx", col_names = TRUE)
        #plot(prediction_data$`Start Time`,prediction_load, type = 's',ylim = c(0,1200),ylab = 'Watt',xlab = 'Time', main = 'NCL Re-scheduled Load Profile(6AM to 6AM)', col = 'orange')
        #lines(prediction_data$`Start Time`,prediction_CL,type ='s',lty = 2, col = 'green')
        #lines(prediction_data$`Start Time`,prediction_ML,type ='s',lty = 5, col = 'blue')
        #lines(prediction_data$`Start Time`,prediction_NCL,type ='s',lty = 3, col = 'red')
        #legend(xpd = TRUE,'topright',legend = c('CL','ML','NCL','Total'),col = c('green','blue','red','orange'),lty = c(2,5,3,1),cex = 0.6,horiz = TRUE)
        ML_hourly_cost <- c()
        for (i in 1:q) {
            if (prediction_EL[i]>0){
                if(prediction_ML[i]<=prediction_EL[i]){
                    ML_hourly_cost[i] <- prediction_ML[i]*Grid_Price_per_kWh[i+c-1]/1000
                }
                else{
                    ML_hourly_cost[i] <- prediction_EL[i]*Grid_Price_per_kWh[i+c-1]/1000
                }
            }
            else{
                ML_hourly_cost[i] <- 0
            }
        }
        ML_rank <- rank(-ML_hourly_cost,ties.method = 'first')
        h <- sum(ML_hourly_cost>0)
        if(h>0){
            check <- 1 #as.integer(readline(prompt = 'Load optimization is still possible through ML scheduling. Press 1 to approve'))
            if (check == 1){
                for (k in 1:h) {
                    Mpos <- match(k,ML_rank)
                    if(prediction_EL[Mpos]<0){
                        next
                    }
                    if(prediction_data$Oven[Mpos]>0){
                        app_load <- prediction_data$Oven[Mpos]
                        prediction_data$Oven[Mpos] <- 0
                        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                        prediction_EL <- prediction_load-Solar_Generation[c:24]
                        for(i in 1:q) {
                            if(prediction_data$Oven[i]==0){
                                if (prediction_EL[i]>0){
                                    slot_price[i]<- app_load*Grid_Price_per_kWh[i+c-1]/1000
                                }
                                else{
                                    if(app_load+prediction_EL[i]>0){
                                        slot_price[i] <- (app_load+prediction_EL[i])*Grid_Price_per_kWh[i+c-1]/1000
                                    }
                                    else{
                                        slot_price[i] <- 0
                                    }
                                }
                            }
                            else{
                                slot_price[i]<- Inf
                            }
                        }
                        m <- which.min(slot_price)
                        prediction_data$Oven[m]<- app_load
                        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                        prediction_EL <- prediction_load-Solar_Generation[c:24]
                    }
                    if(prediction_EL[Mpos]<0){
                        next
                    }
                    if(prediction_data$TV[Mpos]>0){
                        app_load <- prediction_data$TV[Mpos]
                        prediction_data$TV[Mpos] <- 0
                        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                        prediction_EL <- prediction_load-Solar_Generation[c:24]
                        for(i in 1:q) {
                            if(prediction_data$TV[i]==0){
                                if (prediction_EL[i]>0){
                                    slot_price[i]<- app_load*Grid_Price_per_kWh[i+c-1]/1000
                                }
                                else{
                                    if(app_load+prediction_EL[i]>0){
                                        slot_price[i] <- (app_load+prediction_EL[i])*Grid_Price_per_kWh[i+c-1]/1000
                                    }
                                    else{
                                        slot_price[i] <- 0
                                    }
                                }
                            }
                            else{
                                slot_price[i]<- Inf
                            }
                        }
                        m <- which.min(slot_price)
                        prediction_data$TV[m]<- app_load
                        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                        prediction_EL <- prediction_load-Solar_Generation[c:24]
                    }
                    if(prediction_EL[Mpos]<0){
                        next
                    }
                    if(prediction_data$AC[Mpos]>0){
                        app_load <- prediction_data$AC[Mpos]
                        prediction_data$AC[Mpos] <- 0
                        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                        prediction_EL <- prediction_load-Solar_Generation[c:24]
                        for(i in 1:q) {
                            if(prediction_data$AC[i]==0){
                                if (prediction_EL[i]>0){
                                    slot_price[i]<- app_load*Grid_Price_per_kWh[i+c-1]/1000
                                }
                                else{
                                    if(app_load+prediction_EL[i]>0){
                                        slot_price[i] <- (app_load+prediction_EL[i])*Grid_Price_per_kWh[i+c-1]/1000
                                    }
                                    else{
                                        slot_price[i] <- 0
                                    }
                                }
                            }
                            else{
                                slot_price[i]<- Inf
                            }
                        }
                        m <- which.min(slot_price)
                        prediction_data$AC[m]<- app_load
                        prediction_CL <- prediction_data$Fan+prediction_data$Light+prediction_data$Refrigirator
                        prediction_ML <- prediction_data$AC+prediction_data$TV+prediction_data$Oven
                        prediction_NCL <- prediction_data$Mixer+prediction_data$cloth_washer+prediction_data$Vaccum
                        prediction_load <- prediction_NCL+prediction_ML+prediction_CL
                        prediction_EL <- prediction_load-Solar_Generation[c:24]
                    }
                }
            }
        }
        #-------------Status Table----------#
        status_table_temp <- 1*(prediction_data[1:q,1:9] > 0)
        status_table_temp[1:q,1:3] <- matrix(0,q,3)
        status_table_temp<- as.data.frame(status_table_temp)
        return(as.data.frame(status_table_temp))
    }
    
    status_table <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,1)
    #--------------------------------end reschedule------------------------
    
    
    #write_xlsx(status_table, path = "C:/Users/G.Haritha/Desktop/BTP/status.xlsx", col_names = TRUE)
    
    t<-reactiveTimer(intervalMs = 30000)
    p<-0
    observe(
        {
            t()
            p <<- p+1
            output$load <- renderDataTable({ status_table[p,]})
            #w<-"current slot %d",p
            output$text <- renderText({sprintf("Current Slot is %d",p)})
            
        }
    )
    
    #output$load <- renderDataTable({ status_table})
    
    observeEvent(input$fan, 
                 {
                     if(status_table[p,1]==0)
                     {
                         status_table[p,1]<<- 1
                     }
                     else
                     { 
                         status_table[p,1]<<- 0
                     }
                     output$load <- renderDataTable({status_table[p,]})
                 }
    )
    observeEvent(input$light, 
                 {
                     if(status_table[p,2]==0)
                     {
                         status_table[p,2]<<- 1
                     }
                     else
                     { 
                         status_table[p,2]<<- 0
                     }
                     output$load <- renderDataTable({status_table[p,]})
                 }
    )
    observeEvent(input$refridgrator, 
                 {
                     if(status_table[p,3]==0)
                     {
                         status_table[p,3]<<- 1
                     }
                     else
                     { 
                         status_table[p,3]<<- 0
                     }
                     output$load <- renderDataTable({status_table[p,]})
                 }
    )
    observeEvent(input$tv, 
                 {
                     if(status_table[p,5]==1)
                     {
                         shinyalert(title="Present Request - ON to OFF!",text = 'Do you wish to use this at a later time?',type = "warning",showConfirmButton=TRUE,callbackR=mycallbacktvoff)
                     }
                     if(status_table[p,5]==0)
                     {
                         for(i in p:24)
                         {
                             if(status_table[i,5]==1)
                             {
                                 k <- i
                                 shinyalert(title="Present Request - OFF to ON! ",text = sprintf('Load is scheduled at %d Would you like to use at that time also?', k),type = "warning",showConfirmButton=TRUE,showCancelButton=TRUE,callbackR=mycallbacktvon)
                                 break
                             }
                             else
                             {
                                 shinyalert(title="Present Request - OFF to ON! ",type = "warning",showConfirmButton=TRUE,callbackR=mycallbacktvon)
                             }
                         }
                     }
                 }
    )
    mycallbacktvoff<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,5]<<-0
        }
        #else
        #{
        #  status_table[p,5]<<-0
        #}
        output$load <- renderDataTable({status_table[p,]})
    }
    mycallbacktvon<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,5]<<-1
            output$load <- renderDataTable({status_table[p,]})
            
        }
        else
        {
            status_table[p,5]<<-1
            status_table[k,5]<<-0
            status_table_temp <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,p+1)
            status_table[(p+1):24 , 1:9] <<- status_table_temp
            output$load <- renderDataTable({status_table[p,]})
        }
    }
    observeEvent(input$ac, 
                 {
                     if(status_table[p,4]==1)
                     {
                         shinyalert(title="Present Request - ON to OFF!",text = 'Do you wish to use this at a later time?',type = "warning",showConfirmButton=TRUE,callbackR=mycallbackacoff)
                     }
                     if(status_table[p,4]==0)
                     {
                         for(i in p:24)
                         {
                             if(status_table[i,4]==1)
                             {
                                 k <- i
                                 shinyalert(title="Present Request - OFF to ON! ",text = 'Load is scheduled at k! Would you like to use at that time also?',type = "warning",showConfirmButton=TRUE,showCancelButton=TRUE,callbackR=mycallbackacon)
                                 break
                             }
                             else
                             {
                                 shinyalert(title="Present Request - OFF to ON! ",type = "warning",showConfirmButton=TRUE,callbackR=mycallbackacon)
                             }
                         }
                     }
                 }
    )
    mycallbackacoff<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,4]<<-0
        }
        #else
        #{
        #  status_table[p,5]<<-0
        #}
        output$load <- renderDataTable({status_table[p,]})
    }
    mycallbackacon<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,4]<<-1
            output$load <- renderDataTable({status_table[p,]})
            
        }
        else
        {
            status_table[p,4]<<-1
            status_table[k,4]<<-0
            status_table_temp <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,p+1)
            status_table[(p+1):24 , 1:9] <<- status_table_temp
            output$load <- renderDataTable({status_table[p,]})
        }
    }
    observeEvent(input$oven, 
                 {
                     if(status_table[p,6]==1)
                     {
                         shinyalert(title="Present Request - ON to OFF!",text = 'Do you wish to use this at a later time?',type = "warning",showConfirmButton=TRUE,callbackR=mycallbackovenoff)
                     }
                     if(status_table[p,6]==0)
                     {
                         for(i in p:24)
                         {
                             if(status_table[i,6]==1)
                             {
                                 k <- i
                                 shinyalert(title="Present Request - OFF to ON! ",text = 'Load is scheduled at k! Would you like to use at that time also?',type = "warning",showConfirmButton=TRUE,showCancelButton=TRUE,callbackR=mycallbackovenon)
                                 break
                             }
                             else
                             {
                                 shinyalert(title="Present Request - OFF to ON! ",type = "warning",showConfirmButton=TRUE,callbackR=mycallbackovenon)
                             }
                         }
                     }
                 }
    )
    mycallbackovenoff<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,6]<<-0
        }
        #else
        #{
        #  status_table[p,5]<<-0
        #}
        output$load <- renderDataTable({status_table[p,]})
    }
    mycallbackovenon<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,6]<<-1
            output$load <- renderDataTable({status_table[p,]})
            
        }
        else
        {
            status_table[p,6]<<-1
            status_table[k,6]<<-0
            status_table_temp <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,p+1)
            status_table[(p+1):24 , 1:9] <<- status_table_temp
            output$load <- renderDataTable({status_table[p,]})
        }
    }
    observeEvent(input$mixer, 
                 {
                     if(status_table[p,7]==1)
                     {
                         shinyalert(title="Present Request - ON to OFF!",text = 'Do you wish to use this at a later time?',type = "warning",showConfirmButton=TRUE,callbackR=mycallbackmixeroff)
                     }
                     if(status_table[p,7]==0)
                     {
                         for(i in p:24)
                         {
                             if(status_table[i,7]==1)
                             {
                                 k <- i
                                 shinyalert(title="Present Request - OFF to ON! ",text = 'Load is scheduled at k! Would you like to use at that time also?',type = "warning",showConfirmButton=TRUE,showCancelButton=TRUE,callbackR=mycallbackmixeron)
                                 break
                             }
                             else
                             {
                                 shinyalert(title="Present Request - OFF to ON! ",type = "warning",showConfirmButton=TRUE,callbackR=mycallbackmixeron)
                             }
                         }
                     }
                 }
    )
    mycallbackmixeroff<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,7]<<-0
        }
        #else
        #{
        #  status_table[p,5]<<-0
        #}
        output$load <- renderDataTable({status_table[p,]})
    }
    mycallbackmixeron<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,7]<<-1
            output$load <- renderDataTable({status_table[p,]})
            
        }
        else
        {
            status_table[p,7]<<-1
            status_table[k,7]<<-0
            status_table_temp <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,p+1)
            status_table[(p+1):24 , 1:9] <<- status_table_temp
            output$load <- renderDataTable({status_table[p,]})
        }
    }
    observeEvent(input$washingmachine, 
                 {
                     if(status_table[p,8]==1)
                     {
                         shinyalert(title="Present Request - ON to OFF!",text = 'Do you wish to use this at a later time?',type = "warning",showConfirmButton=TRUE,callbackR=mycallbackwmoff)
                     }
                     if(status_table[p,8]==0)
                     {
                         for(i in p:24)
                         {
                             if(status_table[i,8]==1)
                             {
                                 k <- i
                                 shinyalert(title="Present Request - OFF to ON! ",text = 'Load is scheduled at k! Would you like to use at that time also?',type = "warning",showConfirmButton=TRUE,showCancelButton=TRUE,callbackR=mycallbackwmon)
                                 break
                             }
                             else
                             {
                                 shinyalert(title="Present Request - OFF to ON! ",type = "warning",showConfirmButton=TRUE,callbackR=mycallbackwmon)
                             }
                         }
                     }
                 }
    )
    mycallbackwmoff<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,8]<<-0
        }
        #else
        #{
        #  status_table[p,5]<<-0
        #}
        output$load <- renderDataTable({status_table[p,]})
    }
    mycallbackwmon<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,8]<<-1
            output$load <- renderDataTable({status_table[p,]})
            
        }
        else
        {
            status_table[p,8]<<-1
            status_table[k,8]<<-0
            status_table_temp <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,p+1)
            status_table[(p+1):24 , 1:9] <<- status_table_temp
            output$load <- renderDataTable({status_table[p,]})
        }
    }
    observeEvent(input$cleaner, 
                 {
                     if(status_table[p,9]==1)
                     {
                         shinyalert(title="Present Request - ON to OFF!",text = 'Do you wish to use this at a later time?',type = "warning",showConfirmButton=TRUE,callbackR=mycallbackcleaneroff)
                     }
                     if(status_table[p,9]==0)
                     {
                         for(i in p:24)
                         {
                             if(status_table[i,9]==1)
                             {
                                 k <- i
                                 shinyalert(title="Present Request - OFF to ON! ",text = 'Load is scheduled at k! Would you like to use at that time also?',type = "warning",showConfirmButton=TRUE,showCancelButton=TRUE,callbackR=mycallbackcleaneron)
                                 break
                             }
                             else
                             {
                                 shinyalert(title="Present Request - OFF to ON! ",type = "warning",showConfirmButton=TRUE,callbackR=mycallbackcleaneron)
                             }
                         }
                     }
                 }
    )
    mycallbackcleaneroff<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,9]<<-0
        }
        #else
        #{
        #  status_table[p,5]<<-0
        #}
        output$load <- renderDataTable({status_table[p,]})
    }
    mycallbackcleaneron<-function(decision)
    {
        if(decision==TRUE)
        {
            status_table[p,9]<<-1
            output$load <- renderDataTable({status_table[p,]})
            
        }
        else
        {
            status_table[p,9]<<-1
            status_table[k,9]<<-0
            status_table_temp <- reschedule(status_table,cl_table,prediction_data$Solar_Generation,prediction_data$Grid_Price_per_kWh,p+1)
            status_table[(p+1):24 , 1:9] <<- status_table_temp
            output$load <- renderDataTable({status_table[p,]})
        }
    }
    observeEvent(input$data, 
                 { 
                     output$load <- renderDataTable({status_table})
                 }
    )
    
}


)
