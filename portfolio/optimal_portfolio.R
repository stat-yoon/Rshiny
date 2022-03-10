library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stats)
library(kableExtra)
library(dashboardthemes)

header <- dashboardHeader(title = "Optimal Portfolio")

sidebar <- dashboardSidebar(
    
    sidebarMenu(id="type",
                menuItem("Input", tabName = "input", icon = icon("chart-bar"),
                         actionButton("find", "Search data", icon = icon("database"), onclick ="window.open('https://finance.yahoo.com/')"),
                         fileInput('file1', label=h5('Upload Deposit Data'),
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', '.csv')),
                         fileInput('file2', label=h5('Upload Stock Prices Data'),
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', '.csv')),
                         numericInput("V0", h5("Initial Investment Amount"), value=10000000),
                         numericInput("i", h5("Annual Interst Rate"), value=0.05),
                         numericInput("N", h5("Train-set size(Year)"), value=5),
                         numericInput("M", h5("Pay Expiration"), value=5)),
                
                menuItem("Constraints", tabName = "con", icon = icon("lock-open"),
                         sliderInput("dep", h5("Deposit"), min=0, max=1, value =c(0,1)),
                         sliderInput("s1", h5("Stock1"), min=0, max=1, value =c(0,1)),
                         sliderInput("s2", h5("Stock2"), min=0, max=1, value =c(0,1)),
                         sliderInput("s3", h5("Stock3"), min=0, max=1, value =c(0,1)),
                         sliderInput("s4", h5("Stock4"), min=0, max=1, value =c(0,1)),
                         sliderInput("s5", h5("Stock5"), min=0, max=1, value =c(0,1)))
                ),
    actionButton("help", "Help", width = 100,icon = icon("info-circle")),
    width = 250
)

body <- dashboardBody(
    valueBoxOutput("mean", width=4),
    valueBoxOutput("sd", width=4),
    valueBoxOutput("SR", width=4),
    
    fluidRow(
        box(title = "Information", 
            # column(12, align='center', tableOutput('Table')),
             width = 6, height=470, solidHeader = T,
            plotOutput('Plot_theta'),
            "** 0% is not displayed"),
        box(title = "Time-Series Plot", 
            plotlyOutput('Plot_ts'), 
            width = 6, height=470, solidHeader = T)
        ),
    
    fluidRow(
        box(title="Validation", 
            column(12, align='center', tableOutput('Valid')), width=12)
        )
)

ui <- dashboardPage(header, sidebar, body, shinyDashboardThemes(
  theme = "grey_dark"
))


cal.return <- function(x){
  diff(x)/(lag(x)[-1])
}

make.rt <- function(interest, pt){
  interest[,-1] <- (1+interest[,-1]/100)^(1/12)-1
  print(range(interest$t))
  rt <- data.frame(t= pt$t[-1])
  rt <- cbind(rt, apply(pt[,-1], 2, cal.return)) 
  print(tail(rt))
  # print(range(rt$t))
  df <- right_join(interest, rt, by = "t")
  return(df)
}


shape.ratio <- function(theta, r, i){ # input: theta, return, target return ratio
    theta <- c( 1-sum(theta), theta) # theta0 = 1-sum(theta)
    theta <- matrix(theta, nrow=nrow(r), ncol=length(theta), byrow = T)
    rp <- rowSums(r*theta)
    rp.bar <- mean(rp)
    sp <- sd(rp)
    r.star <- (1+i)^(1/12)-1
    
    return((rp.bar - r.star)/sp)
} 

cal.theta <- function(train, i, a0, a1){
    train <- train[,-1]
    j <- ncol(train)-1
    ui <- rbind(rep(-1, j), diag(j))
    ui <- c(t(cbind(ui, -ui)))
    ui <- matrix(ui, ncol=j, byrow = T)
    ci <- c(a0-1, a1) # a0: interest rate interval / a1: others
    ci <- ci*c(1,-1)
    
    # initial value
    ci2 <- matrix(ci+0.1, ncol=1)
    theta0 <- c(MASS::ginv(ui) %*% ci2)
    
    # optim
    res <- constrOptim(theta = theta0, f=shape.ratio, grad=NULL, ui=ui, ci=ci, r=train, i=i, control = list(fnscale = -1))
    
    return(res)
}


outputs <- function(train,test,theta,i,v0){
    
    r.star <- (1+i)^(1/12)-1
    
    date <- test$t 
    
    train <- data.matrix(train[,-1])
    test <- data.matrix(test[,-1])
    n1 <- nrow(train)
    n2 <- nrow(test)
    
    theta <- c(1-sum(theta), theta) # theta0 = 1-sum(theta)
    
    mat1 <- matrix(theta, nrow=n1, ncol=length(theta), byrow = T)
    mat2 <- matrix(theta, nrow=n2, ncol=length(theta), byrow = T)
    
    rpt<-rowSums(train*mat1)
    rpt.star <- rowSums(test*mat2)
    
    rp.bar <-mean(rpt)
    sp <-sd(rpt)
    z<-(rp.bar-r.star)/sp
    
    ex.mean <- 12*rp.bar
    ex.sd <- sqrt(12)*sp
    
    
    real.mean <-mean(rpt.star)*12
    real.sd <-sd(rpt.star)*sqrt(12)
    
    #vt <- v0*cumprod(1+rpt)
    vt.star <- v0*cumprod(1+rpt.star)
    
    result <- list(theta = theta,
                   sharpe = z%>%round(3),
                   month = c(rp.bar,sp)%>%round(3), 
                   expected = c(ex.mean,ex.sd)%>%round(3),
                   realized = c(real.mean,real.sd)%>%round(3),
                   vt = data.frame(t = date , vt.star = vt.star),
                   name = colnames(train))
    
    return(result)
} 


## server.R ##


server <- function(input, output, session){
    observeEvent(input$help,{
      showModal(modalDialog(
        title = "Help message",
        HTML("You can enter up to 5 stocks. The stock price should be the monthly closing price. <br> For both deposit data and stock price data, the first column should be date. <br> And for the stock price data, the first row(column name) should be the stock names"),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    data <- reactive({
        infile1 <- input$file1
        infile2 <- input$file2
        
        if(is.null(infile1) | is.null(infile2)){
            return(NULL)
        }
        
        int <- read.csv(infile1$datapath, col.names = c("t", "Deposit"))
        # colnames(int)[1] <- "t"
        int$t <- as.Date(int$t)
        print(dim(int))
        
        pt <- read.csv(infile2$datapath)
        colnames(pt)[1] <- "t"
        pt$t <- as.Date(pt$t)
        
        df <- make.rt(int, pt)
        print(dim(df))
        print(df[!complete.cases(df),])
        return(df)
    })
    
    out<-reactive({
      
        data  <- data()
        validate(
          need(data, "Please upload a data set"))
        n <- input$N*12
        train <- data[1:n,]
        m <- input$M *12
        test  <- data[(n+1):(n+m),]
        a1 <- c(input$s1,input$s2, input$s3,
                input$s4, input$s5)
        theta<-cal.theta(train, input$i, a0=input$dep, a1=a1)$par
        
        result<- outputs(train,test,theta,input$i,input$V0)
        return(result)
    })
    
    
    output$mean <- renderValueBox({
        valueBox({
            out <- out()
            paste0(out$month[1]*100,"%")}, "Average Monthly Return",color = "olive")       
    })
    output$sd <- renderValueBox({
        valueBox({
            out <- out()
            paste0(out$month[2]*100, "%")}, "SD of Monthly Return", color = "olive")       
    })
    output$SR <- renderValueBox({
        valueBox({
            out <- out()
            out$sharpe}, "Sharpe Ratio",color = "yellow")       
    })
    
    # output$Table <- renderTable({
    #     out <- out()
    #     data <- data()
    #     df_theta <- matrix(out$theta, nrow=1)
    #     colnames(df_theta) <- out$name
    #     print(df_theta)
    # })
    
    output$Plot_theta <- renderPlot({
        out <- out()
        data <- data()
        df_theta <- data.frame(theta=out$theta, stock=out$name)
        pie_plot <- df_theta %>% filter(theta>10^(-5)) %>% ggplot(aes("", y=theta, fill=stock)) + geom_bar(stat='identity') + coord_polar(theta='y') + geom_text(aes(label = paste0(stock, "\n", round(theta*100), "%")), 
                                                                                                                                    position = position_stack(vjust = 0.5))+labs(x='', y='') + scale_fill_brewer(palette = 'Pastel1')+ggtitle("Optimal Portfolio Ratio") + theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text.x=element_blank(), panel.border = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5))
        print(pie_plot)
        
        
        if(!is.null(out)){
            
        }
    })
    
    output$Valid <- function(){
        out <- out()
        df <- data.frame(cbind(out$expected, out$realized))
        colnames(df) <- c('Mean', 'SD')
        rownames(df) <- c('expected', 'real')
        df %>% knitr::kable('html') %>% kable_styling('striped', full_width=T, font_size=20, position='center') %>% row_spec(row = 1:2,
                                                                                                                              color = "white")
                                                                                                                              # background = "black")
    }

    output$Plot_ts <- renderPlotly({
        out <- out()
        if(!is.null(out)){
            t_min <- min(out$vt$t)
            t_max <- max(out$vt$t)
            # plot <- plot_ly(out$vt, x=t, y=vt.star, type = 'scatter', mode = 'lines', color="black", ) 
            # plot
            gg_plot <- out$vt %>% ggplot()+geom_line(aes(x=t,y=vt.star, group=1, text=paste("date:",t,"\nvalue:",format(round(vt.star),big.mark=","))))+ labs(x='date', y='Vt') + theme(plot.title = element_text(size=20))
            ggplotly(gg_plot, tooltip = "text")%>% layout(margin = list(t=70, b=70), title=list(text=paste0("Portfolio value over time in test-set","<br>","<sup>", paste("Date: ", t_min,"~",t_max),"</sup>")))
            # print(ggplotly(gg_plot))
        }
    })
    
    
    
}

shinyApp(ui=ui, server = server)