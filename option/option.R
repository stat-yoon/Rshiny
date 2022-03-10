library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(dashboardthemes)

header <- dashboardHeader(title = "Option Price", titleWidth = 280)


sidebar <- dashboardSidebar(
    # load data
    fileInput('file', label=h3('Upload Stock Price'),
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    textInput("name", h3("Stock Name"), value = "SAMSUNG"),
    
    dateInput('date', label=h3('Choose starting date'), value = "2020-12-29"),
    
    radioButtons("option", h3("Option"), 
                 choices = list("European Call" = "ecall", "European Put" = "eput",
                                "Asian Call" = "acall", "Asian Put" = "aput")),
    tags$hr(),
    actionButton("find", "Search data", icon = icon("database"), onclick ="window.open('https://finance.yahoo.com/')"),
    actionButton("help", "Help", width = 80, icon = icon("info-circle")),
    width = 280
)


body <- dashboardBody(
    
    fluidRow(valueBoxOutput("price", width = 6),
			 valueBoxOutput("mu", width = 3),
			 valueBoxOutput("sigma", width = 3)),
    
    fluidRow(
        box(title = "Input", 
            numericInput("r", h4("Riskless interest rate"),value=0.05), 
            # numericInput("sigma", h3("Volatility"), value=0.5), 
            numericInput("k", h4("Excercise Price"), value=60000),
            sliderInput("t", h4("Expiration(year)"), min=0, max=5, value=1, step=1), 
			      sliderInput("t2", h4("Expiration(month)"), min=0, max=11, value=0, step=1),
			      actionButton("enter", "Enter"),
			solidHeader = T, status = "primary", collapsible = TRUE, width = 6, height = 550),
        
        fluidRow(
            box(title = 'Time-Series plot', status = 'primary',width = 6, 
                solidHeader = T,  collapsible = TRUE, height = 550, plotlyOutput("Plot"),
                HTML("<br> <br> <br>  ** The dotted line is the predicted value, the grey dotted line is 95% prediction interval."))
        ))
)



ui <- dashboardPage(header, sidebar, body, shinyDashboardThemes(
  theme = "grey_dark"))

## function
volatility <- function(st){
	u <- diff(log(st))
	dt <- 1/250
	u.bar <- mean(u)
	s <- sd(u)
	sigma <- s/sqrt(dt)
	mu <- (u.bar + s^2/2)/dt
	return(list(mu=mu, sigma=sigma))}
	

GB <- function(date0, s0, mu, sigma){
  t <- (1:60)/250
  st <- s0*exp((mu-sigma^2/2)*t)
  lower <- s0*exp((mu-sigma^2/2)*t-1.96*sigma*sqrt(t))
  upper <- s0*exp((mu-sigma^2/2)*t+1.96*sigma*sqrt(t))
  
  df <- data.frame(date=date0+t*250, St=st, lower=lower, upper=upper)
  return(df)
}

asian <- function(s0, K, t, r, sigma){
    M <- 10000
    delta.t <- 1/250
    large.t <- round(250*t)
    ci <- c()
    pi <- c()
    z <- matrix(rnorm(M*large.t), ncol=M)
	z <- apply(z, 2, cumsum) 
    
	s0 <- matrix(s0, nrow=large.t, ncol=M)
	j <- matrix(1:large.t, ncol=M, nrow=large.t)
	st <- s0*exp((r-sigma^2/2)*delta.t*j + sigma*sqrt(delta.t)*z)
    st.bar <- colMeans(st)
	
    c.list <- exp(-r*t)*sapply(st.bar-K, max, 0)
    p.list <- exp(-r*t)*sapply(K-st.bar, max, 0)
    
    ct <- mean(c.list)
    pt <- mean(p.list)
    
    return(list(acall=ct, aput=pt))
}

euro <- function(s0, K, t, r, sigma){
	d1 <- (log(s0/K) + (r+sigma^2/2)*t)/(sigma*sqrt(t))
	d2 <- d1 - sigma*sqrt(t)
	
	ct <- exp(-r*t)*(s0*exp(r*t)*pnorm(d1)-K*pnorm(d2))
	pt <- exp(-r*t)*(-s0*exp(r*t)*pnorm(-d1)+K*pnorm(-d2))
	
	return(list(ecall=ct, eput=pt))
}
## server

server <- function(input, output, session){
    observeEvent(input$help,{
      showModal(modalDialog(
        title = "Help message",
        HTML("In the stock price data you enter, the first column should be the date and the second column should be the daily closing price. <br> And the option expiration input is divided into year and month. For example, if the option expires in 6 months, enter year=0, month=6."),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    })
  
    data <- reactive({
        infile <- input$file
        
        if(is.null(infile)){
            return(NULL)
        }
        df <- read.csv(infile$datapath, col.names = c('date', 'St'))
        df$date <- as.Date(df$date)
        df <- df %>% arrange(date)
        return(df)
    })
    
    output$Plot <- renderPlotly({
        data <- data()
        validate(need(data, "Please upload a data set"))
        data <- data[(nrow(data)-250):nrow(data),]
        estimate <- volatility(data$St)
        gb <- GB(rev(data$date)[1], rev(data$St)[1], estimate$mu, estimate$sigma)
        data2 <- rbind(data, gb[,1:2])
        data2["type"] <- as.factor(rep(1:2, c(nrow(data), nrow(gb))))
        ran <- range(data2$date)  
        gg_plot <- ggplot(data2)+geom_line(aes(x=date, y=St, lty=type, group=1, text=paste("date:",date,"\nstock price:",format(round(St), big.mark=",")))) + ylab("stock price") + theme(legend.position = "none", plot.title = element_text(size=20), plot.subtitle = element_text(size=15)) + geom_line(data=gb,aes(x=date, y=lower), col="grey", lty=2) + geom_line(data=gb,aes(x=date, y=upper), col="grey", lty=2) + scale_y_continuous(limits = c(min(data2$St)*0.9, max(data2$St)*1.1))
        ggplotly(gg_plot, tooltip = "text") %>% layout(height=450, margin = list(t=70, b=70), title=list(text=paste0(input$name,"<br>","<sup>", paste("Date: ", ran[1],"~",ran[2]),"</sup>")))
    })
    
    observeEvent(input$enter, {
		data <- data()
		validate(need(data, "Please upload a data set"))
		s0 <- data[data$date==input$date,]$St
		estimate <- volatility(data$St)
		sigma <- estimate$sigma; mu <- estimate$mu
		T <- input$t + input$t2/12
		
		asian_cal <- asian(s0, input$k, T, input$r, sigma)
		euro_cal <- euro(s0, input$k, T, input$r, sigma)
		all <- append(asian_cal, euro_cal)
		
		options <- list("ecall" = "European Call Option", "eput" = "European Put Option",
		                "acall" = "Asian Call Option", "aput" = "Asian Put Option")
		
		output$price <- renderValueBox({
		  valueBox({format(round(all[[input$option]]), big.mark=",")},
		            paste("Price of ", options[[input$option]]), icon = icon("won-sign"), color = "yellow")})

		output$mu <- renderValueBox({
			valueBox(round(mu,3), "Mean of daily return", color="navy")})
			
		output$sigma <- renderValueBox({
			valueBox(round(sigma,3), "Volatility of daily return", color="navy")})
})}





shinyApp(ui=ui, server = server)
