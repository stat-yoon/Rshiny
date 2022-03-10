library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(dashboardthemes)
options(scipen=999)

header <- dashboardHeader(title = "Insuarance Premium")

sidebar <- dashboardSidebar(
  
  sidebarMenu(id="type",
              menuItem("Data", tabName = "data", icon = icon("file-csv")),
              
              menuItem("Premium", tabName = "result", icon = icon("heartbeat"))
  ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "data", 
            fluidRow(
              box(title="Data Input",
                  fileInput('population', label=h5('Upload Populaion Data'),
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', '.csv')),
                  fileInput('death', label=h5('Upload Death Data'),
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', '.csv')),
                  actionButton("enter", "Enter", icon = icon("check"))
                  
                  ),
              
              box(title="NOTICE",
                  HTML("1.It calculates your life insurance and annuity premiums using the uploaded data.<br> 
                  2.Upload population and death toll data by age in the same year. <br> 
                  3.For both data, the first column should be age, the second column should be male, and the last column should be female.<br> 
                  4.Upload the data and press the Enter button. <br> 
                  5. Enter your insurance information and click the Calculate button.<br> 
                       6. Data can be downloaded from KOSIS, and by entering the icon below, you will be connected to the site."),
                  hr(),
                  actionButton("find", "KOSIS", icon = icon("database"), onclick ="window.open('https://kosis.kr/')")
                  
                  ))
    ),
    
    tabItem(tabName = "result",
            fluidRow(
              valueBoxOutput("b", width = 6),
              valueBoxOutput("px", width = 6)),
            
            fluidRow(
              box(title = "Information",
                  radioButtons("choice", label = h3("Choose"), 
                               choices = list("Life insuarance"="life", "Annuity insuarance"="annual"), selected = "life", inline = TRUE),
                  radioButtons("sex", label = h3("Sex"),
                               choices = list("Male"="Male","Female"="Female"), selected = 'Female', inline = TRUE),
                  sliderInput("x", h3("Age"), min = 0, max = 100, value=30, step=1),
                  column(width=5, numericInput("i", h3("Annual Interst Rate"), value=0.05)),
                  column(width=5, offset=1, numericInput("n", h3("Payment Expiration"), value=35)),
                  column(width=5, numericInput("m", h3("Pay Expiration"), value=35)),
                  column(width=5, offset=1, numericInput("b", h3("Benefit"), value=10000)), 
                  actionButton("cal", "Calculate", icon = icon("check")),
                  width = 6, solidHeader = T, collapsible = TRUE),
              
              tabBox(
                tabPanel("Life Expectancy", plotlyOutput("plot.life")),
                tabPanel("Death rate", plotlyOutput("plot.death")),
                tabPanel("Survivors", plotlyOutput("plot.surv")),
                tabPanel("Stationary Population", plotlyOutput("plot.stat"))
              )
              
              
              )
    )
  ))

ui <- dashboardPage(header, sidebar, body, shinyDashboardThemes(theme = "purple_gradient"))


make_data <- function(pop, dead){
  pop <- pop %>% gather("sex","px",2:3) %>% mutate(sex=as.factor(sex))
  dead <- dead %>% gather("sex","dx",2:3) %>% mutate(sex=as.factor(sex))
  life <- merge(pop, dead, by = c("sex","x")) %>% arrange(sex, x)
  
  life_male <- life %>% filter(sex=="male") %>% mutate(mx = dx/px, qx=mx/(1+mx/2), lx=100000*lag(cumprod(1-qx), default=1), Lx=(lx+lead(lx))/2) 
  life_male <- life_male %>% filter(!is.na(Lx)) %>% arrange(desc(x)) %>% mutate(ex = cumsum(Lx)/lx) %>% arrange(x) 
  life_female <- life %>% filter(sex=="female") %>% mutate(mx = dx/px, qx=mx/(1+mx/2), lx=100000*lag(cumprod(1-qx), default=1), Lx=(lx+lead(lx))/2) 
  life_female <- life_female %>% filter(!is.na(Lx)) %>% arrange(desc(x)) %>% mutate(ex = cumsum(Lx)/lx) %>% arrange(x)
  
  life.table <- merge(life_male[c("x","lx","Lx","qx","ex")], life_female[c("x","lx","Lx","qx","ex")], by="x",suffixes = c(".m",".f"))
  life.table[c("lx.m","Lx.m","lx.f","Lx.f")] <- round(life.table[c("lx.m","Lx.m","lx.f","Lx.f")])
  life.table[c("qx.m","qx.f")] <- round(life.table[c("qx.m","qx.f")],4)
  life.table[c("ex.m","ex.f")] <- round(life.table[c("ex.m","ex.f")],1)
  
  return(life.table)
}


pv.life<-function(data,x,i,sex,n,m,b){
  
  r<-log(1+i)
  if(sex=="Male"){
    lx<-data$lx.m
  } else{
    lx<-data$lx.f
  }
  a <- c()
  for (j in 1:m){
    a[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
    a.sum<-sum(a)
  }
  Ax.bar<-1-r*((1/2)+a.sum+(lx[x+1+m]/lx[x+1])*exp(-r*(m+1))/(1-exp(-r)))
  
  c <- c()
  for (j in 1:n-1){
    c[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
    c.sum<-sum(c)
  }
  ax<-(1/2)+c.sum+(1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
  value<-(Ax.bar/ax)
  
  px<-b*value/12
  
  return(px)
}

pv.annual<-function(data,x,i,sex,n,m,b){
  r<-log(1+i)
  a<-c()
  c<-c()
  if(sex=="Male"){
    lx<-data$lx.m
  } else{
    lx<-data$lx.f
  }
  for (j in (m+1):100){
    a[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
    a.sum<-sum(a,na.rm = T)
    
  }
  Ax.bar<-(1/2)*(lx[x+1+m]/lx[x+1])*(exp(-r*m))+a.sum
  
  for (j in 1:n-1){
    c[j]<-((lx[x+1+j]/lx[x+1])*exp(-r*j))
    c.sum<-sum(c)
  }
  ax<-1/2+c.sum+(1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
  value<-(Ax.bar/ax)
  px<-b*value/12
  
  return(px)
}


life.table <- function(data){
  col <- c("age", "survivors", "stationary_population", "death_rate", "life_expectancy", "sex")
  df_m <- data %>% dplyr::select(x, contains("m")) %>% mutate(sex=rep("male", nrow(data)))
  colnames(df_m) <- col
  
  df_f <- data %>% dplyr::select(x, contains("f")) %>% mutate(sex=rep("female", nrow(data)))
  colnames(df_f) <- col
  
  df <- rbind(df_m, df_f)
  df["sex"] <- as.factor(df$sex)
  return(df)
}

## server.R ##

server <- function(input, output, session){
  
  data <- reactive({
    population <- input$population
    death <- input$death
    
    if(is.null(population) | is.null(death)){
      return(NULL)
    }
    
    pop <- read.csv(population$datapath, col.names = c("x", "male", "female"))
    dead <- read.csv(death$datapath, col.names = c("x", "male", "female"))
    
    data <- make_data(pop, dead)
    print(head(data))
    return(data)
  })
  
  
  observeEvent(input$enter,{
    observeEvent(input$cal, {
      data <- data()
      print(head(data))
      
      pv_life <- pv.life(data, input$x, input$i, input$sex, input$n,input$m,input$b)
      pv_annual <- pv.annual(data, input$x, input$i, input$sex, input$n,input$m,input$b)
      pv <- list(life=pv_life, annual=pv_annual)
      
      lt <- life.table(data)
      # df <- rbind(head(lt), 
      #             data.frame(age=c("..."), e=c("..."), q=c("..."), l=c("..."), L=c("...")), 
      #             tail(lt))
      
      output$b <- renderValueBox({
        valueBox(format(input$b*10000,big.mark = ","), h4("Benefit"), icon = icon("won-sign"), color = "navy")
      })
      
      output$px <- renderValueBox({
        valueBox({
          validate(need(data, "Please upload a data set"))
          format(round(pv[[input$choice]],2)*10000,big.mark = ",")
          
        }, h4("Premium"), icon = icon("won-sign"), color = "yellow")       
      })
      
      output$plot.life <- renderPlotly({
        validate(need(data, "Please upload a data set"))
        plot1 <- ggplot(lt) + geom_line(aes(age, life_expectancy, col=sex))
        ggplotly(plot1)
        })
      output$plot.death <- renderPlotly({
        validate(need(data, "Please upload a data set"))
        plot2 <- ggplot(lt) + geom_line(aes(age, death_rate, col=sex))
        ggplotly(plot2)
      })
      output$plot.surv <- renderPlotly({
        validate(need(data, "Please upload a data set"))
        plot3 <- ggplot(lt) + geom_line(aes(age, survivors, col=sex))
        ggplotly(plot3)
      })
      output$plot.stat <- renderPlotly({
        validate(need(data, "Please upload a data set"))
        plot4 <- ggplot(lt) + geom_line(aes(age, stationary_population, col=sex))
        ggplotly(plot4)
      })

    })
  })


}

shinyApp(ui=ui, server = server)

