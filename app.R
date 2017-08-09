library(shiny)

ui <- shinyUI(
  fluidPage(
  titlePanel("Which P-Values Can You Expect?"),
  
  plotOutput("pplot", height=600),
  
  hr(),
  
  fluidRow(
    column(3,
           #hr("Simulation Inputs"),
           numericInput("nSims", "Number of simulation: ", 1000, min = 1, max = 100000),
           numericInput("M", "Mean: ", 106)
         
           ),
    column(3,
           numericInput("n", "N: ", 26),
           numericInput("SD", "SD: ", 15),
           actionButton("go", "Draw the plot") 
           ),
    column(3, 
          # hr("Plot Parameters"),
           sliderInput("xlim", "Range of x-axis", min = 0, max = 1, step = .05,
                       value = c(0, 1)),
           sliderInput("ylim", "Range of y-axis", min = 0, max = 100, step = 5,
                       value = c(0, 100))

    ),
    column(3,
           numericInput("bars", "Number of bars in histogram: ", 20),
           p("The idea and code for this simulation are by Daniel Lakens, and are part of his Coursera course",
             a("Improving your statistical inferences",
               href = "https://www.coursera.org/learn/statistical-inferences/",
               target = "_blank") )
  )
  )
))
 

server <- shinyServer(
  function(input, output, session) {
  #Load pwr package to easily calculate the statistical power
  if(!require(pwr)){install.packages('pwr')}
  library(pwr)
  #Disable scientific notation (1.05e10)
  options(scipen=999)
  
  dataInput <- eventReactive(input$go, {
    nSims <-input$nSims #number of simulated experiments
    #Set number of simulations
    
    M<-input$M #Mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
    n<-input$n #set sample size
    SD<-input$SD #SD of the simulated data
    #With a mean difference of 6, and SD of 15, and a sample size of 26, the test has 50% power)
    
    p <-numeric(nSims) #set up empty variable to store all simulated p-values
    
    #Run simulation
    for(i in 1:nSims){ #for each simulated experiment
      x<-rnorm(n = n, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
      z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
      p[i]<-z$p.value #get the p-value and store it
    }
    p 
  }, ignoreNULL = FALSE )
  
 nSimsInput <- eventReactive(input$go, {
    nSims <- input$nSims #number of simulated experiments
    nSims
  }, ignoreNULL = FALSE )
 
 pwrInput <- eventReactive(input$go, {
   M<-input$M #Mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
   n<-input$n #set sample size
   SD<-input$SD #SD of the simulated data
   power<-pwr.t.test(d=(M-100)/SD, n=n,sig.level=0.05,type="one.sample",alternative="two.sided")$power #determines M when power > 0. When power = 0, will set  M = 100.
 }, ignoreNULL = FALSE )
 
 output$pplot <- renderPlot({
    
    bars<-input$bars
    nSims <-nSimsInput() #number of simulated experiments
    #Set number of simulations
    
    M<-input$M #Mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
    n<-input$n #set sample size
    SD<-input$SD #SD of the simulated data
    
    #Calculate power formally by power analysis
    d <- dataInput()
    
    # set steps for axis labels
    ystep <- ifelse((input$ylim[2]-input$ylim[1])>49, 25,
                    ifelse((input$ylim[2]-input$ylim[1])>15, 10, 1)
    )
    xstep <- ifelse((input$xlim[2]-input$xlim[1])>.3, .1,
                    ifelse((input$xlim[2]-input$xlim[1])>.15, .05, .01)
    )
    
    #Plot figure
    #png(file="P-valueDist.png",width=4000,height=3000, , units = "px", res = 500)
   # op <- par(mar = c(5,7,4,4)) #change white-space around graph
    h <- hist(d, breaks=bars)
    h$density <- h$counts/sum(h$counts)*100
    plot(h,freq=FALSE,
         main=paste("P-value Distribution with",round(pwrInput()*100, digits=1),"% Power"),
         xlab="P-values", ylab="percent of p-values\n",
         xlim=c(input$xlim[1],input$xlim[2]),  ylim=c(input$ylim[1], input$ylim[2]),
         axes=FALSE)
    axis(side=1, at=seq(0,1, xstep), labels=seq(0,1,xstep))
    axis(side=2, at=seq(0,100, ystep), labels=seq(0,100, ystep), las=2)
    abline(h=100/bars, col = "red", lty=3)
    #dev.off()
    
    #© Daniel Lakens, 2016. 
    # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  })
})

shinyApp(ui = ui, server = server)