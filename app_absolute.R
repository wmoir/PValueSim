library(shiny)

ui <- shinyUI(
  fluidPage(
  titlePanel("Which P-Values Can You Expect?"),
  sidebarLayout(
    sidebarPanel(
      numericInput("nSims", "Number of simulation: ", 1000, min = 1, max = 100000),
      numericInput("M", "Mean: ", 106),
      numericInput("n", "N: ", 26),
      numericInput("SD", "SD: ", 15),
      actionButton("go", "Draw the plot"),
      numericInput("bars", "Number of bars in histogram: ", 20),
      sliderInput("xlim", "Range of x-axis", min = 0, max = 1,
                  value = c(0, 1)),
      uiOutput("ylim"),
      p("The idea and code for this simulation are by Daniel Lakens, and are part of his Coursera course",
        a("Improving your statistical inferences",
          href = "https://www.coursera.org/learn/statistical-inferences/",
          target = "_blank")
      )
    ),
    mainPanel(plotOutput("pplot"))
  )
))


server <- shinyServer(
  function(input, output, session) {
  #Load pwr package to easily calculate the statistical power
  if(!require(pwr)){install.packages('pwr')}
  library(pwr)
  #Disable scientific notation (1.05e10)
  options(scipen=999)
  
  output$ylim <- renderUI({
    sliderInput("ylim", "Range of y-axis", min = 0, max = input$nSims,
                value = c(0, input$nSims), sep="")
  })
  
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
    
    #Plot figure
    #png(file="P-valueDist.png",width=4000,height=3000, , units = "px", res = 500)
    op <- par(mar = c(5,7,4,4)) #change white-space around graph
    hist(d, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
         main=paste("P-value Distribution with",round(pwrInput()*100, digits=1),"% Power"),
         col="grey", xlim=c(input$xlim[1],input$xlim[2]),  ylim=c(input$ylim[1], input$ylim[2]))
    axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
    axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
    abline(h=nSims/bars, col = "red", lty=3)
    #dev.off()
    
    #© Daniel Lakens, 2016. 
    # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  })
})

shinyApp(ui = ui, server = server)