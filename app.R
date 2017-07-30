
library(shiny)

ui <- fluidPage(
  titlePanel("Which P-Values Can You Expect?"),
  sidebarLayout(
    sidebarPanel(
      numericInput("nSims", "Number of simulation: ", 100000, min = 1, max = 100000),
      br(),
      numericInput("M", "Mean: ", 106),
      numericInput("n", "N: ", 31),
      numericInput("SD", "SD: ", 15),
      br(),
      numericInput("bars", "Number of bars in histogram: ", 20),
      sliderInput("xlim", "Range of x-axis", min = 0, max = 1,
                  value = c(0, 1)),
      sliderInput("ylim", "Range of y-axis", min = 0, max = 100000,
                  value = c(0, 100000), sep=""),
      br(),
      p("The idea and code for this simulation are by Daniel Lakens, and are part of his Coursera course",
        a("Improving your statistical inferences",
          href = "https://www.coursera.org/learn/statistical-inferences/",
          target = "_blank")
      ),
      br()
    ),
    mainPanel(plotOutput("pplot"))
    )
)


server <- function(input, output) {
  output$pplot <- renderPlot({
    
    #Load pwr package to easily calculate the statistical power
    if(!require(pwr)){install.packages('pwr')}
    library(pwr)
    #Disable scientific notation (1.05e10)
    options(scipen=999)
    #Set number of simulations
    nSims <- input$nSims #number of simulated experiments
    
    M<-input$M #Mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
    n<-input$n #set sample size
    SD<-input$SD #SD of the simulated data
    #With a mean difference of 6, and SD of 15, and a sample size of 26, the test has 50% power)
    
    p <-numeric(nSims) #set up empty variable to store all simulated p-values
    bars<-input$bars
    #Run simulation
    for(i in 1:nSims){ #for each simulated experiment
      x<-rnorm(n = n, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
      z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
      p[i]<-z$p.value #get the p-value and store it
    }
    
    #Check power by summing significant p-values and dividing by number of simulations
    (sum(p < 0.05)/nSims) #power
    #Calculate power formally by power analysis
    power<-pwr.t.test(d=(M-100)/SD, n=n,sig.level=0.05,type="one.sample",alternative="two.sided")$power #determines M when power > 0. When power = 0, will set  M = 100.
    
    #Plot figure
    #png(file="P-valueDist.png",width=4000,height=3000, , units = "px", res = 500)
    op <- par(mar = c(5,7,4,4)) #change white-space around graph
    hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
         main=paste("P-value Distribution with",round(power*100, digits=1),"% Power"),
         col="grey", xlim=c(input$xlim[1],input$xlim[2]),  ylim=c(input$ylim[1], input$ylim[2]))
    axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
    axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
    abline(h=nSims/bars, col = "red", lty=3)
    #dev.off()
    
    #© Daniel Lakens, 2016. 
    # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  })
}


shinyApp(ui = ui, server = server)
