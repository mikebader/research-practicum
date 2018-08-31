#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(TeachingDemos)
library(rsconnect)

generate_population <- function(N,mu=0,sd=1) {
    return(rnorm(N,mu,sd))
}

generate_sample <- function(population,n) {
    return(sample(population,n))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel(""),

   # Sidebar with a slider input for number of bins
   fluidRow(
       column(6,fluidRow(
           column(12,h1("Sampling Distributions")),
           column(6,
                  p("This is a simulation that draws samples of size N from a population of 1,000,000
                     and shows the distribution of those samples."),
                  p("Select a mean and standard deviation of the population, then the size of the
                     samples that you want to draw from that population."),
                  p("The example is set equal to the population of verbal GRE scores from 2013 to
                     2016", br(), a(href="https://www.ets.org/s/gre/pdf/gre_guide_table1a.pdf","source: ETS"))
                  ),
           column(6,wellPanel(
                 numericInput("mu","Population mean", 149.97,-1000,1000,.1),
                 numericInput("sigma","Population standard deviation",8.49,-250,250,.01),
                 hr(),
                 numericInput("sample_size", "Sample Size", 1,
                              1, 1000,1),
                 actionButton("go","Go!"),
                 actionButton("stop","Stop!"),
                 actionButton("reset","Reset"),

                 renderTable("current_samples")
                 ), #End wellPanel
                 textOutput("samples_n"),
                 textOutput("samples_mu"),
                 textOutput("samples_sd")
           ) # end column6
        ) # end fluidRow
   ),
        column(6,
         plotOutput("samplePlot"),
         plotOutput("populationPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    samples <- reactiveValues(means=c(),continue=FALSE,pop=c(),sample_size=NULL)

    autoInvalidate <- reactiveTimer(750)
    observeEvent(input$go,{
        samples$continue=TRUE
        if(is.null(samples$pop)){
            samples$pop <- generate_population(1000000,input$mu,input$sigma)
        }
        samples$sample_size <- input$sample_size
    })
    observeEvent(input$stop,{samples$continue=FALSE})
    observeEvent(input$reset,{
        samples$means = c()
    })
    observe({
        autoInvalidate()
        cont <- isolate(samples$continue)
        if(cont==TRUE) {
        new_mean <- sapply(lapply(rep(samples$sample_size,10),generate_sample,population=samples$pop),mean)
        samples$means <- c(isolate(samples$means),new_mean)
        }
    })

    output$samplePlot <- renderPlot({
        pop <- samples$pop
        ggplot(data.frame(samples$means),aes(samples$means)) + geom_histogram(bins=200) +
            labs(
                title=paste(length(samples$means),"samples of size",samples$sample_size),
                subtitle=paste("Sampling standard deviation:",round(sd(samples$means),3)),
                x = "sample mean"
            ) +
            xlim(min(pop),max(pop)) +
            theme(axis.text.y = element_text(angle=90,hjust=.5))
   })

    output$populationPlot <- renderPlot({
       pop <- samples$pop
        qplot(pop,xlim=c(min(pop),max(pop)),bins=250) +
            theme(axis.text.y = element_text(angle=90,hjust=.5))
    })

    output$samples_n <- renderText(paste(length(samples$means),"samples of size",samples$sample_size))
    output$samples_mu <- renderText(paste("Mean of sample means:",round(mean(samples$means),3)))
    output$samples_sd <- renderText(paste("Standard deviation of sample means:",round(sd(samples$means),3)))


}
# Run the application
shinyApp(ui = ui, server = server)
# rsconnect::setAccountInfo(name='mikebader',
#                           token='09FBDD9A941B60C8B70C41B2F3952B6C',
#                           secret='ahmq3J+TGbSiGj/RW/td+eyGW69vPpfDRwZ5TkhS')
# rsconnect::deployApp("app.R")
#
