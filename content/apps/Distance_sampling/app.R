#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Distance)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Distance sampling"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "file1",
                      label = "Choose csv file",
                      accept = ".csv"),
            
            checkboxInput('header', 'Header', TRUE),
            
            actionButton("go", "Create plots (will take a couple of seconds)"),
            
            width = 3,
            
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("summary"),
            plotOutput("HalfNormalPlot"),
            plotOutput("Uniform"),
            plotOutput("HazardRate"),
            width = 9,
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    transect_data <- eventReactive(input$go, { # defining reactive object called transect data
        
        file <- input$file1 # connectnig with InputFile from UI 
        
        ext <- tools::file_ext(file$datapath) # setting extension of uploaded file
        
        req(file) # requesting that a file is indeed uploaded
        
        validate(need(ext == "csv", "Please upload a csv file")) # validating extension
        
        read.csv(file$datapath, header = input$header) # reading the csv file
        
    })
    
    output$HalfNormalPlot <- renderPlot({
        
        df1 <- transect_data()
        
        plot(ds(df1, transect = "line"),
             main = "Half - normal detection function")
        
    })
    
    output$Uniform <- renderPlot({
        
        df1 <- transect_data()
        
        plot(ds(df1, transect = "line", key = "unif"),
             main = "Uniform detection function")
        
    })
    
    output$HazardRate <- renderPlot({
        
        df1 <- transect_data()
        
        plot(ds(df1, transect = "line", key = "hr"),
             main = "Hazard rate detection function")
        
    })
    
    output$summary <- renderTable({
        df3 <- transect_data()
        
        hn <- summary(ds(df3, transect = "line")) #half normal
        un <- summary(ds(df3, transect = "line", key = "unif")) # uniform
        hr <- summary(ds(df3, transect = "line", key = "hr"))
        
        summary_table <- data.frame(Function = c("Half - normal", "Uniform", "Hazard rate"),
                                    Estimated_p = c(hn$ds$average.p, un$ds$average.p, hr$ds$average.p),
                                    StandardError_p = c(hn$ds$average.p.se, un$ds$average.p.se, hr$ds$average.p.se),
                                    AIC = c(hn$ds$aic, un$ds$aic, hr$ds$aic))
        
        names(summary_table) <- c("Detection function", "Estimated probability of detection (p)", 
                                  "Standard error of p", "Akaike Information Criterion - AIC")
        
        summary_table
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)