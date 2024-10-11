library(shiny)
library(tidyverse)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

ui <- fluidPage(

    # Application title
    titlePanel("Coursera Week 1 Assignment"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("value",
                        "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                        min = 1,
                        max = 5,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(dat %>%
             filter(ideo5 == input$value),
           mapping = aes(x = pid7)) +
      geom_bar() +
      labs(
        x = "7 Point Party ID, 1 = Very D, 7 = Very R",
        y = "Count"
      ) +
      coord_cartesian(ylim = c(0, 125), xlim = c(0, 8))
  },
  res = 96)
}


# Run the application 
shinyApp(ui = ui, server = server)
