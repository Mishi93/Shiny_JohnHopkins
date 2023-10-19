library(shiny)
library(tidyverse)
library(plotly)
library(DT)

##### Import Data
dat <- read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat <- dat %>% select(c("pid7", "ideo5", "newsint", "gender", "educ", "CC18_308a", "region"))
dat <- drop_na(dat)

##### Make your app
ui <- navbarPage("My Application",
                 tabPanel("Page 1",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(inputId = "ideo5",
                                          label = "Select Five Point Ideology (1= Very liberal, 5= Very Conservative)",
                                          min = 1,
                                          max = 5,
                                          value = 3
                              )
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Tab 1", plotOutput("Plot1")),
                                tabPanel("Tab 2", plotOutput("Plot2"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Page 2",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(
                                inputId = "gender",
                                label = "Select Gender",
                                choices = c("1", "2")                              )
                            ),
                            
                            mainPanel(
                              plotlyOutput("Plot3")
                            )
                          )
                 ),
                 
                 tabPanel("Page 3",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = "dropdown",
                                label = "Select Region",
                                choices = c("1", "2", "3", "4"),
                                multiple = TRUE
                              )
                            ),
                            mainPanel( 
                              DTOutput(outputId = "Table",
                                       width= "500",
                                       height="500")
                            )
                          )
                 ),
)

server <- function(input, output) {
  ##### Hint: when you make the data table on page 3, you may need to adjust the height argument in the dataTableOutput function. Try a value of height=500
  output$Plot1 <- renderPlot({
    filtered_data <- dat %>% filter(ideo5 == input$ideo5)
    ggplot(data = filtered_data, aes(x = pid7)) + geom_bar() + labs(x = "7 Point Party ID, 1= Very D, 7= Very R") + scale_x_continuous(limits = c(0, 8)) + scale_y_continuous(limits = c(0, 100))
  })
  output$Plot2 <- renderPlot({
    filtered_data <- dat %>% filter(ideo5 == input$ideo5)
    ggplot(data = filtered_data, aes(x = CC18_308a)) + geom_bar() + labs(x = "Trump Support") 
  })
  output$Plot3 <- renderPlotly({
    selected_gender <- input$gender
    if ("1" %in% selected_gender && "2" %in% selected_gender) {
      filtered_data <- dat
    } else if ("1" %in% selected_gender) {
      filtered_data <- dat %>% filter(gender == 1)
    } else if ("2" %in% selected_gender) {
      filtered_data <- dat %>% filter(gender == 2)
    } else {
      # Default behavior when neither 1 nor 2 is selected
      return(NULL)
    }
    
    g <- ggplot(data = filtered_data, aes(x = educ, y = pid7)) +
      geom_point() + geom_jitter() + geom_smooth(method = "lm")
    ggplotly(g)
  })
  
  output$Table <- renderDT({
    selected_region <- input$dropdown
    region <- dat %>%
      filter(region %in% selected_region)
    datatable(region, options = list(paging = TRUE))
  })
}

shinyApp(ui = ui, server = server)
