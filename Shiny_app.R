library(shiny)
library(tidyverse)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel = (
      sliderInput(inputId ="Ideo5", 
                  label="Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                  min=1,
                  max=5,
                  value=3
                  )
    ),
    mainPanel(width=12,
      plotOutput("Ideo5_Plot")
    )
  )
)
  

server<-function(input,output){
  output$Ideo5_Plot<-renderPlot({
    filtered_data <- dat %>% filter(ideo5 == input$Ideo5)
    ggplot(filtered_data, aes(x = pid7)) +
      geom_bar() +
      scale_x_continuous(limits = c(0, 8))+
      scale_y_continuous(breaks = seq(0, 125, by = 25), limits = c(0, 125))+
      labs(x = "7 point party ID, 1=Very D, 7=Very R")
    
  })
}

shinyApp(ui,server)
