library(shiny) #load the shiny package
library(tidyverse) #load the tidyverse package

gapminder<-read_csv("GapminderApp_data.csv") #load the data package, for this project, using gapminder data set
 
#Create and manage the user interface
ui <- fluidPage(
  #title 
  titlePanel("Gapminder World App"),
  #header - A brief description about what this app is about
  h4("This app explores the data of life expectancy, GDP per capita, and population of different countries and continents."),
  sidebarLayout(
    sidebarPanel(
      #Feature 1: setting a tab in the sidebar panel to allow users to easily select a continent
      #Reason: Too many countries and too many continents are there in the gapminder data set, so this control widgets allow users to choose specifically which continent they want to explore. 
      uiOutput("CONTINENT")
    ),
  
    mainPanel(
      #Create two tabs for the users, one for plots, one for tables, so users can see the tables and plots separately
      tabsetPanel(
        type = "tabs", 
        
        tabPanel("Table", 
                 #Creating a placeholder for the table, and linking it to the table output
                 tableOutput("id_table"),
                 #Creating a download button for users to download the table being displayed
                 downloadButton("downloadTable", "Download Table")), 
        
        tabPanel("Plot", 
                 #Creating a placeholder for the scatter plots, and linking it to scatter plot output
                 plotOutput("scatter"), 
                 #Creating a download button for users to download the plot being displayed
                 downloadButton("downloadPlot", "Download Plot"))
      )
    )
  )
)

  
server <- function(input, output) {
  #making the input box(Feature 1) dynamic
  output$CONTINENT <- renderUI({
    selectInput("CONTINENT", "Continent", 
                #Sort the options in the input box
                sort(unique(gapminder$continent)), 
                #Set the default option to be the "Americas" continent
                selected = "Americas")
  })
  
  #Creating a reactive filter as its own variable for whichever continent users select from the continent tab in the sidebar panel. 
  filtered <- reactive({
    #Inside the reactive function, this is for checking if the continent input exists, and if not, then just return NULL. 
    if(is.null(input$CONTINENT)){
      return(NULL)
    }
    gapminder %>% 
      filter(continent == input$CONTINENT)
  })
  
  #Creating a reactive plot input for a scatter plot. Making this reactive because the exact plot codes will be used again later when creating download handlers for plots. 
  plotInput <- reactive ({
    #Feature 2: a scatter plot for GDP per capita VS life expectancy.  
    #Reason: this scatter plot allows users to explore the relationship between GDP per capita and the life expectancy to see if GDP per capita can be associated with life expectancy. In other words, it allows users to see if higher population income is related to better health and lower mortality of the population.
    #Note: users can explore the scatter plot of each continent separately, because the input box created in the sidebar panel allows them to choose the continents. 
    ggplot(filtered(), aes(x=gdpPercap, y=lifeExp)) +
      geom_point(alpha=0.5)
  })
  
    
  #Using the reactive plot input for this output of plots 
  output$scatter <- renderPlot({ 
    #Make a check with the renderPlot() function, because ggplot function will not work with a NULL dataset
    if(is.null(filtered())){
      return()
    }
    plotInput()
  })
  
  #Feature 3: a table showing data
  #Reason: allows users to see the the complete data. 
  #Note: the users can explore the data of each continent separately, because the input box created in the sidebar panel allows them to choose the continents.  
  output$id_table <- renderTable({
    filtered()
  })
  
  #Feature 4: creating a download handler for plot download 
  #Reason: this is for users who like the plots and would like to download the plots for future use. 
  output$downloadPlot <- downloadHandler(
    filename = function (){
      paste(input$dataset, "plot.png", sep=" ") 
    }, 
    content = function (file){
      ggsave(file, plot=plotInput(), device = "png")
    }
  )
  
  #Feature 4: creating a download handler for table download 
  #Reason: this is for users who like the tables and would like to download the tables for future use. 
  output$downloadTable <- downloadHandler(
    filename = function(){
      paste(input$dataset, "table.csv", sep=" ") 
    }, 
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
