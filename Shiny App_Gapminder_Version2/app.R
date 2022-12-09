library(shiny) #load the shiny package
library(tidyverse) #load the tidyverse package
library(shinythemes) #load the package for theme

gapminder<-read_csv("GapminderApp_data.csv") #load the data package, for this project, using gapminder data set

#Create and manage the user interface
ui <- fluidPage(
  theme = shinytheme("flatly"), 
  #title
  titlePanel("Gapminder World Application"), 
  #header
  h4("An Application that Explores World with You"), 
  
  #Navbar allows me to make my shiny into a website and create different pages for the app, and this function allows me to indicate the contents of the Navigation Bar. 
  #Note: this navigation menu is new for Assignment B4.
  navbarPage(
    #Non-interactive title of the navigation bar
    "Let's explore world data!", 
    
    #Create an "About" page which gives basic information and background about this application to the user
    #Note: this "About" page is new for Assignment B4
    tabPanel("About",
      sidebarLayout(
      sidebarPanel(
        #Create an image at the sidebar panel on the "About" page, separated from the main description text 
        width = 5, 
        imageOutput("world_image", width = "100%", height = "250px"), 
        h5("Data of different continents and countries always worth exploration")),
      mainPanel(
        #Create a main block of the homepage introducing what this app is, and how it works
        width=7, 
        h3("What's Gapminder App?"), 
        p("Welcome to my Gapminder World App repository! This app is created for everyone to explore the data regarding life expectancy, GDP per capita, and populations of different countries and continents. The data used in this app is collected from the gapminder data set, which provides data about the population, life expectancy and GDP per capita in different countries of the world from 1952 to 2007. It's always very interesting to explore the development of different countries and continents around the world. This app allows users to explore the data of different continents and different time range. Also, users can select which variables they are interested in. The variables selected will be graphed into a scatter plot. The app also allows users to download the tables and plots in case they would like to save the tables and plots."), 
        br(), 
        p("The Data page contains the entire Gapminder data set, and you can choose the specific continents and time range you are interested in. The Plot page allows you to graph out your own scatter plot by choosing the specific continent and two factors (of GDP per capita, population, and life expectancy) you are interested in.")
      )
    )), 
      
    #Create a navigation page called "Data"
    tabPanel("Data",
       sidebarLayout(
        sidebarPanel(
         #Create an output so the users can easily select a continent they want to explore on the table
         #Too many countries and too many continents are there in the gapminder data set, so this control widgets allow users to choose specifically which continent they want to explore 
          uiOutput("continentOutput1"), 
          #New Feature 1: allows users to explore the data set by year
          #Reason: allows users to narrow down the data set to the specific year they want to explore, so users can focus on the data they are interested in
          #sliderInput allows the users to select a range of time they would like to explore (any time range within the data set's range of 1952-2007)
          sliderInput("yearInput", "Year", 1952, 2007,
                      value = c(1970,1990))
        ), 
        
        mainPanel(
              #Create a placeholder for the table, and link it to the table output
              tableOutput("id_table"),
              br(), 
              #Create a download button for users to download the table being displayed
              downloadButton("downloadTable", "Download Table"))
        )), 
    
    #Create a navigation page called "Plot"
    tabPanel("Plot", 
            sidebarLayout(
              sidebarPanel(
                h4("You can explore the relationship between any two variables you are interested!"), 
                #create an output so the users can easily select the continent they want to explore on the plot
                uiOutput("continentOutput2"), 
                #New Feature 2: create buttons that allow users to choose whichever variables they are interested in
                #Reason: the gapminder dataset has several variables, so letting users choose their own variables-of-interest with this new feature 
                #create a button that let users select the variable on the x-axis for the plot, set the default option being the gdpPercap variable
                radioButtons("Criterion_1", "select the first variable", 
                             choices = c("lifeExp", "pop", "gdpPercap"), 
                             selected = "gdpPercap"), 
                #create a button that let users select the variable on the y-axis for the plot, set the default option being the lifeExp variable 
                radioButtons("Criterion_2", "select the second variable", 
                             choices = c("lifeExp", "pop", "gdpPercap"), 
                             selected = "lifeExp")
              ), 
              
            mainPanel(
              #Creating a placeholder for the scatter plots, and linking it to the scatter plot output
              #New Feature 3: the plot is now interactive when users click on data points
              #Reason: this is a scatter plot with many data points, so this feature allows users to click on the plot and explore the values of the data points
              plotOutput("scatter", click = "plot_click"),
              br(), 
              em("Please click on the data points to explore the specific values"),
              br(), 
              #Creating an information box underneath the plot, so when users click the data points, values will be shown in the information box 
              verbatimTextOutput("info"), 
              #Creating a download button for users to download the plot being displayed
              downloadButton("downloadPlot", "Download Plot"))
           )
        )
   )
 )

  
server <- function(input, output) {
#About 
  
  #Making an image output for the About page
  output$world_image <- renderImage({
    list(src = "Image/world_map.png", width = "100%", height = 250)
  }, deleteFile = F)

#Data 
  
  #Making the input box within the "Data" navigation page dynamic
  output$continentOutput1 <- renderUI({
    selectInput("continentInput1", "Continent", 
                #Sort the options in the input box
                sort(unique(gapminder$continent)), 
                #Set the default option to be the "Americas" continent
                selected = "Americas")
  })
  
  #Creating a reactive filter that allows users to select continent and year (time range) within the "Data" navigation page
  filtered <- reactive({
    #Inside the reactive function, this is for checking if the continent input exists, and if not, then just return NULL 
    if(is.null(input$continentInput1)){
      return(NULL)
    }
      gapminder %>%
        filter(year >= input$yearInput[1], 
               year <= input$yearInput[2], 
               continent %in% input$continentInput1)
  })
  
  
  #Creating a table showing data
  #This table allows users to see the the complete data. 
  #Note: the users can explore the data of each continent separately and the data of different years, because the input box created in the sidebar panel allows them to choose the continents and the time range.  
  output$id_table <- renderTable({
    filtered()
  })

#Plot 
  
  #Making the input box within the "Plot" navigation page dynamic
  output$continentOutput2 <- renderUI({
    selectInput("continentInput2", "Continent", 
                #Sort the options in the input box
                sort(unique(gapminder$continent)), 
                #Set the default option to be the "Americas" continent
                selected = "Americas")
  })
  
  #Creating a reactive filter that allows users to select the specific continent they want to explore within the "Plot" navigation page
  filtered_2 <- reactive({
    #Inside the reactive function, this is for checking if the continent input exists, and if not, then just return NULL. 
    if(is.null(input$continentInput2)){
      return(NULL)
    }
    gapminder %>%
      filter(continent %in% input$continentInput2)
  })
  
  #Creating the output for the first variable to be selected by users
  output$criterion_1_text <- renderText({
    input$Criterion_1
  })
  
  #Creating the output for the second variable to be selected by users
  output$criterion_2_text <- renderText({
    input$Criterion_2
  })
  
  #Creating a reactive plot input for a scatter plot. Making this reactive because the exact plot codes will be used again later when creating download handlers for plots. 
  plotInput <- reactive ({
    #This scatter plot can demonstrate the relationship between any two variables users chosen for criterion 1 and criterion 2. 
    #Note: users can explore the scatter plot of each continent separately, because the input box created in the sidebar panel allows them to choose the continents 
    ggplot(filtered_2(), aes(x=.data[[input$Criterion_1]], y=.data[[input$Criterion_2]])) +
      geom_point(alpha = 0.5)
  })
    
  #Using the reactive plot input for this output of plots 
  output$scatter <- renderPlot({ 
    #Making a check with the renderPlot() function, because ggplot function will not work with a NULL dataset
    if(is.null(filtered_2())){
      return()
    }
    plotInput() 
  })
  
  #Creating an output for the information box underneath the plot 
  #Reason: This information box gives details of each data point when users click on the data points. 
  output$info <- renderPrint({
    #if users don't click on plot, no information will be shown.
    if(is.null(input$plot_click)) return()
    #nearPoints() only gives data information if users are clicking near the data points
    #threshold: set max distance to be 7, in pixels
    #maxpoints: maximum number of rows to return is 1
    #xvar and yvar: set the x-variable and y-variable to be the ones users select from the side bar panel 
    nearPoints(filtered_2(), input$plot_click, xvar=input$Criterion_1, yvar=input$Criterion_2, threshold = 7, maxpoints = 1)
  })
  
  #Creating a download handler for plot download 
  #This is for users who like the plots and would like to download the plots for future use. 
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste(input$dataset, "plot.png", sep=" ") 
    }, 
    content = function (file){
      ggsave(file, plot=plotInput(), device = "png")
    }
  )
  
  #Creating a download handler for table download 
  #This is for users who like the tables and would like to download the tables for future use. 
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
