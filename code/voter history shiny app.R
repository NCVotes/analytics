# Shiny App that uses actual turnout and demoraphics to allow 
# users to select a sample of voters from any municipality in NC that
# meet turnout and demographic criteria

#### load the shiny and data.table packages
options(java.parameters = "-Xss2048k") #To address a Shiny server problem
library(shiny)
library(data.table)

#### Import the dataframe (initially from the CSV prodused from Python modeling and prediction, next from a native R dataframe)
#election <- read.csv("demo_df.csv", header = TRUE)

#change 1s and 0s to "Y" and "N" for 'year' columns.  Use vectorized 'ifelse' function
# election$X2008 <- ifelse(election$X2008 == 1, 'Y', 'N')  
# election$X2012 <- ifelse(election$X2012 == 1, 'Y', 'N')
# election$X2016 <- ifelse(election$X2016 == 1, 'Y', 'N') 
# save(election, file = "election.RData")


load(file = "election.RData") #data is now stored in a native R dataframe, which loads much more quickly than a CSV


#rename the columns for display
data.table::setnames(election, old = c('first_name', 'last_name', 'party_cd'), 
         new = c('First_Name','Last_Name', 'Registered_Party'))




# Create a list of counties and put Mecklenburg in 'county'
counties <- levels(election$county_desc) # all the possible values of county should be listed
county <- "MECKLENBURG" # start with Mecklenburg County.  Charlotte will be the first municipality since it appears first in muni.vector
munis = subset(election, county_desc == county, select = c(municipality_abbrv)) # Get all the municipalities in Mecklenburg
munis <- unique(munis) # Only keep the unique values
munis$municipality_abbrv <- factor(munis$municipality_abbrv) # Reduce the factors to only Mecklenburg municipalites
muni.factor <- munis[["municipality_abbrv"]] # Convert column to factor variable
muni.vector <- as.vector(levels(muni.factor)) # Convert factor variable to vector

# Create a dictionary for race
race_dict <- list(
  "UNDESIGNATED" = "U",  
  "OTHER" = "O" ,     
  "BLACK or AFRICAN AMERICAN" = "B" ,                                
  "AMERICAN INDIAN or ALASKA NATIVE"  = "I" ,                          
  "WHITE" = "W" ,                                                       
  "ASIAN" = "A",                                                      
  "TWO or MORE RACES" = "M"                                          
)
races <- names(race_dict) # all the possible values of race
#races = unlist(races, recursive = TRUE, use.names = TRUE)

# Create a dictionary for ethnicity
ethnicity_dict <- list(
  "UNDESIGNATED" = "U" ,  
  "HISPANIC/LATINO" = "HL" ,                                
  "NOT HISPANIC/LATINO"  = "NL"                       
)
enthnicities <- names(ethnicity_dict) # all the possible values of ethnicity  

# Create a dictionary for gender
gender_dict <- list(
  "UNDESIGNATED" = "U",
  "MALE" = "M" ,                                
  "FEMALE"  = "F"                           
)
genders <- names(gender_dict) # all the possible values of gender



# Define a UI for the Shiny App
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Sample Voters by Voting History in Last Three Presidential Elections"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with four inputs and three buttons
    sidebarPanel(
      selectInput("county", "County:", 
                  choices=counties, selected="MECKLENBURG"), 
      selectInput("muni", "Municipality:", 
                  choices=muni.vector),
      selectInput("race", "Race:", selected="BLACK or AFRICAN AMERICAN",
                  choices=races),
      selectInput("gender", "Gender:", selected="FEMALE",
                  choices=genders),
      selectInput("ethnicity", "Ethnicity:", selected="NOT HISPANIC/LATINO",
                  choices=enthnicities),
      radioButtons("X2008", "Voted in 2008 General Election: ", choices =  c("Yes" = "Y",
                                                                          "No" = "N")),
      radioButtons("X2012", "Voted in 2012 General Election: ", choices =  c("Yes" = "Y",
                                                                            "No" = "N")),
      radioButtons("X2016", "Voted in 2016 General Election: ", choices =  c("Yes" = "Y",
                                                                            "No" = "N"), selected = "N"),
      helpText("Choose the county and municipality of interest.  Then choose the
               demographic characteristics of voters desired.  Lastly, select the election 
               history for the last three elections.  A random sample of 15 voters meeting these critera will be displayed")
    ),
    
    mainPanel(
      tableOutput("values"), 
      tableOutput("names")
    )
  )
)
    




# Define a server for the Shiny app
server <- function(input, output, session) {
  
  
  #Observe county and update the left sidebar accordingly with the correct municpalities for that county
  observe({
    county <- input$county

    # Find all the municipalities for the county
    munis = subset(election, county_desc == county, select = c(municipality_abbrv)) # Get all the municipalities in the selected county
    munis <- unique(munis) # Only keep the unique values
    munis$municipality_abbrv <- factor(munis$municipality_abbrv) # Reduce the factors to only selected county municipalites
    muni.factor <- munis[["municipality_abbrv"]] # Convert column to factor variable
    muni.vector <- as.vector(levels(muni.factor)) # Convert factor variable to vector

    # update the valid municipality values
    updateSelectInput(session, "muni", label = "Municipality:",
                      choices=muni.vector)

  })
  

  #Create a "reactive" dataframe containing the values selected in the left sidebar.  "reactive" means that the table will change automatically
  #if any values in the sidebar change.  No button pressing required.
  tableValues <- reactive({
    
    # Compose the data frame summarizing the selected values.  This will be on the top right of the screen.
    data.frame(
      Name = c("County", 
               "Municipality",
               "Race",
               "Ethnicity",
               "Gender",
               "Voted in 2008",
               "Voted in 2012",
               "Voted in 2016"
               ),
      Value = as.character(c(input$county, 
                             input$muni,
                              input$race,
                             input$ethnicity,
                             input$gender,
                             input$X2008,
                             input$X2012,
                             input$X2016
                             )),
                             
      stringsAsFactors=FALSE)
    
    
  })
  
  #Create a "reactive" data frame containing all the voters meeting the criteria from the values selected in the left sidebar.  "reactive" means that the table will change automatically
  #if any values in the sidebar change.  No button pressing required.
  nameValues <- reactive({
    
    # First, make sure that at least 15 voters meet the criteria.  If not, return a null.
    if(nrow(subset(election, 
           county_desc == input$county & municipality_abbrv == input$muni & race_code == race_dict[[input$race]] & ethnic_code == ethnicity_dict[[input$ethnicity]] 
           & gender_code == gender_dict[[input$gender]]  & X2008 == input$X2008 & X2012 == input$X2012 & X2016 == input$X2016,
           select = c(First_Name,Last_Name, Registered_Party))) < 15) return()
    
    # If there are at least 15 voters meeting the criteria, return the full set of voters
    subset(election,
           county_desc == input$county & municipality_abbrv == input$muni & race_code == race_dict[[input$race]] & ethnic_code == ethnicity_dict[[input$ethnicity]]
          & gender_code == gender_dict[[input$gender]] & X2008 == input$X2008 & X2012 == input$X2012 & X2016 == input$X2016,
           select = c(First_Name,Last_Name, Registered_Party))
       
  })

  # First render the summary datframe on the top right
  output$values <- renderTable({
    tableValues()
  })
  
  # Next render a 15 voter sample of the voter dataframe on bottom right.  We first validate that at least 15 voters are in the dataframe
  # and return an error message if not
  output$names <- renderTable({
    validate(
      need(nameValues(), 'No registered voters meet these criteria')
    )
    nameValues()[sample(nrow(nameValues()), 15), ]
    
  })
  
 
  
  
}
  

#Lauch the Shiny App with the ui and server we just created
shinyApp(ui=ui, server=server)

 

                