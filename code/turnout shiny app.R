# Shiny App that uses both the turnout and political sentiment models and allows
# and allows users to select a sample of voters from any municipality in NC and
# meet turnout and sentiment critera

#### load the shiny and data.table packages
library(shiny)
library(data.table)

#### Import the dataframe (initially from the CSV prodused from Python modeling and prediction, next from a native R dataframe)

#election <- read.csv("prob_df_enhanced.csv", header = TRUE)
#save(election, file = "election.RData")
load(file = "election.RData") #data is now stored in a native R dataframe, which loads much more quickly than a CSV


#rename the columns for display
data.table::setnames(election, old = c('first_name', 'last_name', 'percent_republican', 'percent_participation' , 'party_cd'), 
         new = c('First_Name','Last_Name', 'Conservative_Sentiment', 'Likelihood_of_Voting', 'Registered_Party'))



# Create a list of counties and put Mecklenburg in 'county'
counties <- levels(election$county_desc) # all the possible values of county
county <- "MECKLENBURG" # start with Mecklenburg County.  Charlotte will be the first municipality since it appears first in muni.vector

munis = subset(election, county_desc == county, select = c(municipality_abbrv)) # Get all the municipalities in Mecklenburg
munis <- unique(munis) # Only keep the unique values
munis <- subset(munis, municipality_abbrv != 'None') # Remove 'None'
munis$municipality_abbrv <- factor(munis$municipality_abbrv) # Reduce the factors to only Mecklenburg municipalites
muni.factor <- munis[["municipality_abbrv"]] # Convert column to factor variable
muni.vector <- as.vector(levels(muni.factor)) # Convert factor variable to vector



# Define a UI for the Shiny App
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Sample Voters by Political Sentiment and Likelihood of Voting in 2017 Local Elections"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with four inputs and three buttons
    sidebarPanel(
      selectInput("county", "County:", 
                  choices=counties, selected="MECKLENBURG"), 
      selectInput("muni", "Municipality:", 
                  choices=muni.vector),
      sliderInput("prob_rep", "Conservative Sentiment:",
                  min=0, max=1, value=c(.1,.9)),
      sliderInput("prob_vote", "Likelihood of Voting:",
                  min=0, max=1, value=c(.1, .9)),
      actionButton("CUV", "Conservative Unlikely Voters"),
      actionButton("LUV", "Liberal Unlikely Voters"),
      actionButton("PLV", "Persuadable Likely Voters"),
      helpText("Select the county and municipality of interest.  Then select the range of conservative sentiment (with 1 being very conservative and 0 being not conservative).  Lastly, select the range of likelihood of voting in the 2017 local election (with 1 being very likely and 0 being very unlikely).  On the right, you will see a random selection of 15 voters meeting these criteria")
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
    munis <- subset(munis, municipality_abbrv != 'None') # Remove 'None'
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
               "Conservative Sentiment",
               "Likelihood of Voting"),
      Value = as.character(c(input$county, 
                             input$muni,
                              paste(input$prob_rep, collapse=' thru '),
                              paste(input$prob_vote, collapse = ' thru '))),
                             
      stringsAsFactors=FALSE)
    
    
  })
  
  #Create a "reactive" data frame containing all the voters meeting the criteria from the values selected in the left sidebar.  "reactive" means that the table will change automatically
  #if any values in the sidebar change.  No button pressing required.
  nameValues <- reactive({
    
    # First, make sure that at least 15 voters meet the criteria.  If not, return a null.
    if(nrow(subset(election, 
           county_desc == input$county & municipality_abbrv == input$muni & Conservative_Sentiment > input$prob_rep[1] & Conservative_Sentiment < input$prob_rep[2] 
           & Likelihood_of_Voting > input$prob_vote[1] & Likelihood_of_Voting < input$prob_vote[2], 
           select = c(First_Name,Last_Name, Conservative_Sentiment, Likelihood_of_Voting, Registered_Party))) < 15) return()
    
    # If there are at least 15 voters meeting the criteria, return the full set of voters
    subset(election, 
            county_desc == input$county & municipality_abbrv == input$muni & Conservative_Sentiment > input$prob_rep[1] & Conservative_Sentiment < input$prob_rep[2]
            & Likelihood_of_Voting > input$prob_vote[1] & Likelihood_of_Voting < input$prob_vote[2], 
            select = c(First_Name, Last_Name, Conservative_Sentiment, Likelihood_of_Voting, Registered_Party))
       
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
  
  # If "Conservative Unlikely Voters" button is pushed, update "prob_rep" and "prob_vote" sliders accordingly
  observeEvent(input$CUV, {
    updateSliderInput(session, "prob_rep", value = c(.6,1.0))
    updateSliderInput(session, "prob_vote", value = c(0,0.5))
  })
  
  # If "Liberal Unlikely Voters" button is pushed, update "prob_rep" and "prob_vote" sliders accordingly
  observeEvent(input$LUV, {
    updateSliderInput(session, "prob_rep", value = c(0,.4))
    updateSliderInput(session, "prob_vote", value = c(0,0.5))
  })

  # If "Persuadable Likely Voters" button is pushed, update "prob_rep" and "prob_vote" sliders accordingly  
  observeEvent(input$PLV, {
    updateSliderInput(session, "prob_rep", value = c(0.4,0.6))
    updateSliderInput(session, "prob_vote", value = c(0.5,1))
  })
  
  
}
  

#Lauch the Shiny App with the ui and server we just created
shinyApp(ui=ui, server=server)

 

                