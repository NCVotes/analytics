#### Create the Shiny App
library(shiny)

#### Import the coeffiencts so that we can use the model

coeff <- read.csv("Data/full_coefficients.csv", header = TRUE)

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
races = unlist(races, recursive = TRUE, use.names = TRUE)

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

# Create a dictionary for drivers license
license_dict <- list(
  "UNDESIGNATED" = "U",
  "YES" = "Y" ,                                
  "NO"  = "N"                           
)
licenses <- names(license_dict) # all the possible values of drivers license

# Create a list of all state codes and add "None"
states <- state.abb
states <-append(states,"None") # all the possible values of state adding "None" to the end

# Create a list of all zip codes in North Carolina
library(zipcode)
zipcodes <- subset(coeff, name == "zip_code")$index
#zipcodes <- sort(zipcodes)

# Create a list of years since regisration
registration_years <- seq.int(as.numeric(substr(Sys.Date(), 0, 4)), as.numeric(substr(Sys.Date(), 0, 4))-90)

# Create a list of ages
birth_ages <- seq.int(18, 100)

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Prediction of Political Sentiment in North Carolina"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("race", "Race:", 
                  choices=races),
      selectInput("gender", "Gender:", 
                  choices=genders),
      selectInput("ethnicity", "Ethnicity:", 
                  choices=enthnicities),
      selectInput("drivers_lic", "Drivers License:", 
                  choices=licenses),
      selectInput("birth_state", "Birth State:", 
                  choices=states),
      selectInput("zip_code", "Zip Code:", 
                  choices=zipcodes, selected = "27515"),
      sliderInput("birth_age", "Age:",
                  min=18, max=99, value=65),
      sliderInput("registration_year", "Year Registerd to Vote in NC:",
                  min=1948, max=2017, value=1980, sep = ""),
      
      fluidRow(column(3, verbatimTextOutput("years"))),
      hr(),
      helpText("Select the Values")
    ),
    
    mainPanel(
      tableOutput("values"), img(src='NCFlag.png', align = "right", height='300',width='600', align = "top")
    )
  )
)
    


#### Function to take all the inputs and calculate the probablity of leaning republican
get_probability <- function(birth_age,
                      registration_year,
                      race,
                      gender,
                      ethnicity,
                      drivers_lic,
                      birth_state,
                      zip_code ){
    #### Import the coeffiencts so that we can use the model

    coeff <- read.csv("Data/full_coefficients.csv", header = TRUE)
    
    #### Drop columns X, class, stderr
    coeff <- subset(coeff, select = c("name","index", "value") )
    
    #### Let's get the values of all of the coefficients for continuous variables plus the intercept
    intercept_coeff <- subset(coeff, name == "(intercept)")$value
    birth_age_coeff <- subset(coeff, name == "birth_age")$value
    age_squared_coeff <- subset(coeff, name == "age_squared")$value
    age_cubed_coeff <- subset(coeff, name == "age_cubed")$value
    years_since_registration_coeff <- subset(coeff, name == "years_since_registration")$value
    years_squared_coeff<- subset(coeff, name == "years_squared")$value
    years_cubed_coeff <- subset(coeff, name == "years_cubed")$value
    
    #### We now create all the calculated fields
    age_squared = birth_age^2
    age_cubed = birth_age^3
    years_since_registration = as.numeric(substr(Sys.Date(), 0, 4)) - as.numeric(registration_year) 
    years_squared = years_since_registration^2
    years_cubed = years_since_registration^3
    race_gender = paste0(race,gender)
    race_ethnicity = paste0(race,ethnicity)
    gender_ethnicity = paste0(gender,ethnicity)
    
    #### Now, let's write a function to pull out the specific value of a categorical coefficient
    get_coeff <- function(df, feature, fact){
      output <- subset(df, (name == feature & index == fact))$value
      if (length(output) == 0){
        return(0) # if not found, return 0
      }
      else{
        return(output)
      }
    }
    
    #### Now we calculate Y, the 'margin' or value of the polynomial that will be inputted into the logisitc funtion
    
    # First the continuous variables
    Y <- birth_age*birth_age_coeff+age_squared*age_squared_coeff+age_cubed*age_cubed_coeff
    Y <- Y + years_since_registration*years_since_registration_coeff+years_squared*years_squared_coeff+years_cubed*years_cubed_coeff
    # Now the intercept
    Y <- Y + intercept_coeff
    # Now the categorical variables
    Y <- Y + get_coeff(coeff,"race_gender", race_gender)
    Y <- Y + get_coeff(coeff,"race_ethnicity", race_ethnicity)
    Y <- Y + get_coeff(coeff,"gender_ethnicity", gender_ethnicity)
    Y <- Y + get_coeff(coeff,"zip_code", zip_code)
    Y <- Y + get_coeff(coeff,"birth_state", birth_state)
    Y <- Y + get_coeff(coeff,"drivers_lic", drivers_lic)
    
    #### We will use the R function "logigistic" from the psych package to calcluate the probabilities
    library(psych) # for built in logistic function
    probability <- logistic(Y)
    if (probability >.50){
      party_preference <- "Republican"
    }else{
      party_preference <- "Democratic"}
    
    return(probability)
}

# Define a server for the Shiny app
server <- function(input, output) {
  score <- .50
  party <- "None"
  registration_year <- 1950
  
  
  tablevalues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Race", 
               "Gender",
               "Ethnicity",
               "Drivers Licence",
               "Birth State",
               "Zip Code",
               "Age",
               "Registration_Year",
               "Score",
               "Prediction"),
      Value = as.character(c(input$race, 
                             input$gender,
                             input$ethnicity,
                             input$drivers_lic,
                             input$birth_state,
                             input$zip_code,
                             input$birth_age,
                             input$registration_year,
                             get_probability(input$birth_age,
                                             input$registration_year,
                                             race_dict[[input$race]],
                                             gender_dict[[input$gender]],
                                             ethnicity_dict[[input$ethnicity]],
                                             license_dict[[input$drivers_lic]],
                                             input$birth_state,
                                             input$zip_code),
                             if (get_probability(input$birth_age,
                                                 input$registration_year,
                                                 race_dict[[input$race]],
                                                 gender_dict[[input$gender]],
                                                 ethnicity_dict[[input$ethnicity]],
                                                 license_dict[[input$drivers_lic]],
                                                 input$birth_state,
                                                 input$zip_code) > .50){"Republican"} else {"Democratic"}
                             )), 
      stringsAsFactors=FALSE)
    
    
  })

  # Show the values using an HTML table
  #observeEvent(input$action, {output$values <- renderTable({tablevalues() })})
  
  output$values <- renderTable({
    tablevalues()
    
  })
  
  
  
}
  
  
  




shinyApp(ui=ui, server=server)

 

                