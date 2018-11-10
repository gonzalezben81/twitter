#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(twitteR)

api_key <- "u4a6iqPs58FvQydRpqVQHpiIy"

api_secret <- "cFAQLO5KjAQ3DF5XmdrFzJKlekc8sNQXfdrvD7p0Jsls7oQWmP"

access_token <- "	4808407757-wJeb7o4shlAV8SJfvOPzRbH5PTel5vZE9NAzt6o"

access_token_secret <- "KspPEnRW29bk40kNqlAMRRxqynjaFp8VOeaXHnn5jYEuU"

setup_twitter_oauth(api_key,api_secret)

# install.packages(c("shiny","ROAuth","RCurl","dplyr","purrr","XLConnect","ggplot2",
#                    "tidytext","tidyr","lubridate","scales","stringr","wordcloud","tm"))

library(shiny)
# library(devtools)
library(ROAuth)
library(RCurl)
library(dplyr)
library(purrr)
library(XLConnect)
library(ggplot2)
library(tidytext)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)
library(wordcloud)
library(tm)
library(ggplot2)
library(DT)
library(plyr)
library(xtable)
library(syuzhet)

## UI Code #####
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(includeScript("www/google-analytics.js")),
   # Application title
   titlePanel("Twitter Sentiment"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput(inputId = "name",label = "Twitter Handle",placeholder = "realdonaldtrump"),
         numericInput(inputId = "num",label = "Number of Tweets",value = 10),
         br(),
         actionButton(inputId = "display",label = "Create Twitter Table"),
         hr(),
         actionButton(inputId = "get",label = "Calculate Emotional Sentiment"),
         hr(),
         actionButton(inputId = "gettwo",label = "Calculate Positive vs. Negative Sentiment"),
         hr(),
         checkboxInput(inputId = "horizontal", label = "Horizontal Bars", value = FALSE),
         hr(),
         downloadButton("download", label="Download Twitter Data")

         
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Twitter Table",DT::dataTableOutput("table")),
          tabPanel(title = "Emotional Sentiment",plotOutput("plot")),
          tabPanel(title = "Positive vs. Negative",plotOutput("plot2"))
                  )
      )
   )
)



