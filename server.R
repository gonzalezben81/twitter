##Server Code ####
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  df <- eventReactive(input$do, {
    withProgress(message = 'Running',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    tweets <- userTimeline(input$name, n = input$num)
    
    tweets_df <- tbl_df(map_df(tweets, as.data.frame))
    
    
    tweets.text = tweets_df$text
    
    tweets.text
  })
  
  observeEvent(input$get,{output$plot <- renderPlot({
    withProgress(message = 'Calculating Emotional Sentiment',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
   
     tweets <- userTimeline(input$name, n = input$num)
     
     tweets_df <- tbl_df(map_df(tweets, as.data.frame))
     
     
     tweets.text = tweets_df$text
     
     clean.text <- function(some_txt)
     {
       some_txt = gsub("&amp", "", some_txt)
       
       # some_txt<-gsub("[[:cntrl:]]","",some_txt)
       
       some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
       
       some_txt = gsub("@\\w+", "", some_txt)
       
       some_txt = gsub("[[:punct:]]", "", some_txt)
       
       some_txt = gsub("[[:digit:]]", "", some_txt)
       
       some_txt = gsub("http\\w+", "", some_txt)
       
       some_txt = gsub("[ ]{2,}", "", some_txt)
       
       some_txt = gsub("^\\s+|\\s+$...", "", some_txt)
       
       
       # define "tolower error handling" function
       
       try.tolower = function(x)
         
       {
         
         y = NA
         
         try_error = tryCatch(tolower(x), error=function(e) e)
         
         if (!inherits(try_error, "error"))
           
           y = tolower(x)
         
         return(y)
         
       }
       
       some_txt = sapply(some_txt, try.tolower)
       
       some_txt = some_txt[some_txt != ""]
       
       names(some_txt) = NULL
       
       return(some_txt)
       
     }
     
     ##Cleans the twitter data
     clean_text = clean.text(tweets.text)
     
     value <- get_nrc_sentiment(clean_text)
     
     
     
     barplot(
       sort(colSums(prop.table(value[, 1:10]))),
       horiz = input$horizontal,
       cex.names = 0.7,
       las = 1,
       main = paste(input$name," Emotional Sentiment")
       ,col = "blue"

     )
   })})
  
  
  
  ## Positive vs. Negative 
  
  observeEvent(input$gettwo,{output$plot2 <- renderPlot({
    withProgress(message = 'Calculating Positive vs Negative Sentiment',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    tweets <- userTimeline(input$name, n = input$num)
    
    tweets_df <- tbl_df(map_df(tweets, as.data.frame))
    
    
    tweets.text = tweets_df$text
    
    clean.text <- function(some_txt)
    {
      some_txt = gsub("&amp", "", some_txt)
      
      # some_txt<-gsub("[[:cntrl:]]","",some_txt)
      
      some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
      
      some_txt = gsub("@\\w+", "", some_txt)
      
      some_txt = gsub("[[:punct:]]", "", some_txt)
      
      some_txt = gsub("[[:digit:]]", "", some_txt)
      
      some_txt = gsub("http\\w+", "", some_txt)
      
      some_txt = gsub("[ ]{2,}", "", some_txt)
      
      some_txt = gsub("^\\s+|\\s+$...", "", some_txt)
      
      
      # define "tolower error handling" function
      
      try.tolower = function(x)
        
      {
        
        y = NA
        
        try_error = tryCatch(tolower(x), error=function(e) e)
        
        if (!inherits(try_error, "error"))
          
          y = tolower(x)
        
        return(y)
        
      }
      
      some_txt = sapply(some_txt, try.tolower)
      
      some_txt = some_txt[some_txt != ""]
      
      names(some_txt) = NULL
      
      return(some_txt)
      
    }
    
    ##Cleans the twitter data
    clean_text = clean.text(tweets.text)
    
    value <- get_nrc_sentiment(clean_text)
    
    
    
    barplot(
      sort(colSums(prop.table(value[, 9:10]))),
      horiz = input$horizontal,
      cex.names = 0.7,
      las = 1,
      main = paste(input$name," Positive vs Negative Sentiment")
      ,col = "blue"
    )
  })})
  
  
  observeEvent(input$get2,{texterdf<- reactive({
    texter<-userTimeline(searchString = input$name, n=input$num) 
    texter <- tbl_df(map_df(texter, as.data.frame))
    texter
    return(df)
    
  })})
  
  
  ###Creates Twitter Data Frame
  tweetdf <- reactive({
    withProgress(message = 'Creating Twitter Data Table',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    # generate bins based on input$bins from ui.R
    tweets <- userTimeline(user = input$name,n = input$num)
    tweets<-tbl_df(map_df(tweets,as.data.frame))
    tweets
  })
  # 
  observeEvent(input$display,{output$table<-DT::renderDataTable(tweetdf(), options = list(lengthChange = TRUE,autoWidth = TRUE,scrollX = TRUE),filter='top',
                                    class = "cell-border stripe")})
  

  
  output$download <- downloadHandler(
    filename = function() { paste("Twitter Data Frame",input$name, sep='',".csv") },
    content = function(file) {
      withProgress(message = 'Downloading Twitter Data',
                   value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      
      texter<-userTimeline(input$name, n = input$num)
      texterdf <- tbl_df(map_df(texter, as.data.frame))
      write.csv(texterdf, file)
      
    })
  
  
}

 


