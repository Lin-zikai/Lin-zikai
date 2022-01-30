rm(list = ls())
library("randomForest")
library(caTools)
library(shiny)
library(rsconnect)
options(encoding = "UTF-8")
## R Shiny Dashboard of “Bike Sharing Dataset” - Analysis ##

#Import the dataset
data <- read.csv('hour.csv')[,-1]
str(data)

#Data preparation
#Arranging values and changing data type
data$yr <- as.factor(ifelse(data$yr == 0, '2011', '2012'))

data$mnth <- as.factor(months(as.Date(data$dteday), 
                              abbreviate = TRUE))

data$hr <- factor(data$hr)

data$weekday <- as.factor(weekdays(as.Date(data$dteday)))

data$season <- as.factor(ifelse(data$season == 1, 'Spring',
                                ifelse(data$season == 2, 'Summer',
                                       ifelse(data$season == 3, 
                                              'Fall', 'Winter'))))

data$weathersit <- as.factor(ifelse(data$weathersit == 1, 'Good',
                                    ifelse(data$weathersit == 2, 
                                           'Fair',
                                           ifelse(data$weathersit == 
                                                    3, 'Bad', 
                                                  'Very Bad'))))

data$holiday<-as.factor(ifelse(data$holiday == 0, 'No', 'Yes'))

data$workingday<-as.factor(ifelse(data$workingday == 0, 'No', 
                                  'Yes'))

#Changing columns names
names(data)[names(data) == "registered"] <- "new"
names(data)[names(data) == "cnt"] <- "total"

#Denormalizing the values
#Temperature
for (i in 1:nrow(data)){
  tn = data[i, 10]
  t = (tn * (39 - (-8))) + (-8)
  data[i, 10] <- t
}

#Feeling temperature
for (i in 1:nrow(data)){
  tn = data[i, 11]
  t = (tn * (50 - (-16))) + (-16)
  data[i, 11] <- t
}

#Humidity
data$hum <- data$hum * 100

#Wind speed
data$windspeed <- data$windspeed * 67

#Write the new file
data <- data[-1]
# write.csv(data, "bike_sharing.csv", row.names = FALSE)

#Modeling
#Dropping columns
data <- data[c(-1,-2,-7,-13,-14)]

#Splitting data


set.seed(1)
split = sample.split(data$total, SplitRatio = 0.8)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

rf = randomForest(formula = total ~ ., data = train,
                  ntree = 100)



test_pred<- test

values = data.frame(mnth = '1月', 
                    hr = '0', 
                    holiday = 'No', 
                    weekday = '星期六',
                    weathersit = 'Good',
                    temp = 3.8, 
                    atemp = 3.0014, 
                    hum = 81, 
                    windspeed = 0,
                    total = NA)

test_pred <- rbind(test_pred,values)
text<-test_pred[nrow(test_pred),-10]
prediction <- predict(rf, newdata =text )
prediction
importance(rf)
print(rf) 
plot(rf)
varImpPlot(x=rf,sort=TRUE,n.var=nrow(rf$importance),main="输入变量重要性测度散点图")


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Reactivity"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(inputId = "month",
                  label = "Choose a month:",
                  choices = c("1月" , "2月" , "3月")),
      numericInput(inputId = "hour",
                   label = "Number of hour:",
                   value = 10),
      selectInput(inputId = "holiday",
                  label = "Choose holiday:",
                  choices = c("No",  "Yes")),
      selectInput(inputId = "weekday",
                  label = "Choose a weekday:",
                  choices = c("星期六", "星期日")),
      selectInput(inputId = "weather",
                  label = "Choose a weather:",
                  choices = c("Good" ,"Fair" ,"Bad")),
      numericInput(inputId = "temp",
                   label = "Number of temperture:",
                   value = 4),
      numericInput(inputId = "atemp",
                   label = "Number of atemp:",
                   value = 4),
      numericInput(inputId = "hum",
                   label = "Number of hum:",
                   value = 80),
      numericInput(inputId = "windspeed",
                   label = "Number of windspeed:",
                   value = 0)

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h3( textOutput("cap1", container = span)),
      # Output: Formatted text for caption ----
      h3( textOutput("cap", container = span)),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
     
      
    )
  )
)

server <- function(input, output) {

  datasetInput <- reactive({
    switch(input$month,
           "1月"== "1月" ,
           "2月"== "2月", 
           "3月"=="3月")
    switch(input$holiday,
           "No"== No ,
           "Yes"== Yes)
    switch(input$weekday,
           "星期六"== "星期六",  
           "星期日"== "星期日")
    switch(input$weather,
           "Good" == Good ,"Fair"==Fair ,"Bad"==Bad)
  })
  output$cap1 <- renderText({
   "预测结果："
  })
  output$cap <- renderText({
    values = data.frame(mnth = input$month, 
                        hr =input$hour , 
                        holiday =input$holiday , 
                        weekday =input$weekday,
                        weathersit =input$weather ,
                        temp = input$temp, 
                        atemp = input$atemp, 
                        hum =input$hum , 
                        windspeed = input$windspeed,
                        total = NA)
    
    test_pred <- rbind(test_pred,values)
    text<-test_pred[nrow(test_pred),-10]
    prediction <- predict(rf, newdata =text )
    prediction
  })
  output$mpgPlot <- renderPlot({
    varImpPlot(x=rf,sort=TRUE,n.var=nrow(rf$importance),main="输入变量重要性测度散点图")
  })

  
}

# Create Shiny app ----
shinyApp(ui, server)










