library(shiny)


             

ui <- fluidPage(
  
  
  titlePanel("Analiza zdravja in fizične aktivnosti v Evropi"),
  
  fluidRow(
    
    column(3,
           selectInput(inputId = "prvi_stolpec",
                  label = "Izberi državo",
                  choices = unique(skupaj$Država)),
           br(),
           selectInput(inputId = "drugi_stolpec",
                  label = "Izberi pokazatelja",
                  choices = unique(colnames(skupaj[-1])) 
           )
       ),
    column(8, offset = 1,
        tabsetPanel(
          tabPanel("Država", plotOutput("prvi_stolpec")),
          tabPanel("Pokazatelj", plotOutput("drugi_stolpec"))
      ))
    )
    
  )

      
      
    
      
    


           
