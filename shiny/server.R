library(shiny)
library(tidyr)
library(ggplot2)


shinyServer(function(input, output) {
  output$prvi_stolpec <- renderPlot({
    dfd <- pivot_longer(skupaj %>% filter(Država == input$prvi_stolpec), -Država, names_to = "Pokazatelji", values_to = "Vrednosti")
    
    graf.drzava <- ggplot(dfd, aes(x = Država, y = Vrednosti, fill = Pokazatelji)) + geom_bar(aes(x=Pokazatelji),stat = "identity", width = 1)+ ylim(-10,10)
    print(graf.drzava)
    
  })
  
  
 
  output$drugi_stolpec <- renderPlot({
  
    pokazatelj <<- input$drugi_stolpec
    df <- pivot_longer(skupaj %>% select(pokazatelj,Država),-Država, names_to = "Pokazatelj", values_to = "Vrednosti")
    df[,c(1,2)] <- df[,c(2,1)]
    colnames(df) <- c("Pokazatelj","Države","Vrednosti")
    df$Vrednosti <- round(as.numeric(df$Vrednosti),2)
    
    graf.pokazatelj <- ggplot(df,aes(Države,pokazatelj,fill=Vrednosti)) + geom_bar(aes(x=Države),stat="identity") +geom_text(aes(label=Vrednosti),colour="white",vjust=10) + theme(axis.text = element_text(angle = 90))

    print(graf.pokazatelj)
    
   
    
  })
    
   
}
  )