library(shiny)
library(shinydashboard)

shinyServer <- function(input, output, session) {
  output$TorontoInfection <- renderPlot({ggplot(tor_inf_df, 
                                                aes(x="", y=value, fill=paste(group, pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="PiYG") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0.1) +
      guides(fill = guide_legend(title = "Source of Infection"))
  })
  
  output$YorkInfection <- renderPlot({ggplot(yor_inf_df, 
                                             aes(x="", y=value, fill=paste(group,pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="Paired") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0) +
      guides(fill = guide_legend(title = "Source of Infection"))
    
  })
  
  output$YorkOutcome <- renderPlot({ggplot(yor_out_df, 
                                           aes(x="", y=value, fill=paste(group, pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="Pastel2") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0.1) +
      guides(fill = guide_legend(title = "Status"))
  })
  
  output$TorontoOutcome <- renderPlot({ggplot(tor_out_df, 
                                              aes(x="", y=value, fill=paste(group, pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="Pastel1") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0.1) +
      guides(fill = guide_legend(title = "Status"))
    
    
    
    
  })
  
  
}