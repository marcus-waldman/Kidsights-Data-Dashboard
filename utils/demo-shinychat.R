if(FALSE){
  library(shiny)

  
 # Sys.setenv(ANTHR0PIC_AdI_K3Y = "")
  ui <- bslib::page_fluid(
    chat_ui("chat", fill = T)
  )
  
  server <- function(input, output, session) {
    
    observeEvent(input$chat_user_input, {
      print(input$chat_user_input)
      chat <- ellmer::chat_anthropic(system_prompt = input$chat_user_input)
      stream <- chat$stream_async(input$chat_user_input, tool_mode = "sequential")
      shinychat::chat_append("chat", stream)
    })
  }
  
  shinyApp(ui, server)
}

