library(shiny)


ui <- fluidPage(
  titlePanel("nm2mrg"),

  sidebarLayout(
    sidebarPanel(
      fileInput("modFile", "Select mod", accept = c(".mod", ".ctl")),
      actionButton("convert", "Convert"),
      htmlOutput("html")
    ),

    mainPanel(
      textAreaInput("text", "Output editor", row = 40, width = "100%")
    )
  )
)


server <- function(input, output, session) {
  text_content <- reactiveVal()
  conversion_complelte <- reactiveVal(FALSE)

  observeEvent(input$convert, {
    req(input$modFile)

    file.rename(input$modFile$datapath, paste0(dirname(input$modFile$datapath), "/", input$modFile$name))

    output <- nm2mrg(mod_name = gsub("\\..+$", "", input$modFile$name), dir = dirname(input$modFile$datapath))

    text_content(output)
    updateTextAreaInput(session, "text", value = paste(text_content(), collapse = "\n"))

    conversion_complelte(TRUE)
  })

  observeEvent(input$text, {
    text_content(strsplit(input$text, "\n")[[1]])
  })

  output$html <- renderUI({
    req(conversion_complelte())
    tags$div(
      tags$b("Conversion has been completed!"),
      tags$br(),
      downloadButton("download", "Download .cpp file")
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      req(conversion_complelte())
      paste0(gsub("\\..+$", "", input$modFile$name), ".cpp")
    },
    content = function(file) {
      req(conversion_complelte())
      writeLines(text_content(), con = file)
    }
  )
}


shinyApp(ui = ui, server = server)
