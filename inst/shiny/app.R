library(nm2mrg)
library(shiny)
library(shinyalert)


ui <- fluidPage(
  titlePanel("nm2mrg"),
  sidebarLayout(
    sidebarPanel(
      fileInput("modFile", "Convert .mod with initial parameters", accept = c(".mod", ".ctl")),
      fileInput("modFile2", "Convert .mod with final estimates (.ext, .lst)", accept = c(".mod", ".ctl", ".ext", ".lst"), multiple = TRUE),
      checkboxInput("addcap", "Add $CAPTURE", TRUE),
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
    if (!is.null(input$modFile) & !is.null(input$modFile2)) {
      shinyalert::shinyalert(
        title = "Error",
        text = "Please use only one option.",
        type = "error"
      )
      return()
    }

    if (is.null(input$modFile) & is.null(input$modFile2)) {
      shinyalert::shinyalert(
        title = "Error",
        text = "Please upload some files.",
        type = "error"
      )
      return()
    }

    if (!is.null(input$modFile)) {
      tmp_dir <- dirname(input$modFile$datapath)
      tmp_name <- input$modFile$name
      if (!file.exists(paste0(tmp_dir, "/", tmp_name))) {
        file.rename(input$modFile$datapath, paste0(tmp_dir, "/", tmp_name))
      }
      res <- withCallingHandlers(
        tryCatch(
          nm2mrg(
            mod_name = gsub("\\..+$", "", input$modFile$name),
            dir = dirname(input$modFile$datapath),
            add_CAPTURE = input$addcap
          ),
          error = function(e) {
            shinyalert::shinyalert(
              title = "Error",
              text = conditionMessage(e),
              type = "error"
            )
            return(NULL)
          }
        ),
        warning = function(w) {
          shinyalert::shinyalert(
            title = "Warning",
            text = conditionMessage(w),
            type = "warning"
          )
          invokeRestart("muffleWarning")
        }
      )
      if (is.null(res)) return()
      output_text <- res
    } else if (!is.null(input$modFile2)) {
      if (length(unique(gsub("\\..+$", "", input$modFile2$name))) != 1) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Please upload files with the same name.",
          type = "error"
        )
        return()
      }
      if (nrow(input$modFile2) != 3) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Please upload 3 files (.mod, .ext, .lst).",
          type = "error"
        )
        return()
      }

      for (i in 1:nrow(input$modFile2)) {
        tmp_dir <- dirname(input$modFile2$datapath[i])
        tmp_name <- input$modFile2$name[i]
        if (!file.exists(paste0(tmp_dir, "/", tmp_name))) {
          file.rename(input$modFile2$datapath[i], paste0(tmp_dir, "/", tmp_name))
        }
      }
      res <- withCallingHandlers(
        tryCatch(
          nm2mrg(
            mod_name = gsub("\\..+$", "", input$modFile2$name[1]),
            dir = dirname(input$modFile2$datapath[1]),
            use_final = TRUE,
            add_CAPTURE = input$addcap
          ),
          error = function(e) {
            shinyalert::shinyalert(
              title = "Error",
              text = conditionMessage(e),
              type = "error"
            )
            return(NULL)
          }
        ),
        warning = function(w) {
          shinyalert::shinyalert(
            title = "Warning",
            text = conditionMessage(w),
            type = "warning"
          )
          invokeRestart("muffleWarning")
        }
      )
      if (is.null(res)) return()
      output_text <- res
    }


    text_content(output_text)
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
      if (is.null(input$modFile$name)) {
        paste0(gsub("\\..+$", "", input$modFile2$name[1]), ".cpp")
      } else {
        paste0(gsub("\\..+$", "", input$modFile$name), ".cpp")
      }
    },
    content = function(file) {
      req(conversion_complelte())
      writeLines(text_content(), con = file)
    }
  )
}


shinyApp(ui = ui, server = server)
