library(shiny)

ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(width = 1, offset = 0, shiny::div("Email: ")),
    shiny::column(
      width = 3, offset = 0,
      shiny::plotOutput(outputId = "email", height = "20px", width = "200px")
      )
  ),
  shiny::fluidRow(
    shiny::column(width = 12, offset = 0,
      shiny::tags$ul(
        shiny::tags$li("hello"),
        shiny::tags$li("world"),
        shiny::tags$a("href" = "https://github.com", shiny::icon(name = "github", class = "fa-2x"))
      )
    )
  )
)


email <- function(.txt) {
  grid::grid.newpage()
  # grid::grid.lines(x = c(0.5, 0.5), y = c(0, 1))
  # grid::grid.lines(x = c(0, 1), y = c(0.5, 0.5))
  grid::grid.text(
    label = .txt, hjust = 0, vjust = 1,
    x = 0, y = 1, 
    gp = grid::gpar(col = "#008176", fontsize = 18)
  )
}

server <- function(input, output, session) {
  output$email <- shiny::renderPlot({email('samliu@hust.edu.cn')})
}

shinyApp(ui, server)

