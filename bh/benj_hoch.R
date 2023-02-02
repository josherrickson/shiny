library(shiny)


make_table <- function(pv, alpha) {
  pv <- as.numeric(strsplit(pv, split= ",[:blank:]*")[[1]])
  if (any(is.na(pv))) {
    stop("Non-numeric entry in p-value")
  }
  if (any(pv <= 0) | any(pv > 1)) {
    stop("p-values must be in (0,1]")
  }
  sorted <- data.frame(pv = pv)
  sorted$row <- seq_len(nrow(sorted))
  sorted <- sorted[order(sorted$pv), ]
  sorted$critvals <- seq_len(nrow(sorted))*as.numeric(alpha)/nrow(sorted)
  thresh <- max(which(sorted$pv < sorted$critvals))
  sorted$signif <- seq_len(nrow(sorted)) <= thresh
  return(sorted)
}


# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Benjamini Hochberge Correction"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "pvalues",
                label = "P-values:",
                value = ".01, .05, .03, .2, .001"),

      # Input: Numeric entry for number of obs to view ----
      textInput(inputId = "alpha",
                label = "False Discovery Rate (alpha):",
                value = ".05")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      h3(textOutput("caption1", container = span)),
      tableOutput("unsorted"),

      h3(textOutput("caption2", container = span)),
      tableOutput("sorted")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  output$caption1 <- renderText("Unsorted Table")

  output$unsorted <- renderTable({
    sorted <- make_table(input$pvalues, input$alpha)
    print(sorted[, names(sorted) != "row"])
  })

  output$caption2 <- renderText("Sorted table")

  output$sorted <- renderTable({
    sorted <- make_table(input$pvalues, input$alpha)
    sorted <- sorted[order(sorted$row), ]
    print(sorted[, names(sorted) != "row"])
  })

}

# Create Shiny app ----
shinyApp(ui, server)
