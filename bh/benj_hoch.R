library(shiny)
library(ggplot2)

# Given p-values and an alpha, generate a table of BH critical values along
# side the conclusion about significance
bh_correction <- function(pv, alpha, sorted) {
  if (pv == "") {
    stop("Some p-values must be given")
  }
  pv <- as.numeric(strsplit(pv, split= ",[:blank:]*")[[1]])
  if (any(is.na(pv))) {
    stop("Non-numeric entry in p-value")
  }
  if (any(pv <= 0) | any(pv > 1)) {
    stop("p-values must be in (0,1]")
  }
  alpha <- as.numeric(alpha)
  if (is.na(alpha)) {
    stop("alpha is non-numeric or missing")
  }
  if (alpha <= 0 | alpha >= 1) {
    stop("alpha must be between 0 and 1")
  }
  outtab <- data.frame(pv = pv)
  outtab$row <- seq_len(nrow(outtab))
  outtab <- outtab[order(outtab$pv), ]
  outtab$critvals <- seq_len(nrow(outtab))*alpha/nrow(outtab)
  thresh <- max(which(outtab$pv < outtab$critvals))
  outtab$signif <- seq_len(nrow(outtab)) <= thresh
  
  if (!sorted) {
    print("Sorting!")
    outtab <- outtab[order(outtab$row), ]
  }
  outtab <- outtab[, names(outtab) != "row"]
  return(outtab)
}


# Define UI for dataset viewer app ----
ui <- fluidPage(
  titlePanel("Benjamini–Hochberg procedure"),

  sidebarLayout(
    sidebarPanel(
      p("To control the False Discover Rate across a series of tests, 
        enter here a list of p-values. Additionally, you may choose your 
        alpha parameter, which is the proportion of false positives 
        among all tests performed."),
      textInput(inputId = "pvalues",
                label = "p-values:",
                value = ".01, .05, .03, .2, .001"),
      textInput(inputId = "alpha",
                label = "False Discovery Rate (alpha):",
                value = ".05"),
      p("By checking the box below, the output will be sorted by 
        p-value. This may make it more clear what the B-H procedure 
        is doing (especially in the plot). This is for display purposes
        only and does not affect the calculations."),
      checkboxInput(inputId = "sorted",
                    label = "Sort p-values?",
                    value = FALSE),
      br(),
      p("The Benjamini-Hochberg procedure works by first sorting
        all p-values in order from smallest to largest and giving
        them ranks (e.g. the first has rank 1, the second rank 2, etc).
        For each p-value, a critical value is calculated as
        alpha*rank/(# tests). The largest p-value for which this
        critical value is greater than the p-value is identified,
        and all p-values from the smallest through the identified
        p-value are determined to be significant,")
    ),

    mainPanel(
      tableOutput("result"),
      p("In this plot, the p-values are represented by the points (values on
        the x-axis correspond to Row No. in the above table). The line
        represents the Benjamini-Hochberg critical values. The largest
        p-value at or below the line and all p-values smaller than that
        one are deemed to be significant. (Much more visually obvious if
        the \"Sort p-values\" option is checked.)"),
      plotOutput("plot"),
      p("It can occur that a p-value smaller than the largest p-value
        below the critical value is itself larger than its own critical
        value. This does not represent the problem, nor make the p-value 
        fail to reject.")
    )
  )
)

server <- function(input, output) {

  output$result <- renderTable({
    outtab <- bh_correction(input$pvalues, input$alpha, input$sorted)
    outtab <- cbind(seq_len(nrow(outtab)), outtab)
    names(outtab) <- c("No.", "p-value", "critical value", "Significant?")
    print(outtab)
  }, digits = 3)
  
  output$plot <- renderPlot({
    outtab <- bh_correction(input$pvalues, input$alpha, input$sorted)
    outtab$rows <- seq_len(nrow(outtab))
    ggplot(outtab, aes(x = rows)) + 
      geom_point(aes(y = pv, color = signif), size = 4) + 
      geom_line(aes(y = critvals)) + 
      theme(text = element_text(size = 20)) + 
      xlab("No.") + ylab("") + 
      guides(color=guide_legend("Sigificant?")) 
             
    
  })


}

shinyApp(ui, server)
