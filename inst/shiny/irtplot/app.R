library(shiny)
ui <-
  fluidPage(
    titlePanel("Item Response Theory"),
    sidebarLayout(
      sidebarPanel(sliderInput(
        "a",
        "Discrimination (a)",
        min = 0,
        max = 5,
        value = 2,
        step = .01
      ),
      sliderInput(
        "b",
        "Item Difficulty (b)",
        min = -4,
        max = 4,
        value = -1,
        step = .1
      ),
      sliderInput(
        "c",
        "Guessing (c)",
        min = 0,
        max = 1,
        value = 0,
        step = .01
      ),
      sliderInput(
        "d",
        "Upperbound (d)",
        min = 0,
        max = 1,
        value = 1,
        step = .01
      ),
      sliderInput(
        "theta",
        "Ability (theta)",
        min = -4,
        max = 4,
        value = 0,
        step = .1
      )),
      mainPanel = mainPanel(plotOutput("modelplot"))
    )
  )
server <- function(input, output, session) {
  output$modelplot <- renderPlot({
    a <- input$a
    b <- input$b
    c <- input$c
    d <- input$d
    par(pty = "s", cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.3, mar = c(5,5,3,6))
    theta <- seq(-4, 4, 0.01)
    plot(
      theta,
      c + (d - c) / (1 + exp(-1 * a * (theta - b))),
      xlim = c(-4, 4),
      ylim = c(0, 1),
      xlab = expression("Ability" ~ (theta)),
      ylab = "Probability of Success",
      main = bquote(italic(a) == .(a)),
      bty = "n",
      col = "royalblue",
      lwd = 2,
      type = "l",
      las = 1
    )
    abline(h = c, lty = 3)
    abline(h = d, lty = 3)
    lines(c(b, b), c(c, (d + c) / 2), lty = 2, col = "royalblue")
    points(b,
           (d + c) / 2,
           col = "royalblue",
           pch = 16,
           cex = 1.5)

    text(b, c, bquote(italic(c) == .(c)), pos = 1, xpd = NA, cex = 1.3)
    text(b, d, bquote(italic(d) == .(d)), pos = 3, xpd = NA, cex = 1.3)
    text(b,
         c,
         bquote(italic(b) == .(b)),
         srt = 90,
         adj = c(-0.1,-0.5),
         xpd = NA,
         cex = 1.3)

    text(b, (d + c) / 2, expression(P(italic({
      x == 1
    }) * "|" * {
      italic(theta) == italic(b)
    }) == (italic(d) + italic(c)) / 2),
    pos = 4,
    xpd = NA,
    cex = 1.3)

    text(
      b,
      (d + c) / 2,
      expression(P * minute * (italic(b)) == italic(a * (d - c) / 4)),
      adj = c(0.5,-0.3),
      srt = (180 / pi) * atan(8 * a * (d - c) / 4),
      xpd = NA,
      cex = 1.3
    )

    points(input$theta,
           c + (d - c) / (1 + exp(-1 * a * (input$theta - b))),
           pch = 16,
           col = "firebrick", cex = 1.5)



  }, width = 700, height = 700)
}

shinyApp(ui, server)
