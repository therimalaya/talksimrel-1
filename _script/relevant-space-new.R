y_x0 <- 0.05
y_y0 <- 0.1

y_width <- 0.2
x_width <- 0.6

x_x0 <- y_x0 + y_width + 0.1
x_y0 <- 0.1

y_height <- x_height <- 0.6

grid.newpage()

## Rectangles for X and Y variables ----
grid.rect(x = y_x0, 
          y = y_y0, 
          height = y_height, 
          width = y_width, 
          hjust = 0, vjust = 0)
grid.rect(x = x_x0, 
          y = x_y0, 
          height = x_height, 
          width = x_width, 
          hjust = 0, vjust = 0)

## Relevant space in Y space ----
df_y <- data.frame(
  x = c(0.05, 0.05, 0.05, y_width) + y_x0,
  y = c(0.05, 0.05, 0.05, y_height) + y_y0
)
grid.xspline(
  x = rep(df_y$x, 2),
  y = rep(rev(df_y$y), 2),
  gp = gpar(fill = "green4", alpha = 0.5, lwd = 2),
  shape = 1, open = FALSE
)

## Relevant space in X space ----
df_x <- data.frame(
  x = c(0.05, 0.05, 0.05, x_width) + x_x0,
  y = c(0.05, 0.05, 0.05, x_height) + y_y0
)
grid.xspline(
  x = rep(df_x$x, 2),
  y = rep(rev(df_x$y), 2),
  gp = gpar(fill = "blue4", alpha = 0.5, lwd = 2),
  shape = 1, open = FALSE
)

## Title of the diagram ----
grid.text(x = y_x0, y = 0.95, label = "Relevant space within a model", 
          hjust = 0, gp = gpar(cex = 1.6))
## Subtitle of the diagram ----
grid.text(x = y_x0, y = 0.88, hjust = 0, gp = gpar(cex = 0.9),
          label = "A concept behind reduction of regression model")

## Labels on top of the X and Y boxes ----
grid.text(x = y_x0 + y_width/2, 
          y = y_y0 + y_height, 
          label = "Response (Y)", 
          draw = T, vjust = -1,
          gp = gpar(cex = 1))
grid.text(x = x_x0 + x_width/2 , 
          y = x_y0 + x_height, 
          label = "Predictor (X)", 
          draw = T, vjust = -1, 
          gp = gpar(cex = 1))

## Labels inside the X and Y boxes ----
grid.text(x = y_x0 + y_width/2, 
          y = y_y0 + y_height, 
          vjust = 1.5,
          label = "Redudent Y\ninformation",
          gp = gpar(cex = 0.8))
grid.text(x = x_x0 + x_width/2 , 
          y = x_y0 + x_height, 
          vjust = 1.5,
          gp = gpar(cex = 0.8),
          label = "Irrelevant X-Space\n redudent information and noise")

## Big arror from X to Y to represent their connection ----
big_arrow <- function(lw = 22, color = "grey40"){
  segmentsGrob(x0 = x_x0 - 0.01, 
               x1 = y_x0 + y_width + 0.05, 
               y0 = x_height/2, 
               y1 = x_height/2,
               arrow = arrow(length = unit(0.15, "inches")),
               gp = gpar(lwd = lw, 
                         linejoin = "mitre", 
                         lineend = "butt",
                         col = color))
}
grid.draw(big_arrow(25, "grey50"))
grid.draw(big_arrow(20, "grey90"))

## Arrows to annotate relevant X and Y spaces ----
grid.segments(x0 = c(y_x0 + y_width/2, x_x0 + x_width/2), 
              x1 = c(y_x0 + y_width/2, x_x0 + x_width/2),
              y0 = c(y_y0 - 0.05, y_y0 - 0.05), 
              y1 = c(y_x0 + 0.1, y_x0 + 0.1),
              arrow = arrow(length = unit(0.09, "inches")),
              gp = gpar(lty = 1, lwd = 2, col = "grey40"))
grid.segments(x0 = y_x0 + y_width/2, 
              y0 = y_y0 - 0.05, 
              x1 = x_x0 + x_width/2, 
              y1 = y_y0 - 0.05)

## Text annotation for relevant X and Y space ----
grid.text(x = x_x0, y = x_y0 - 0.1, gp = gpar(cex = 0.9),
          label = "X and Y envelope/\n Relevant Spaces",
          vjust = -0.25)
