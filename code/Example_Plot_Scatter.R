Example_Plot_Scatter <- function(DataInput, X, Y, Z, Theme, Title, Ylab, Xlab, LegendTitle, TitleSize = 10, LabSize = 10) {

  g <-
    ggplot(data = DataInput) +
    geom_point( aes_string(x = X, y = Y, color = Z) ) +
    Theme + ggtitle(Title) +
    theme(plot.title = element_text(size = TitleSize)) +
    theme(axis.title = element_text(size = LabSize),
          axis.title.x = element_text()) +
    ylab(Ylab) +
    xlab(Xlab) +
    labs(color = LegendTitle)

  g
}
