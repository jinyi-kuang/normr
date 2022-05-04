#' A ridge plot show the VOI density distribution of a individual measure by groups
#'
#' \code{RidgeDensityIdv} A ridge plot show the VOI density distribution by groups
#' It is written for supplying value to be specified in the boxplot.
#' The boxplot value is calculated by the \link[normr]{DensityQuantile}.
#'
#' @param data A dateframe with var "psutype" and "psuname".
#' @param dfBoxplot A dataframe contain value calcualted from \link[normr]{DensityQuantile}..
#' @param dv Variable of interest to be calculated.
#' @return A plot show the VOI density ridge and boxplot with specified value
#' @keywords plot

RidgeDensityIdv <- function(data, dfBoxplot, dv, xlabel = "Proportion of Community Whom Respondent Believes Uses a Toilet", color = c("#2081f9", "#fbac49", "#bbbbbb"), adjust = adjust) {



  # quote variable
  require("dplyr")
  require("ggplot2")
  require("ggthemes")
  dv <- enquo(dv)

  # create df--------------------------------------------------
  w2 <- data %>%
    dplyr::select(!!dv,"psuname", "psutype")  %>%
    dplyr::mutate(O4 = (!!dv)/10)

  # geom_violin source ----------------------------------------
  # somewhat hackish solution to:
  # https://twitter.com/EamonCaddigan/status/646759751242620928
  # based mostly on copy/pasting from ggplot2 geom_violin source:
  # https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r


  "%||%" <- function(a, b) {
      if (!is.null(a)) a else b
  }


  geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
  position = "dodge", trim = TRUE, scale = "area",
  show.legend = NA, inherit.aes = TRUE, ...) {
      layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
      trim = trim,
      scale = scale,
      ...
      )
      )
  }


  GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
  setup_data = function(data, params) {
      data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
      group_by(group) %>%
      mutate(ymin = min(y),
      ymax = max(y),
      xmin = x,
      xmax = x + width / 2)

  },

  draw_group = function(data, panel_scales, coord) {
      # Find the points for the line to go all the way around
      data <- transform(data, xminv = x,
      xmaxv = x + violinwidth * (xmax - x))

      # Make sure it's sorted properly to draw the outline
      newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
      plyr::arrange(transform(data, x = xmaxv), -y))

      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])

      ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },

  draw_key = draw_key_polygon,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
  alpha = NA, linetype = "solid"),

  required_aes = c("x", "y")
  )



  # plot ----------------------------------------------------

  # jk suggestion red-blue-gray
  mycolors <- c("#2081f9", "#fbac49", "#bbbbbb")

  df <- w2 %>% # for individual
    group_by(psutype) %>%
    dplyr::summarize(mee = mean(O4,na.rm = T), n=n())


  g <- ggplot(data = w2, aes(y = O4, x = psutype, fill = psutype)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, bw = 0.1, trim = F, colour = "black", adjust  = adjust) +
    geom_boxplot(inherit.aes = F,data = dfBoxplot, aes(x = psutype, fill = psutype, ymin = value0, lower = value25/10, middle = value50/10, upper = value75/10, ymax = value100/10), stat = "identity", width = .1, outlier.shape = NA, alpha = 0.1, colour = "black" ) +
    # geom_point(aes(y = O4/10, color = psutype), position = position_jitter(width = .04, height = 0.05), size = .3, alpha = 0.2) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +

    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_colour_manual(values  = color) +
    scale_fill_manual(name = "Geography",
                      values = color) +
    labs(title="",
        y = xlabel ,
        x = ""
       ) +

    coord_flip() +
    theme_few() +
    theme(plot.margin = unit(c(1,1,1,0), "cm"))

 return(g)

}
