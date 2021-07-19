#' @rdname geom_timeline
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = c("x", "y"=NULL),
  default_aes = ggplot2::aes(shape = 16, size = 1, alpha = 0.5, colour = "blue"),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    
    if(! "y" %in% colnames(data)){
      data <- dplyr::mutate(data, y=0.5)
    }
    
    coords <- coord$transform(data, panel_scales)
    uniqueY <- unique(coords$y)
    
    linelist <- grid::gList()
    for(thisY in uniqueY){
      line <- grid::linesGrob(
        x = dplyr::filter(coords, y==thisY)$x,
        y = dplyr::filter(coords, y==thisY)$y
      )
      
      linelist <- grid::gList(linelist, line)
    }
    
    pg <- grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      size = grid::unit(coords$size / 5, "char"),
      gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha))
    )
    
    grid::gList(linelist, pg)
  }
)


#' GGplot function that creates a timeline graph of a group of earthquakes
#'
#' @param mapping ggplot aes parameter
#' @param data input data - dataframe of NOAA earthquakes
#' @param stat ggplot stat parameter
#' @param position ggplot position parameter
#' @param na.rm ggplot na.rm parameter
#' @param show.legend ggplot show.legend parameter
#' @param inherit.aes ggplot inherit.aes parameter
#' @param ... ggplot other parameters
#'
#' @return Draws a GGplot graph
#' @export
#' @importFrom scales alpha
#' @importFrom grid unit
#' @importFrom grid gList
#' @importFrom grid linesGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 Geom
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{ggplot(inputData) + geom_timeline(aes(x=Date, colour=Deaths, n_max=5, size=Mag))}
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, 
                          show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_timeline_label
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,
  required_aes = c("x", "label", "y"=NULL, n_max=NULL),
  default_aes = ggplot2::aes(shape = 16, size = 1, alpha = 0.5, colour = "blue"),
  draw_panel = function(data, panel_scales, coord) {
    
    if(! "y" %in% colnames(data)){
      data <- dplyr::mutate(data, y=0.5)
    }
    
    if(! "n_max" %in% colnames(data)){
      maxForDisplay <- nrow(data)
    } else {
      maxForDisplay <- unique(data$n_max)
    }
    
    data <- top_n(x=data, n=maxForDisplay, wt=size)
    
    coords <- coord$transform(data, panel_scales)
    uniqueY <- unique(coords$y)
    
    segmentlist <- grid::gList()
    for(thisY in uniqueY){
      segment <- grid::segmentsGrob(
        x0 = unit(dplyr::filter(coords, y==thisY)$x, "native"), 
        y0 = unit(dplyr::filter(coords, y==thisY)$y, "native"),
        x1 = unit(dplyr::filter(coords, y==thisY)$x, "native"), 
        y1 = unit(dplyr::filter(coords, y==thisY)$y + 0.05, "native"),
        default.units = "native"
      )
      segmentlist <- grid::gList(segmentlist, segment)
    }
    
    textList <- grid::gList()
    for(thisY in uniqueY){
      text <- grid::textGrob(
        x = unit(dplyr::filter(coords, y==thisY)$x, "native"), 
        y = unit(dplyr::filter(coords, y==thisY)$y + 0.05, "native"),
        label = dplyr::filter(coords, y==thisY)$label,
        just = c("left","bottom"),
        rot=30
      )
      
      textList <- grid::gList(textList, text)
    }
    
    grid::gList(segmentlist, textList)
  }
)

#' GGplot function that creates a timeline graph of a group of earthquakes with associated labels
#'
#' @param mapping ggplot aes parameter
#' @param data input data - dataframe of NOAA earthquakes
#' @param stat ggplot stat parameter
#' @param position ggplot position parameter
#' @param na.rm ggplot na.rm parameter
#' @param show.legend ggplot show.legend parameter
#' @param inherit.aes ggplot inherit.aes parameter
#' @param ... ggplot other parameters
#'
#' @return Draws a GGplot graph
#' @export
#' @importFrom scales alpha
#' @importFrom grid unit
#' @importFrom grid gList
#' @importFrom grid linesGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 Geom
#' @importFrom dplyr filter mutate
#'
#' @examples
#' \dontrun{ggplot(inputData) + geom_timeline(aes(x=Date, colour=Deaths, label=Location, n_max=5, size=Mag))}
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, 
                          show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GGplot function that creates a timeline graph of a group of earthquakes with associated labels
#'
#'
#' @return Draws a GGplot graph
#' @export
#' @importFrom ggplot2 theme theme_classic element_blank
#'
#' @examples
#' \dontrun{ggplot(inputData) + geom_timeline(aes(x=Date, colour=Deaths, label=Location, n_max=5, size=Mag)) + theme_quake()}
theme_quake <- function(){
  ggplot2::theme_classic() + 
    ggplot2::theme(
      legend.position = "bottom",
      axis.line.y = ggplot2::element_blank()
    )
}

                 
