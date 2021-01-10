#modified from the defunct geom_net package

geom_net <- function (
  mapping = NULL, data = NULL, stat = "net", position = "identity", show.legend = NA, na.rm = FALSE, inherit.aes = TRUE,
  layout.alg="kamadakawai", layout.par=list(), directed = FALSE, fiteach=FALSE,  selfloops = FALSE, singletons = TRUE,
  alpha = 0.25, ecolour=NULL, ealpha=NULL, arrow=NULL, arrowgap=0.01, arrowsize=1,
  labelon=FALSE, labelcolour=NULL, labelgeom = 'text', repel = FALSE,
  vertices=NULL, ...) {
    ggplot2::layer(
    geom = GeomNet, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, layout.alg=layout.alg, layout.par=layout.par, fiteach=fiteach, labelon=labelon, labelgeom=labelgeom,
                  ecolour = ecolour, ealpha=ealpha, arrow=arrow, arrowgap=arrowgap, directed=directed, repel=repel,
                  arrowsize=arrowsize, singletons=singletons,
                  labelcolour=labelcolour, vertices=vertices, selfloops = selfloops,
                  ...)
  )
}

GeomNet <- ggplot2::ggproto("GeomNet", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5, label = NULL,
                             shape = 19, colour = "grey40", arrowsize = 1,
                             size = 4, fill = NA, alpha = NA, stroke = 0.5,
                             linewidth=1, angle=0, vjust=0, hjust=0.5, curvature = 0),

  draw_key = function(data, params, size)  {
    draw_arrow <-  NULL
    if (params$directed) {
      if (any(data$curvature != 0)){
        draw_arrow <- arrow(length = unit(params$arrowsize*5,"points"), type="open")
        }
      else{
        draw_arrow <- arrow(length = unit(params$arrowsize*5,"points"), type="closed")
      }
   }

    with(data, grobTree(
      grid::pointsGrob(x = c(.15, .85), y = c(.85, .15),
                       pch = data$shape, size = unit(data$size, "points"),
                       gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                       fill = alpha(data$fill, data$alpha),
                                       fontsize = data$size * .pt + data$stroke * .stroke/2,
                                       lwd = data$stroke * .stroke/2)
                       ),
     grid::segmentsGrob(x0 = .15, y0 = .85 ,
                        x1 = ifelse(is.null(draw_arrow), .85, .82),
                        y1 = ifelse(is.null(draw_arrow), .15, .18),
                     gp = grid::gpar(
                       col = alpha(data$colour, data$alpha),
                       fill = alpha(data$colour, data$alpha),
                       lwd = data$linewidth,
                       lty = data$linetype,
                       lineend="butt"),
                     arrow = draw_arrow
                     )

    ))
  },

  setup_data = function(data, params, mapping) {

    data$from <- as.character(data$from)
    data$to <- as.character(data$to)
    selfie <- (data$from == data$to) & (params$selfloops == TRUE)
    data$ymax = max(with(data, pmax(y, yend) + 2*0.05*selfie))
    data$xmax = with(data, pmax(x, xend) + 2*0.05*selfie)

    data$from <- factor(data$from)
    data$to <- factor(data$to)

    data
  },

  draw_panel = function(data, panel_scales, coord,  ecolour=NULL, ealpha=NULL, arrow=NULL, arrowgap=0.01,
                        directed=FALSE, arrowsize=1, repel = FALSE, singletons = TRUE,
                        labelon=FALSE, labelgeom='text', labelcolour=NULL, selfloops = FALSE) {



    edges <- data.frame(
      x = data$x,
      xend = data$xend,
      y = data$y,
      yend = data$yend,
      weight = data$weight,
      colour = ecolour %||% ifelse(data$.samegroup, data$colour, "grey40"),
      size = data$linewidth %||% (data$size / 4),
      nodesize = data$size,
      alpha = ealpha %||% data$alpha,
      linetype=data$linetype,
      stroke = data$stroke,
      selfie = data$.selfie,
      stringsAsFactors = FALSE
    )

    selfy <- subset(edges, selfie == TRUE)
    edges <- subset(edges, selfie != TRUE)
    edges <- subset(edges, !is.na(xend))

    vertices <- data.frame(
      x = data$x,
      y = data$y,
      colour = data$colour,
      shape = data$shape,
      size = data$size,
      fill = NA,
      alpha = data$alpha,
      stroke = 0.5,
      stringsAsFactors = FALSE
    )
    vertices <- unique(vertices)


	arrow=NULL
#
    if (any(data$curvature != 0)){
      edges_draw <- GeomCurve$draw_panel(edges, panel_scales,
                                         coord, arrow=arrow, curvature=data$curvature[1], angle=90)
    }
      else {edges_draw <- GeomSegment$draw_panel(edges, panel_scales, coord, arrow, lineend = "round")}

#

    selfies_draw <- NULL
    if ((nrow(selfy) > 0) & selfloops) {
      selfy$radius <- min(0.04, 1/sqrt(nrow(vertices)))
      selfy <- transform(selfy,
                           x = x + (radius + nodesize/(100*.pt) + size/100)/sqrt(2),
                           y = y + (radius + nodesize/(100*.pt) + size/100)/sqrt(2),
                           linewidth = size*.pt,
                           fill = NA
      )
      selfies_draw <- GeomCircle$draw_panel(selfy, panel_scales, coord)
    }

    selfies_arrows <- NULL

    label_grob <- NULL
    if (labelon | !is.null(data$label)) {
      labels <- data.frame(
        x = data$x,
        y = data$y,
        label = data$label %||% data$from,
        colour = labelcolour %||% data$colour,
        shape = data$shape,
        size = data$fontsize,
        angle = data$angle,
        alpha = data$alpha,
        hjust = data$hjust,
        fill = data$colour,
        vjust = data$vjust,
        stringsAsFactors = FALSE
      )
      labels <- unique(labels)
      if (labelgeom=='label'){
        if(repel){
          label_grob <- ggrepel::GeomLabelRepel$draw_panel(labels, panel_scales, coord)
        } else {label_grob <- ggplot2::GeomLabel$draw_panel(labels, panel_scales, coord)}
      } else {
        if(repel){
          label_grob <- ggrepel::GeomTextRepel$draw_panel(labels, panel_scales, coord)
        } else{label_grob <- ggplot2::GeomText$draw_panel(labels, panel_scales, coord)}
      }

  }

    ggplot2:::ggname("geom_net", grobTree(
      edges_draw,
      selfies_draw,
      selfies_arrows,
      GeomPoint$draw_panel(vertices, panel_scales, coord),
      label_grob
    ))
  }
)


theme_net <- function (base_size = 11, base_family = "")
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), unit="cm"),

      panel.background = element_blank(),

      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),

      legend.key = element_rect(fill = NA, colour = "white"),

      panel.border = element_blank(),
      panel.grid = element_blank(),

      aspect.ratio = 1
    )
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


StatNet <- ggplot2::ggproto("StatNet", ggplot2::Stat,
  required_aes = c("from_id", "to_id"),
  non_missing_aes = "weight",
  setup_params = function(data, params) {
    params
  },


  setup_data = function(self, data, params) {

    fiteach=params$fiteach

    if (!is.null(params$vertices)) {
      data <- merge(data, params$vertices, by.x="from_id", by.y="label", all=TRUE)
    }
    levels <- levels(as.factor(data$to_id))
    data$to_id <- as.character(data$to_id)
    data$.selfie <- as.character(data$from_id) == data$to_id
    data$to_id[is.na(data$to_id)] <- as.character(data$from_id)[is.na(data$to_id)]
    only_to <- setdiff(levels(data$to_id), levels(data$from_id))
    only_to <- setdiff(only_to, "..NA..")
    if (length(only_to) > 0)
      warning(sprintf("There are %d nodes without node information: %s\n\nDid you use all=T in merge?\n\n", length(only_to), paste(only_to, collapse=", ")))
    if (! is.null(params$seed)) set.seed(params$seed)
    data
  },

compute_network = function(data, layout.alg="kamadakawai", layout.par=list(), singletons = TRUE) {
  edges <- subset(data, to_id != "..NA..")[,c('from_id', 'to_id')]
  if (!singletons){
    all_singletons <- edges$from_id[edges$from_id == edges$to_id]
    true_singletons <- all_singletons[!all_singletons %in% edges$to_id[edges$from_id != edges$to_id]]
    edges <- edges[!edges$from_id %in% true_singletons,]
  }
  edges <- dplyr::group_by(edges, from_id, to_id)
  edges <- dplyr::summarise(edges, wt = n())
  if (any(is.na(edges$from_id))) message(sprintf("%d missing values excluded\n", sum(is.na(edges$from_id))))
  net <- network::as.network(na.omit(edges[,1:2]), matrix.type = "edgelist")
  edgeweights <- diff(range(edges$wt)) != 0
  if (edgeweights) {
    edgelist <- sna::as.edgelist.sna(net) #sna pkg
    edgelist[,3] <- sqrt(edges$wt)  # doesn't change anything for wt == const
  } else {
    edgelist <- sna::as.edgelist.sna(net) # switched from network to sna for consistency
  }


  if (is.null(layout.alg)) {
    if (is.null(data$x) || is.null(data$y)) stop("If no layout mechanism is specified, x and y coordinates have to be given\n\n")
    vert.coord <- data[, c("x", "y", "from_id")]
    vert.coord <- subset(vert.coord, from_id %in% attr(edgelist, "vnames"))
    vert.coord <- unique(vert.coord)
#    vert.coord$x <- as.numeric(scale(vert.coord$x, center=min(vert.coord$x), scale=diff(range(vert.coord$x))))
#    vert.coord$y <- as.numeric(scale(vert.coord$y, center=min(vert.coord$y), scale=diff(range(vert.coord$y))))
    names(vert.coord)[3] <- "label"
  } else {
  #print("it would be nice at this point to check, whether layout is one of the supported functions, and if not,
# browser()
  layoutFun <- getFromNamespace(paste('gplot.layout.',layout.alg,sep=''), asNamespace("sna"))
#  requireNamespace("sna")
#  layoutFun <- paste('gplot.layout.',layout.alg,sep='')
#  vert.coord <- data.frame(do.call(layoutFun, list(edgelist, layout.par = layout.par)))
  vert.coord <- data.frame(layoutFun(edgelist, layout.par = layout.par))

  vert.coord$label <- attr(edgelist, "vnames") #row.names(m)
  vert.coord$X1 <- as.numeric(scale(vert.coord$X1, center=min(vert.coord$X1), scale=diff(range(vert.coord$X1)))) # center nodes
  vert.coord$X2 <- as.numeric(scale(vert.coord$X2, center=min(vert.coord$X2), scale=diff(range(vert.coord$X2))))
  names(vert.coord) <- c("x", "y", "label")
}

  edge.coord <- data.frame(vert.coord[edgelist[,1],], vert.coord[edgelist[,2],], row.names=NULL)
  names(edge.coord) <- c('x','y', "from", 'xend','yend', "to")

  relVars <- setdiff(names(data), c("x", "y"))
  fromto <- subset(data, to_id != "..NA..")[,relVars]
  edges <- merge(edge.coord, fromto, by.x=c("from", "to"), by.y=c("from_id", "to_id"), all=TRUE)

  fromonly <- subset(data, to_id == "..NA..")[,relVars]
  if (nrow(fromonly) > 0) {
    fromonly <- merge(fromonly, edge.coord[,c("xend", "yend", "to")], by.x = "from_id", by.y="to", all.x=T)
    fromonly <- transform(fromonly,
                          from = from_id, to=to_id,
                          x=xend, y=yend, xend=NA, yend=NA)

    edges <- rbind(edges, fromonly[, names(edges)])
  }

  edges <- mutate(group_by(edges, from, to), weight = n())
  unique(edges)
},

compute_panel = function(self, data, scales, na.rm = FALSE,
                         layout.alg="kamadakawai", layout.par=list(), singletons=TRUE,
                         fiteach=FALSE, vertices=NULL) {
#  cat("compute_panel in stat_net\n")
#  browser()
#    if (fiteach)
      data <- self$compute_network(data, layout.alg =layout.alg, layout.par=layout.par, singletons=singletons)

    #    data <- plyr::ddply(data, "group", plyr::mutate, .samegroup = to %in% unique(from))
    if (any(data$group) != -1) {
      data <- mutate(group_by(data, group), .samegroup = to %in% unique(from))
    }

#    browser()
    data.frame(data)
  },

compute_layer = function(self, data, params, layout, na.rm = FALSE, layout.alg,
                        # layout="kamadakawai", layout.par=list(),
                         fiteach=FALSE,
                         vertices=NULL) {
#  cat("compute_layer in stat_net\n")
#  #browser()

  if (params$fiteach) {
    # only do this plyr statement in the case that fiteach is true.
    plyr::ddply(data, "PANEL", function(data) {
      if (ggplot2:::empty(data)) return(data.frame())
#browser()
      scales <- #ggplot2:::Layout$get_scales(data$PANEL[1])
        list(x = NULL, y = NULL)
      self$compute_panel(data = data, scales = scales,
                         na.rm=params$na.rm, layout.alg=params$layout.alg,
                         layout.par=params$layout.par, fiteach=params$fiteach,
                         vertices=params$vertices, singletons=params$singletons)
    })
  }
  else {
    if (ggplot2:::empty(data)) return(data.frame())
#browser()
    scales <- list(x = NULL, y = NULL)
    #scales <- ggplot2:::Layout$get_scales(data$PANEL[1])
    self$compute_panel(data = data, scales = scales,
                       na.rm=params$na.rm, layout.alg=params$layout.alg,
                       layout.par=params$layout.par, fiteach=params$fiteach,
                       vertices=params$vertices, singletons=params$singletons)
  }
})

stat_net <- function(mapping = NULL, data = NULL, geom = "net",
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, layout.alg="kamadakawai", layout.par=list(), fiteach=FALSE, vertices=NULL, singletons = TRUE, 
                     na.rm=FALSE, ...) {
# #browser()
    ggplot2::layer(
    stat = StatNet, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(layout.alg=layout.alg, layout.par=layout.par, fiteach=fiteach, singletons=singletons,
                  na.rm=na.rm, vertices=vertices, ...
    )
  )
}
