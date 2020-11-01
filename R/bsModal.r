bsModal <- function(id, title, trigger, ..., size, footer = NULL, close.button = TRUE, width = NULL) {
  if(!missing(size)) {
    if(size == "large") {
      size = "modal-lg"
    } else if(size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
    width = NULL
  } else {
    size <- "modal-dialog"
  }
  
  if(is.null(footer)) {
    footer <- tagList()
  } 
  
  if(close.button) {
    footer <- shiny::tagAppendChild(footer, tagList(shiny::tags$button(type = "button", class = "btn btn-default", "data-dismiss" = "modal", "Close")))
  }
  
  bsTag <- shiny::tags$div(class = size, 
                  shiny::tags$div(class = "modal-content", 
                                  shiny::tags$div(class = "modal-header",
                                                  shiny::tags$button(type = "button", class = "close", "data-dismiss" = "modal", shiny::tags$span(shiny::HTML("&times;"))),
                                                  shiny::tags$h4(class = "modal-title", title)
                                  ),
                                  shiny::tags$div(class = "modal-body", list(...)),
                                  shiny::tags$div(class = "modal-footer",
                                                  footer
                                  )
                  )
  )
  
  if(!is.null(width)) {
     bsTag <- addAttribs(bsTag, style = paste0("width: ", width, " !important;"))
  }
  
  bsTag <- shiny::tags$div(class = "modal sbs-modal fade", id = id, tabindex = "-1", "data-sbs-trigger" = trigger,
                           bsTag
  )
  
  htmltools::attachDependencies(bsTag, shinyBSDep)
  
}
