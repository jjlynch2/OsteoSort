toggleModal <- function(session, modalId, toggle = "toggle") {
  session$sendInputMessage(modalId, list(toggle = toggle))
}
