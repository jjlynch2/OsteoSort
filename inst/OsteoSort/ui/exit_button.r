exit_button <- tabPanel(
	tags$button(type = "button", id = "exitOsteoSort", class = "increment btn btn-default", onclick = "window.close();", HTML("<i class ='fa fa-rectangle-xmark'></i>"), "Exit"),
	tags$style(type = "text/css", "#exitOsteoSort { width:100%; height:25%; vertical-align:middle; font-size:70%; background-color:#D3D3D3; color:#126a8f }")
)
