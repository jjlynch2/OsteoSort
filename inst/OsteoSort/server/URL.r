output$URL <- renderUI({	
	HTML(paste("<p><b><h4>URLs</h4></b></p>",
	"<strong>Website: </strong>",
	a("OsteoCodeR.com", href='https://OsteoCodeR.com', target='_blank'),
	"<p></p>",
	"<strong>Live Tool: </strong>",
	a("OsteoSort.net", href='https://OsteoCodeR.com', target='_blank'),
	"<p></p>",
	"<strong>Source Code: </strong>",
	a(img(" Repository", src='github.png',width='20px'), href='https://github.com/jjlynch2/OsteoSort', target='_blank'),
	"<br><br><br><br><p></p>"
	,sep=""))
})
