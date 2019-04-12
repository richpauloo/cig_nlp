dashboardPage(skin = "black", 

    # Application title
    dashboardHeader(title = tags$a(href='https://geodynamics.org/',
                                   tags$img(src='cig_logo.png', height = "30px", width = "200px")
                                   )),
        

    # Sidebar with a radio button input for all software
    dashboardSidebar(
            radioButtons("input_sw",
                         "Select a Software:",
                         choices = sw,
                         selected = sw[1])
    ),

        # Two tab panels for the plot and about.md
        dashboardBody(
            tags$head(
                tags$style(HTML("
                         blockquote {
                         padding: 10px 20px;
                         margin: 0 0 20px;
                         font-size: 13px;
                         border-left: 5px solid #eee;
                         }
                         "))),
            fluidRow(
                tabBox(width = 12, height = NULL,
                       
                    tabPanel("Explore",
                        plotlyOutput("output_plot", width = "100%", height = "750px")
                    ),
                    
                    tabPanel("About",
                        includeMarkdown("about.md")
                    )
                )
            )
        )
)

