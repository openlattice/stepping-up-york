home_ui <- function(id) {
  tabPanel(title = 'Home',
           fluidRow(
             uiOutput("warningbox"),
             box(width = 12,
                 column( 12, br(),br(),
                         # When need to see session values
                         # h3("clientData values"),
                         # verbatimTextOutput("clientdataText"),
                         HTML('<center><img src="stepup_logo-100.png" width="325px"></center>'),
                         br(),
                         HTML('<center><img src="york_logo.png" width="250px"></center>'),
                         
                         # column( 7, offset = 1,
                         br(),
                         tags$a(href = "https://stepuptogether.org/", "Stepping Up"),
                         'is a National Initiative to reduce the number of people with mental illnesses in jails.
               The following are select metrics from the Stepping Up
               Initiative, made in conjunction with',
                         tags$a(href = "https://openlattice.com", "OpenLattice."),
                         br(),
                         br(),
                         # 'Each tab contains data related to its respective organization.'
                 )
                 # )
             )
           ))
}
