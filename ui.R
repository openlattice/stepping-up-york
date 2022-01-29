# Define UI for application
tagList(
  useShinyjs(),
  navbarPage(
    title =
      div(
        img(src = "logomark.png",
            class = "logo"),
        "York County | Stepping Up Dashboard",
        class = "navbar-title"
      ), id = "tabs",
    theme = "custom.css",
    fluid = FALSE,
    windowTitle = "Stepping Up",
    collapsible = TRUE,
    header = tags$head(
      tags$style(HTML(
        "#page-nav > li:first-child { display: none; }"
      )),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Source+Sans+Pro:200,300,400,600,700"),
      tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js")
    ),
    home_ui(),
    jail_ui("jail")
    # dictionaries_ui("dictionaries")
  )
)