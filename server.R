shinyServer(function(input, output, session) {
  
  cat(file = stderr(), "\nstarting session...\n")
  
  # local testing
  # jwt = "***"
  # jwt <- reactiveVal(jwt)
  
  # publish to prod (no need to run successfully locally first)
  jwt <- reactiveVal('NA')
  jwt <- callModule(authentication_server, "authentication", jwt)
  
  # SESSION DATA-----
  # Store in a convenience variable
  cdata <- session$clientData
  
  # Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)
    
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  #-------
  
  observe({
    role_check = check_role(jwt(), required_role)
    print(role_check)
    if (!is.null(role_check)){
      if (role_check == TRUE){
        callModule(jail_server, "jail")
        showTab(inputId = "tabs", target = "Jail")
      }}
  })
  
  hideTab(inputId = "tabs", target = "Jail")

  output$warningbox <- renderUI({
    role_check = check_role(jwt(), required_role)
    
    if (is.null(role_check)) {
      message = "You are currently not logged in to the OpenLattice Domain. To log in, either clear your browser cookies here or open a new tab and go to https://openlattice.com/orgs/#/, login and then refresh the dashboard."
    } else if (!role_check) {
      message = "You have insufficient permissions to see the dashboard. Please contact dashboard administrators for access."
    } else {
      return(NULL)
    }
    return(box(
      status = "warning",
      width =12,
      align = "center",
      tags$b(message)
    ))
    
  })
  
})
