# here you can write your own authentication function:
# i.e. who has access to the basics of the app?
#
# This checks whether the user has the role
# "SU_dashboard" (in the York Jail org)
#
# You can add more authentication and data reads down
# in the app as well.

custom_auth_function <- function(jwt) {
  role <-  "SU_dashboard"
  basepath = "https://api.openlattice.com"
  header_params <-
    unlist(list("Authorization" = paste("Bearer", jwt)))
  client <- ApiClient$new(defaultHeaders = header_params,
                          basePath = basepath)
  prinApi <- PrincipalApi$new(apiClient = client)
  all_roles = prinApi$get_current_roles()$title
  
  return(role %in% all_roles)
}


redirect <- function(url) {
  loginurl <- paste0('window.location.replace("',
                     url,
                     '");')
  runjs(loginurl)
  
}

################
## JS STRINGS ##
################

get_cookies <- '
var ze_cookies = Cookies.get();
Shiny.onInputChange("authentication-cookies", ze_cookies);
'

#######################
## SERVER COMPONENTS ##
#######################

authentication_server <-
  function(input,
           output,
           session,
           jwt,
           role) {
    ns <- session$ns
    
    
    # read cookies into session-data
    runjs(get_cookies)
    
    observeEvent(input$cookies, {
      
      # define loginurl (redirect to go login)
      loginurl <- URLencode(
        paste0(
          "https://openlattice.com/login/?redirectUrl=https://",
          session$clientData$url_hostname,
          "?app=",
          session$clientData$url_pathname
        )
      )
      
      # define baseurl (redirect to strip url hash)
      redirectbackurl <- URLencode(
        paste0(
          "https://",
          session$clientData$url_hostname,
          session$clientData$url_pathname
        )
      )
      
      # parse hash in url
      query <-
        parseQueryString(session$clientData$url_hash_initial)
      
      
      if ("id_token" %in% names(query)) {
        
        # PHASE 1: Token in url hash
        
        # parse hash in url
        cookiedomain = paste0(
          strsplit(session$clientData$url_hostname, ".", fixed = TRUE)[[1]][-1],
          collapse = "."
        )
        
        local_jwt <- query[['id_token']]
        setcookiecmd = paste(
          'var inFifteenMinutes = new Date(new Date().getTime() + 15 * 60 * 1000);',
          "Cookies.set('authorization', ",
          paste0("'Bearer ", local_jwt, "', {"),
          "expires: inFifteenMinutes,",
          paste0("domain: '", cookiedomain, "',"),
          "path: '/',",
          "SameSite: 'strict',",
          "secure: true",
          "});",
          sep="\n"
        )
        
        runjs(setcookiecmd)
        redirect(redirectbackurl)
        
      } else if ("authorization" %in% names(input$cookies)) {
        
        # PHASE 2: Token in cookie (can be from other app)
        
        local_jwt = str_replace(input$cookies$authorization, "Bearer ", "")
        
      } else {
        
        # PHASE 0: Need for authentication
        
        local_jwt = ""
        redirect(loginurl)
        
      }
      
      jwt(local_jwt)
      
      # at this point jwt should be set !
      
      # PHASE 3
      
      runjs("console.log('login successful');")
    })
    
    return(jwt)
  }