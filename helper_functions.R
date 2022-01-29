transform_object_to_tibble <- function(data){
  unlisted = lapply(data, function(x) x$toJSON() %>% unlist())
  data = as_tibble(invert_list(unlisted)) %>% mutate_all(unlist)
  return(data)
}
# count = dataApi$get_entity_set_size(entsetid)

# somewhat repetitive of function above
read_table <- function(jwt, datasetname){
  entitySetsApi <- get_api(jwt, EntitySetsApi)
  dataApi <- get_api(jwt, DataApi)
  entsetid = entitySetsApi$get_entity_set_id(datasetname)
  entset = dataApi$load_entity_set_data(entsetid) %>% lapply(function(x) {
    x <- unlist(x)})
  unlisted = lapply(entset, function(x) x %>% unlist())
  data = as_tibble(invert_list(unlisted)) %>% mutate_all(unlist)
  return(data)
}

invert_list <-  function(ll) { # @Josh O'Brien
  nms <- unique(unlist(lapply(ll, function(X) names(X))))
  ll <- lapply(ll, function(X) setNames(X[nms], nms))
  ll <- apply(do.call(rbind, ll), 2, as.list)
  lapply(ll, function(X) X[!sapply(X, is.null)])
}

# Load appropriate APIs. For use in following functions.
get_api <- function(jwt, api, local = FALSE) {
  if (local) {
    basepath = "http://localhost:8080"
  } else {
    basepath = "https://api.openlattice.com"
  }
  header_params = unlist(list("Authorization" = paste("Bearer", jwt)))
  client <- ApiClient$new(defaultHeaders = header_params,
                          basePath = basepath)
  thisApi <- api$new(apiClient = client)
  return(thisApi)
}

check_role <- function(jwt, role) {
  principalApi <- get_api(jwt, PrincipalApi)
  all_roles = principalApi$get_current_roles()
  if (typeof(all_roles) == "environment"){return(NULL)}
  role_titles = transform_object_to_tibble(all_roles) %>% pull(title)
  return(role %in% role_titles)
}