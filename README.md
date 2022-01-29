# stepping-up-york  
  
This code ran the Stepping Up dashboard for York Co, PA, originally made by OpenLattice, Inc. 
* Load and clean jail csvs in `read_format_data.R` 
* Authentication occurs through auth0 in `/modules/authentication.R`, which calls upon `helper_functions.R`. These should be replaced with one's authentication of choice.
