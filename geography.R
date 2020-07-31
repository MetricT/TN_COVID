################################################################################
###  Some predefined geography to make map-making easier
################################################################################

### Counties in the Nashville Metropolitan Statistical Area
nashville_msa <-
  c("Cannon", "Cheatham", "Davidson", "Dickson", "Macon", "Maury", "Robertson",
    "Rutherford", "Smith", "Sumner", "Trousdale", "Williamson", "Wilson")

### Define counties for East, Middle, and West Tennessee
east_tn <- c(
  "Anderson",  "Bledsoe",   "Blount",     "Bradley",   "Campbell", "Carter",
  "Claiborne", "Cocke",     "Cumberland", "Grainger",  "Greene",   "Hamblen",
  "Hamilton",  "Hancock",   "Hawkins",    "Jefferson", "Johnson",  "Knox",
  "Loudon",    "McMinn",    "Marion",     "Meigs",     "Monroe",   "Morgan",
  "Polk",      "Rhea",      "Roane",      "Scott",     "Sevier",   "Sullivan",
  "Unicoi",    "Union",     "Washington")

middle_tn <- c(
  "Bedford",    "Cannon",  "Cheatham",  "Clay",       "Coffee",    "Davidson",
  "DeKalb",     "Dickson", "Fentress",  "Franklin",   "Giles",     "Grundy",
  "Hickman",    "Houston", "Humphreys", "Jackson",    "Lawrence",  "Lewis",
  "Lincoln",    "Macon",   "Marshall",  "Maury",      "Montgomery", "Moore",
  "Overton",    "Perry",   "Pickett",   "Putnam",     "Robertson", "Rutherford",
  "Sequatchie", "Smith",   "Stewart",   "Sumner",     "Trousdale", "Van Buren",
  "Warren",     "Wayne",   "White",     "Williamson", "Wilson")

west_tn <- c(
  "Benton",     "Carroll", "Chester",    "Crockett", "Decatur", "Dyer",
  "Fayette",    "Gibson",  "Hardeman",   "Hardin",   "Haywood", "Henderson",
  "Henry",      "Lake",    "Lauderdale", "Madison",  "McNairy", "Obion",
  "Shelby",     "Tipton",  "Weakley")

cheatham_adjacent <-
  c("Cheatham", "Robertson", "Davidson", "Williamson", "Dickson", "Montgomery")

cheatham_superset <-
  c(cheatham_adjacent, "Rutherford", "Wilson", "Sumner", "Stewart", "Houston",
    "Humphreys", "Hickman", "Maury")

#graph_counties <- middle_tn
#map_counties <- middle_tn
#location <- "Middle TN"

#graph_counties <- all_counties
#map_counties <- graph_counties
#location <- "Tennessee ex 'Other States/Counties'"
