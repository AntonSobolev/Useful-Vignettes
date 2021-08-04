if (!require("pacman")) install.packages("pacman")
require(pacman)
p_load(data.table, dplyr, plyr, tidyr, R.utils, stringfix, stringr)
p_load(pbmcapply, beepr)
#devtools::install_github(repo = 'mlampros/fuzzywuzzyR')
#install.packages("reticulate")
Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python")
library(reticulate)
use_python("/usr/local/bin/python")
reticulate::py_config()
library(fuzzywuzzyR)
reticulate::py_discover_config(required_module = 'fuzzywuzzy')
reticulate::py_discover_config(required_module = 'Levenshtein')
reticulate::py_discover_config(required_module = 'difflib')
check_availability()
word = "new york jets"
choices = c("Atlanta Falcons", "New York Jets", "New York Giants", "Dallas Cowboys")
#------------
# processor :
#------------
init_proc = FuzzUtils$new()      # initialization of FuzzUtils class to choose a processor
PROC = init_proc$Full_process    # processor-method
PROC1 = tolower                  # base R function ( as an example for a processor )
#---------
# scorer :
#---------
init_scor = FuzzMatcher$new()    # initialization of the scorer class
SCOR = init_scor$WRATIO          # choosen scorer function
init <- FuzzExtract$new()        # Initialization of the FuzzExtract class
# init$Extract(string = word, sequence_strings = choices, processor = PROC, scorer = SCOR)
