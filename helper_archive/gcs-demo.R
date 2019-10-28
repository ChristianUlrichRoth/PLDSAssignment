# Setup -------------------------------------------------------------------------

#project <- "rassignment"
#zone <- "europe-west3-c"
#account_key <- "gcsAuth.json"

#Sys.setenv(GCE_AUTH_FILE = "/Users/christianroth/Documents/gitProjects/github/rassignment/gcsAuth.json")
library(googleComputeEngineR)


#gce_auth()


#Set our default global project 
gce_global_project(project)
gce_global_zone(zone)

default_project <- gce_get_project("rassignment")
default_project$name


list.files(get_dockerfolder("shiny-googleAuthRdemo"), recursive = TRUE)
# "Dockerfile"        "shiny/DESCRIPTION" "shiny/readme.md"   "shiny/server.R"    "shiny/ui.R"



vm <- gce_vm("myapp", 
             template = "shiny",
             predefined_type = "n1-standard-2",
             dynamic_image = gce_tag_container("custom-shiny-app", "rassignment"))

# delete build VM
gce_vm_delete("myapp", project = gce_get_global_project(),
              zone = gce_get_global_zone())
