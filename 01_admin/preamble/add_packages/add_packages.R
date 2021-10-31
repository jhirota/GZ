# run this lead function manually step by step
lead <- function(){
  # set_initial_only_once() -> library_path
  add_packages() -> packages_to_install
  make_miniCRAN(library_path, packages_to_install)
}


set_initial_only_once <- function(){
  library_path <- "01_admin/preamble/lib"
  .libPaths(library_path)
  install.packages("devtools", path = library_path)
  install.packages("miniCRAN", path = library_path)
  # if error, try: Tools -> Global Options -> Packages
  # -> uncheck "User secure download method for HTTP"
  return(library_path)
}


add_packages <- function(){
  packages <- c("tidyverse",
            "rmarkdown",
            "here",
            "kableExtra",
            "tinytex",
            "conflicted",
            "devtools",
            "miniCRAN")
  return(packages)
}


make_miniCRAN <- function(library_path, packages){
  CRAN_mirror_site_in_Japan <- "https://cran.ism.ac.jp/"
  repo_CRAN <- c(CRAN = CRAN_mirror_site_in_Japan)
  packages_list <- miniCRAN::pkgDep(packages, 
                                    repos = repo_CRAN, 
                                    type = "source", 
                                    suggests = FALSE)
  dir.create(storage_path <- file.path(tempdir(), "miniCRAN"))
  miniCRAN::makeRepo(packages_list, 
                     path = library_path, 
                     repos = repo_CRAN,
                     type = "source")
}

install.packages("devtools", 
                 repos = paste0("file:", library_path),
                 type = "source")

devtools::update_packages(packages_list,
                          repos = library_path)


install.packages("here", 
                 repos = paste0("file:", library_path),
                 type = "source")

install.packages(pkgs, 
                 repos = paste0("file:///", pth),
                 type = "source")

install.packages("devtools")

devtools::update_packages

run_installpackages <- function(library_path, packages){
  install.packages("devtools", 
                   repos = paste0("file:///", storage_path),
                   type = "source")
  install.packages(packages, 
                   lib = library_path)
  
}

# after defining all functions, run step by step.
#lead()
#