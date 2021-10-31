lead <- function(){
  pave()
  build()
  analyze()
  report()
}


pave <- function(){
  hit('pave', 'preamble')
}


build <- function(){
  hit('build', 'tsukumi_admin')
  hit('build', 'MHLW_macro')
  hit('build', 'join_tsukumi')  
}


analyze <- function(){
  hit('analyze', 'RENAME')
}


report <- function(){
  hit('report', 'RENAME')
}


hit <- function(verb_name, object_name){
  if (verb_name == 'pave'){
    library_path <- "01_admin/preamble/lib"
    .libPaths(library_path)
    #source(here::here('01_admin', 
    #                  object_name, 'add_packages', 'add_packages.R'))
    source(here::here('01_admin', object_name, 'pave', 'pave.R'))
  }
  
  else if (verb_name == 'build'){
    source(here::here('03_build', object_name, 'code', 'build.R'))
  }
  
  else if (verb_name == 'analyze'){
    source(here::here('04_analyze', object_name, 'code', 'analyze.R'))
  }
  
  else if (verb_name == 'report'){
    rmarkdown::render(here::here('05_report', 
                                 object_name, 'markdown', 'report.Rmd'),
                      output_dir = here::here('05_report', 
                                              object_name, 'output')) 
  }
}


lead()
#