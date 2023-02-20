
Run <- function(){
  build()
  analyze()
}

build <- function(){
  outline <- readxl::read_excel(here::here("01_admin/outline.xlsx"))
  folders <- subset(outline, outline$Verb == "build")
  for (i in nrow(folders)) {
    source(here::here("03_build", folders$Folder[i], "code", "build.R"))
  }
}

analyze <- function(){
  outline <- readxl::read_excel(here::here("01_admin/outline.xlsx"))
  foldersR <- subset(outline, (outline$Verb == "analyze" & outline$Script == "R"))
  for (i in nrow(foldersR)) {
    source(here::here("04_analyze", foldersR$Folder[i], "code", "analyze.R"))
  }
  
  foldersPy <- subset(outline, (outline$Verb == "analyze" & outline$Script == "py"))
  for (i in nrow(foldersPy)) {
    reticulate::source_python(here::here("04_analyze", foldersPy$Folder[i], "code", "analyze.py"))
  }
}

Run()

# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS 12.1
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
#   [1] knitr_1.28        magrittr_2.0.1    usethis_2.0.1     devtools_2.4.2    pkgload_1.2.3    
# [6] here_1.0.1        R6_2.5.1          rlang_0.4.12      fastmap_1.1.0     tools_4.0.5      
# [11] pkgbuild_1.2.0    xfun_0.28         sessioninfo_1.2.1 cli_3.1.0         stargazer_5.2.2  
# [16] withr_2.4.2       remotes_2.3.0     htmltools_0.5.1.1 ellipsis_0.3.2    yaml_2.2.1       
# [21] rprojroot_2.0.2   digest_0.6.28     lifecycle_1.0.1   crayon_1.4.2      processx_3.5.2   
# [26] purrr_0.3.4       callr_3.7.0       fs_1.5.0          ps_1.6.0          testthat_3.1.0   
# [31] glue_1.5.0        memoise_2.0.0     cachem_1.0.5      evaluate_0.14     rmarkdown_2.11   
# [36] compiler_4.0.5    desc_1.4.0        prettyunits_1.1.1