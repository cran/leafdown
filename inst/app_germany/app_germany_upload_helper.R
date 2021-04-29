
file.copy(list.files("inst/extdata/", full.names = TRUE), "inst/app_germany/")
rsconnect::deployApp(appDir = "inst/app_germany", appName = "leafdown-basic-example", lint = FALSE,
                     appPrimaryDoc = "app_germany.R")
file.remove("inst/app_germany/ger1-005.R")
file.remove("inst/app_germany/ger2-005.R")

