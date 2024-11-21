library(sf); library(terra); library(data.table); library(fst); library(ggplot2)

## cesta ke cropnutym souborum
cz_pth <- "/media/phill/Extreme SSD/CORDEX_1h/"

## kam ulozit maxima
mx_pth <- "~/Desktop/aux/mx/"

##### #####

dir.create(path = mx_pth,
           showWarnings = FALSE,
           recursive = TRUE)

fls_dr <- list.dirs(path = cz_pth,
                    full.names = TRUE)[-1:-2]

for (dir in fls_dr) {

  # dir <- fls_dr[1]

  cz_fls <- list.files(path = dir,
                       pattern = ".nc")
  mx_dir <- strsplit(x = dir,
                     split = "/")
  mx_dir <- mx_dir[[1]][length(x = mx_dir[[1]])]

  cdo_query <- paste("cd ", gsub(pattern = " ",
                                 replacement = "\\\\ ",
                                 x = cz_pth),

                     "&& cdo seltimestep,1",
                     file.path(mx_dir, cz_fls[1]),
                     file.path(mx_pth, "tmp_file.nc"),

                     "&& cdo invertlat ",
                     file.path(mx_pth, "tmp_file.nc"),
                     file.path(mx_pth, "tmp_file_rev.nc"),

                     "&& cdo -outputtab,lon,lat",
                     file.path(mx_pth, "tmp_file_rev.nc"), ">",
                     file.path(mx_pth, "tmp_coord.txt"))

  system(command = cdo_query)

  coo <- fread(input = file.path(mx_pth, "tmp_coord.txt"),
               header = FALSE,
               skip = 1,
               col.names = c("lon", "lat"))
  coo[, cell_id := 1:.N]
  coo <- as.data.table(x = sapply(X = coo,
                                  FUN = as.numeric))

  fwrite(x = coo,
         file = file.path(mx_pth,
                          paste0(mx_dir,
                                 "_coord.csv")))

}

file.remove(c(file.path(mx_pth, "tmp_file.nc"),
              file.path(mx_pth, "tmp_file_rev.nc"),
              file.path(mx_pth, "tmp_coord.txt")))
