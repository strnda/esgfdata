library(sf); library(terra);

## cesta ke stazenym datum
fls_pth <- "/media/phill/Extreme Pro/data_download/"
fls_pth <- gsub(pattern = " ",
                replacement = "\\\\ ",
                x = fls_pth)

## symbolicky link kvuli moznym mezeram
symlink <- "~/Desktop/aux/"

## kam ulozit crop pro cz
cz_pth <- "~/Desktop/aux/cz/"

system(command = paste("ln -s", fls_pth, symlink))

##### #####

cz <- st_read(dsn = "./data/cz.shp")
cz_aux <- st_transform(x = cz,
                       crs = 4326)
lon_lat <- paste(x = round(x = as.vector(x = ext(x = cz_aux)),
                           digits = 2),
                 collapse = ",")

dir.create(path = cz_pth,
           showWarnings = FALSE,
           recursive = TRUE)

pth_aux <- strsplit(x = fls_pth,
                    split = "/")

fls <- list.files(path = file.path(symlink, pth_aux[[1]][length(x = pth_aux[[1]])]),
                  pattern = "\\.nc",
                  ignore.case = TRUE,
                  recursive = TRUE,
                  full.names = TRUE)

fls_dr <- strsplit(x = fls,
                   split = "/")
fls_dr <- lapply(X = fls_dr,
                 FUN = function(x) {
                   ndx <- length(x = x)
                   x[ndx]})
fls_dr <- lapply(X = fls_dr,
                 FUN = "[[",
                 index = length(x = fls_dr[[1]]))
fls_dr <- sapply(X = fls_dr,
                 FUN = strsplit,
                 split = "_")
fls_dr <- sapply(X = fls_dr,
                 FUN = function(x) {
                   ndx <- length(x = x)
                   paste(x[-ndx],
                         collapse = "_")})
fls_dr <- gsub(pattern = "CZ_",
               replacement = "",
               ignore.case = TRUE,
               x = unique(x = fls_dr))

for (j in seq_along(fls_dr)) {

  # j <- 20

  fls_cz_crop <- fls[grep(pattern = fls_dr[j],
                          x = fls)]

  dir.create(path = file.path(cz_pth, fls_dr[j]),
             showWarnings = FALSE,
             recursive = TRUE)

  for (i in seq_along(along.with = fls_cz_crop)) {

    # i <- 1
    e <- try(
      expr = {

        nc <- rast(x = file.path(fls_cz_crop[i]))

        fl_out <- strsplit(x = fls_cz_crop[i],
                           split = "/")
        fl_out <- fl_out[[1]][length(x = fl_out[[1]])]
        cdo_run <- paste(paste0("cdo sellonlatbox,",
                                lon_lat),
                         fls_cz_crop[i],
                         file.path(cz_pth, fls_dr[j],
                                   paste0("cz_",
                                          fl_out)))
        print(x = fl_out)
        system(command = cdo_run)
      },
      silent = TRUE
    )
  }
}










