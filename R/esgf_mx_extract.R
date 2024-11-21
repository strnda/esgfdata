library(sf); library(terra);
library(data.table); library(fst);
library(parallel)

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

dur_all <- c(1, 2, 3, 6, 12, 24, 48)

ncore <- 6

for (dir in fls_dr[12:88]) {

  # dir <- fls_dr[12]

  cz_fls <- list.files(path = dir,
                       pattern = ".nc",
                       full.names = TRUE)

  MX <- list()

  for (i in seq_along(along.with = cz_fls)) {

    # i <- 5

    if (i == 1) {
      nc <- rast(x = c(cz_fls[i],
                       cz_fls[i + 1]))
    }

    if (i == length(x = cz_fls)) {
      nc <- rast(x = c(cz_fls[i - 1],
                       cz_fls[i]))
    }

    if (i != 1 & i != length(x = cz_fls)) {
      nc <- rast(x = c(cz_fls[i - 1],
                       cz_fls[i],
                       cz_fls[i + 1]))
    }

    yr <- unique(x = year(x = time(x = rast(x = cz_fls[i]))))[1]

    MX[[i]] <- mclapply(
      X = dur_all,
      FUN = function(dur) {

        e <- try(
          expr = {

            # dur <- dur_all[5]

            if(dur == 1) {

              nc_agg <- nc
            } else {

              nc_agg <- roll(x = nc,
                             n = dur,
                             fun = mean,
                             na.rm = TRUE)
            }

            mx <- max(nc_agg[[year(x = time(x = rast(x = nc_agg))) %in% yr]],
                      na.rm = TRUE)

            out <- as.data.table(x = as.data.frame(x = mx,
                                                   cell = TRUE))
            out <- out[, let(year = yr,
                             dur = dur)]

            out_d <- dcast(data = out,
                           formula = year + dur ~ cell,
                           value.var = "max")

            return(out_d)
          },
          silent = TRUE
        )

        if (inherits(x = e,
                     what = "try-error")) {

          return(NULL)
        } else {

          return(out)
        }

      },
      mc.cores = ncore
    )

    MX[[i]] <- rbindlist(l = MX[[i]])

    # View(MX[[i]])
  }

  mx <- rbindlist(l = MX)

  mx_dir <- strsplit(x = dir,
                     split = "/")
  mx_dir <- mx_dir[[1]][length(x = mx_dir[[1]])]

  fwrite(x = mx,
         file = file.path(mx_pth,
                          paste0(mx_dir,
                                 "_mx.csv")))

  ## xy

  # cell_id <- 1:(ncol(x = mx) - 2)
  # coords <- data.table(cell_id = cell_id,
  #                      xyFromCell(object = nc,
  #                                 cell = cell_id))
  # fwrite(x = coords,
  #        file = file.path(mx_pth,
  #                         paste0(mx_dir,
  #                                "_coord.csv")))
}
