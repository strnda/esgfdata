library(data.table); library(fst)

pth_dta_save <- "/media/phill/Extreme SSD/aladin_postpro"
aladin_fls <- list.files(path = "/media/phill/Extreme Pro/Aladin/",
                         recursive = TRUE,
                         full.names = TRUE)

fls_split <- strsplit(x = aladin_fls,
                      split = "/")

nfo <- unique(x = as.data.table(x = t(x = sapply(X = fls_split,
                                                 FUN = function(i){
                                                   unlist(i)[length(x = fls_split[[1]]) - c(5, 3)]
                                                 }))))

for (ndx in c(1, 3)) {

  fls_xtr <- aladin_fls[grepl(nfo$V1[ndx],
                              x = aladin_fls) &
                          grepl(pattern = nfo$V2[ndx],
                                x = aladin_fls)]
  fls_split <- strsplit(x = fls_xtr,
                        split = "/")
  yr <- unique(x = as.data.table(x = sapply(X = fls_split,
                                            FUN = function(i){
                                              unlist(i)[length(x = fls_split[[1]]) - c(2)]
                                            })))

  dir.create(path = file.path(pth_dta_save,
                              paste(nfo$V1[ndx], nfo$V2[ndx],
                                    sep = "_")),
             showWarnings = FALSE,
             recursive = TRUE)

  for (x in unlist(x = yr)) {

    out_l <- lapply(
      X = fls_xtr[grep(pattern = x,
                       x = fls_xtr)],
      FUN = function(j) {

        e <- try(
          expr = {

            dta <- fread(input = j,
                         dec = ",")
            dta[, let(date = as.IDate(x = Datum,
                                      format = "%Y/%m/%d"),
                      Prvek = NULL,
                      Sum = NULL,
                      Mean = NULL,
                      Datum = NULL)]

            setnames(x = dta,
                     old = "SQ",
                     new = "cell_id")

            dta
          },
          silent = TRUE)

        if (inherits(x = e,
                     what = "try-error")) {

          return(NULL)
        } else {

          return(dta)
        }
      }
    )

    write_fst(x = rbindlist(l = out_l,
                            fill = TRUE),
              path = file.path(pth_dta_save,
                               paste(nfo$V1[ndx], nfo$V2[ndx],
                                     sep = "_"),
                               paste0(x, ".fst")))
  }
}

test <- read.fst(path = "/media/phill/Extreme SSD/aladin_postpro/SSP2-4.5_SRA/2015.fst")

################################

pth_dta_save <- "/media/phill/Extreme SSD/aladin_postpro"

yr_dta_dir <- list.dirs(path = pth_dta_save,
                        full.names = TRUE)
yr_dta_dir <- yr_dta_dir[grep(pattern = "sra",
                              ignore.case = TRUE,
                              x = yr_dta_dir)]

dur_all <- c(1, 2, 3, 6, 12, 24, 48)
# i <- 1
# dir <- yr_dta_dir[2]
# dur <- 2
for (dir in yr_dta_dir) {

  yr_fls <- list.files(path = dir,
                       pattern = "fst",
                       ignore.case = TRUE,
                       recursive = TRUE,
                       full.names = TRUE)

  MX <- list()

  for (i in seq_along(along.with = yr_fls)) {

    if (i == 1) {
      dta_aux <- read_fst(path = yr_fls[i],
                          as.data.table = TRUE)
      after_fst <- fst(path = yr_fls[i + 1])

      dta_aux <- rbindlist(l = list(dta_aux,
                                    after_fst[(nrow(x = after_fst) - 200):nrow(x = after_fst),]))
    }

    if (i == length(x = yr_fls)) {
      dta_aux <- read_fst(path = yr_fls[i],
                          as.data.table = TRUE)
      before_fst <- fst(path = yr_fls[i - 1])

      dta_aux <- rbindlist(l = list(before_fst[1:200,],
                                    dta_aux))
    }

    if (i != 1 & i != length(x = yr_fls)) {
      dta_aux <- read_fst(path = yr_fls[i],
                          as.data.table = TRUE)
      before_fst <- fst(path = yr_fls[i - 1])
      after_fst <- fst(path = yr_fls[i + 1])

      dta_aux <- rbindlist(l = list(before_fst[1:200,],
                                    dta_aux,
                                    after_fst[(nrow(x = after_fst) - 200):nrow(x = after_fst),]))
    }

    dta_aux_m <- melt(data = dta_aux,
                      id.vars = c("date", "cell_id"))
    yr <- strsplit(x =  yr_fls[i],
                   split = "/")
    yr <- gsub(pattern = "\\.fst",
               replacement = "",
               x = yr[[1]][length(x = yr[[1]])])

    MX[[i]] <- lapply(
      X = dur_all,
      FUN = function(dur) {

        e <- try(
          expr = {

            dta_aux_m[, agg := frollmean(x = value,
                                         n = dur,
                                         align = "center",
                                         na.rm = TRUE)]
            mx <- dta_aux_m[, .(mx = max(agg,
                                         na.rm = TRUE)),
                            by = .(cell_id,
                                   year(x = date))]

            out <- dcast(data = mx[year %like% yr,],
                         formula = year ~ cell_id,
                         value.var = "mx")
            out[, dur := dur]

            out
          },
          silent = TRUE
        )

        if (inherits(x = e,
                     what = "try-error")) {

          return(NULL)
        } else {

          return(out)
        }

      }
    )

    MX[[i]] <- rbindlist(l = MX[[i]])
  }

  mx <- rbindlist(l = MX)

  write_fst(x = mx,
            path = file.path(pth_dta_save,
                             paste0(gsub(pattern = pth_dta_save,
                                         replacement = "",
                                         x = dir), "_mx.fst")))
}
