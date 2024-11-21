library(fst); library(data.table)

mx_pth <- "~/Desktop/aux/aux/"

fls_mx <- list.files(path = mx_pth,
                     # pattern = "_mx",
                     full.names = TRUE)

for (i in seq_along(along.with = fls_mx)) {

  x <- fread(input = fls_mx[i])

  x_m <- melt(data = x,
              id.vars = c("year", "dur"))

  mx <- x_m[, .(mx = max(value,
                         na.rm = TRUE)),
            by = .(variable, dur, year)]

  mx <- dcast(data = mx,
              formula = year + dur ~ variable,
              value.var = "mx")

  fwrite(x = mx,
         file = gsub(pattern = "aux/aux",
                     replacement = "aux/mx",
                     x = fls_mx[i]))
}

pth_aladin <- "/media/phill/Extreme SSD/aladin_postpro"
mx_pth <- "~/Desktop/aux/mx"

fls_aladin <- list.files(path = pth_aladin,
                         pattern = ".fst")

for (i in seq_along(along.with = fls_aladin)) {

  x <- read.fst(path = file.path(pth_aladin, fls_aladin[i]),
                as.data.table = TRUE)
  setcolorder(x = x, neworder = c(1, ncol(x = x),
                                  2:(ncol(x = x) - 1)))

  fwrite(x = x,
         file = file.path(mx_pth,
                          gsub(pattern = "fst",
                               replacement = "csv",
                               x = fls_aladin[i])))
}

aladin_coo <- fread(input = "~/Desktop/aux/GRIDové body.csv")
aladin_coo <- aladin_coo[, .(LONGITUDE, LATITUDE, SQ)]
names(x = aladin_coo) <- c("lon", "lat", "cell_id")

fwrite(x = aladin_coo,
       file = "~/Desktop/aux/mx/SSP2_SRA_coord.csv")
