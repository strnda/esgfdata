library(curl); library(rvest)

pth <- "./data/dl/"
download_tries <- 15
user_creds <- "eucp:EUCPWP3"

url <- c("https://ensemblesrt3.dmi.dk/data/EUCP/output/REU-2/MOHC/ECMWF-ERAINT/evaluation/r1i1p1/HadREM3-RA-UM10.1/x0n1v1/1hr/prsn/",
         "https://ensemblesrt3.dmi.dk/data/EUCP/output/REU-2/MOHC/ECMWF-ERAINT/evaluation/r1i1p1/HadREM3-RA-UM10.1/x0n1v1/1hr/pr/",
         "https://ensemblesrt3.dmi.dk/data/EUCP/output/REU-2/MOHC/HadGEM3-GC3.1-N512/historical/r1i1p1/HadREM3-RA-UM10.1/x0n1v1/1hr/pr/",
         "https://ensemblesrt3.dmi.dk/data/EUCP/output/REU-2/MOHC/HadGEM3-GC3.1-N512/rcp85/r1i1p1/HadREM3-RA-UM10.1/x0n1v1/1hr/pr/")

dest <- paste0(pth,
               gsub(pattern = "https://ensemblesrt3.dmi.dk/data/EUCP/output/REU-2/MOHC/",
                    replacement = "",
                    x = url))
h <- new_handle()

handle_setopt(
  handle = h,
  httpauth = 1,
  userpwd = user_creds
)

for (i in seq_along(along.with = url)) {

  # i <- 2

  con <- curl(url = url[i],
              open = "r",
              handle = h)

  fls_html <- read_html(paste0(readLines(con = con),
                               collapse = "\n"))
  fls <- fls_html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep(pattern = "\\.nc$", value = TRUE)

  urls <- paste0(url[i], fls)

  dir.create(path = dest[i],
             showWarnings = FALSE,
             recursive = TRUE)

  md <- multi_download(urls = urls,
                       destfiles = paste0(dest[i],
                                          fls),
                       userpwd = user_creds)

  chck <- md$success
  aux <- 0

  while ((any(!chck,
              na.rm = TRUE))) {

    ndx <- which(!chck)
    md_aux <- multi_download(urls = urls[ndx],
                             destfiles = paste0(dest[i],
                                                fls[ndx]),
                             userpwd = user_creds)
    chck <- md_aux$success
    aux <- aux + 1

    if (aux > download_tries) {

      break
    }
  }

  close(con = con)
}





