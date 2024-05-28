library(curl); library(jsonlite); library(digest); library(data.table)

pth <- "./data/dl/"
download_tries <- 15
user_creds <- "strnda:Nkrn!3Yyzm7rLDG"

project <- c("CORDEX")#, "CMIP5", "CMIP6")
variable <- c("pr", "tas")
time_frequency <- c("1hr")
domain <- paste0("&domain=EUR-", c("11", "11i", "22", "44", "44i"),
                 collapse = "")

to_get <- c("domain", "experiment", "ensemble", "rcm_name", "driving_model")

#########################

url <- paste0("https://esgf-node.llnl.gov/esg-search/search?type=Dataset&facets=*",
              "&project=", project[1],
              "&variable=", variable[1],
              domain,
              "&limit=0&format=application%2Fsolr%2Bjson")

json_fetch <- fromJSON(txt = curl(url = url),
                       flatten = TRUE)

fct <- json_fetch[["facet_counts"]][["facet_fields"]]
fct <- fct[names(x = fct) %in% to_get]

fct_sub <- lapply(
  X = fct,
  FUN = function(x) {

    if (length(x = x) == 0) {

      out <- NULL
    } else {

      out <- matrix(data = unlist(x = x,
                                  recursive = TRUE),
                    ncol = 2,
                    byrow = TRUE)
      out <- as.data.table(x = out)
      names(x = out) <- c("id", "number")
    }

    return(out)
  }
)

meta <- rbindlist(l = fct_sub,
                  idcol = "variable")
meta

# meta[, .(n = sum(x = as.numeric(x = number))),
#      by = variable]

#########################

# wget url
#
# https://esgf-node.llnl.gov/esg-search/wget?
#
# https://esgf-node.llnl.gov/esg-search/wget?project=CORDEX&variable=pr&variable=tas&domain=EUR-11&domain=EUR-11i&domain=EUR-44&domain=EUR-44i&rcm_name=ALADIN63&driving_model=MOHC-HadGEM2-ES&time_frequency=1hr

wget_url <- "https://esgf-node.llnl.gov/esg-search/wget?"

to_get_wget <- list()

for (var in variable) {
  for (ens in meta[variable == "ensemble", id]) {
    for (exp in meta[variable == "experiment", id]) {
      for (dom in meta[variable == "domain", id]) {
        for (dr in meta[variable == "driving_model", id]) {
          for (rcm in meta[variable == "rcm_name", id]) {

            to_get_wget[[paste0(var, ens, exp, dom, dr, rcm)]] <- paste(wget_url,
                                                                        "project=", project, "&",
                                                                        "variable=", var, "&",
                                                                        "domain=", dom, "&",
                                                                        "rcm_name=", rcm, "&",
                                                                        "driving_model=", dr, "&",
                                                                        "ensemble=", ens, "&",
                                                                        "experiment=", exp, "&",
                                                                        "time_frequency=", time_frequency,
                                                                        sep = "")


          }
        }
      }
    }
  }
}

wget_all <- as.vector(do.call(what = rbind,
                              args = to_get_wget))#[10000:12000]

wget_nms <- gsub(pattern = "\\&",
                 replacement = "_",
                 x = gsub(pattern = paste(wget_url,
                                          "project=",
                                          "variable=",
                                          "domain=",
                                          "rcm_name=",
                                          "driving_model=",
                                          "ensemble=",
                                          "experiment=",
                                          "time_frequency=",
                                          "\\?",
                                          sep = "|"),
                          replacement = "",
                          x = wget_all))

dir.create(path = file.path(pth, "/wget/"),
           recursive = TRUE,
           showWarnings = FALSE)

md_wget <- multi_download(urls = unlist(x = wget_all),
                          destfiles = file.path(pth, "/wget/",
                                                paste0(wget_nms,
                                                       ".sh")))

chck <- md_wget$success
aux <- 0

while ((!any(chck,
             na.rm = TRUE))) {

  ndx <- which(!chck)
  md_aux <- multi_download(urls = unlist(x = wget_all)[ndx],
                           destfiles = file.path(pth, "/wget/",
                                                 wget_nms[ndx],
                                                 ".sh"))
  chck <- md_aux$success
  aux <- aux + 1

  if (aux > download_tries) {

    break
  }
}


#########################
fls <- list.files(path = file.path(pth, "wget"),
                  full.names = TRUE)

nfo <- as.data.table(x = file.info(fls))

file.remove(fls[which(x = nfo$size <= 42)])

fls_valid <- fls[which(x = nfo$size > 821)]

log_file <- file.path(pth, "log.txt")

if (!file.exists(log_file)) {

  file.create(log_file)
}

write(x = timestamp(),
      file = log_file,
      append = TRUE)

for (i in seq_along(along.with = fls_valid)) {

  # i <- 1

  wget <- readLines(con = fls_valid[i])
  dl_dt <- wget[eval(expr = parse(text = paste(which(wget %in% c("download_files=\"$(cat <<EOF--dataset.file.url.chksum_type.chksum",
                                                                 "EOF--dataset.file.url.chksum_type.chksum")),
                                               collapse = ":")))]
  dl_dt <- dl_dt[c(-1, -length(dl_dt))]
  dl_dt <- gsub(pattern = "'",
                replacement = "",
                x = dl_dt)
  dl_dt <- sapply(X = dl_dt,
                  FUN = strsplit,
                  split = " ")

  dl <- as.data.table(x = do.call(what = rbind,
                                  args = dl_dt))

  names(x = dl) <- c("name", "url", "type", "chksum")

  dr <- unlist(x = strsplit(x = dl[1, name],
                            split = "_"),
               recursive = TRUE)
  dr <- paste0(unlist(x = dr[-length(x = dr)]),
               collapse = "_")

  dir.create(path = file.path(pth, dr),
             recursive = TRUE,
             showWarnings = FALSE)

  md <- multi_download(urls = dl$url,
                       destfiles = file.path(pth, dr, dl$name),
                       userpwd = user_creds)

  chck <- md$success
  aux <- 0

  while ((!any(chck,
               na.rm = TRUE))) {

    ndx <- which(!chck)
    md_aux <- multi_download(urls = dl$url[ndx],
                             destfiles = file.path(pth, dr, dl$name[ndx]),
                             userpwd = user_creds)
    chck <- md_aux$success
    aux <- aux + 1

    if (aux > download_tries) {

      break
    }
  }

  ####################  log  ##############################

  ifelse(test = aux > download_tries,
         yes = assign(x = "log_entry",
                      value = paste("file failed to download:", md_aux$url)),
         no = assign(x = "log_entry",
                     value = paste("sim downloaded correctly:", dr)))

  # log_entry <- paste("XXXX")
  write(x = log_entry,
        file = log_file,
        append = TRUE)

}


# digest(object = paste(pth, dr, dl$name[1],
#                       sep = "/"),
#        algo = "sha256")
# dl[1,]
