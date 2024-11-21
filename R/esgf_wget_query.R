lop <- c("data.table", "curl", "jsonlite")

to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

if(length(to_instal) != 0) {

  install.packages(to_instal)
}

temp <- lapply(X = lop,
               FUN = library,
               character.only = T)
rm(temp, to_instal, lop)

gc()

##### #####

pth <- "./data/"
server_timeout <- 60

# project <- c("CORDEX")#, "CMIP5", "CMIP6")
# variable <- c("pr", "tas")
# time_frequency <- c("1hr")
# domain <- paste0("&domain=EUR-", c("11", "11i", "22", "44", "44i"),
#                  collapse = "")
#
# to_get <- c("domain", "experiment", "ensemble", "rcm_name", "driving_model")
#
##### #####
#
# url <- paste0("https://esgf-node.llnl.gov/esg-search/search?type=Dataset&facets=*",
#               "&project=", project[1],
#               "&variable=", variable[1],
#               domain,
#               "&limit=0&format=application%2Fsolr%2Bjson")
#
# json_fetch <- fromJSON(txt = curl(url = url),
#                        flatten = TRUE)
#
# fct <- json_fetch[["facet_counts"]][["facet_fields"]]
# fct <- fct[names(x = fct) %in% to_get]
#
# fct_sub <- lapply(
#   X = fct,
#   FUN = function(x) {
#
#     if (length(x = x) == 0) {
#
#       out <- NULL
#     } else {
#
#       out <- matrix(data = unlist(x = x,
#                                   recursive = TRUE),
#                     ncol = 2,
#                     byrow = TRUE)
#       out <- as.data.table(x = out)
#       names(x = out) <- c("id", "number")
#     }
#
#     return(out)
#   }
# )
#
# meta <- rbindlist(l = fct_sub,
#                   idcol = "variable")
# meta
# meta[, .(n = sum(x = as.numeric(x = number))),
#      by = variable]
#
##### #####

wget_url <- "https://esgf-node.llnl.gov/esg-search/wget?"

# to_get_wget <- list()
#
# for (var in variable) {
#   for (ens in meta[variable == "ensemble", id]) {
#     for (exp in meta[variable == "experiment", id]) {
#       for (dom in meta[variable == "domain", id]) {
#         for (dr in meta[variable == "driving_model", id]) {
#           for (rcm in meta[variable == "rcm_name", id]) {
#
#             to_get_wget[[paste0(var, ens, exp, dom, dr, rcm)]] <- paste(wget_url,
#                                                                         "project=", project, "&",
#                                                                         "variable=", var, "&",
#                                                                         "domain=", dom, "&",
#                                                                         "rcm_name=", rcm, "&",
#                                                                         "driving_model=", dr, "&",
#                                                                         "ensemble=", ens, "&",
#                                                                         "experiment=", exp, "&",
#                                                                         "time_frequency=", time_frequency,
#                                                                         sep = "")
#
#
#           }
#         }
#       }
#     }
#   }
# }
#
# wget_all <- as.vector(do.call(what = rbind,
#                               args = to_get_wget))
#
# write.table(x = wget_all,
#             file = file.path(pth,
#                              paste0("wget_", Sys.Date(), ".txt")))

wget_all <- unlist(x = read.table(file = file.path(pth,
                                                   paste0("wget_", Sys.Date(), ".txt"))))

wget_all_l <- split(x = wget_all,
                    f = ceiling(x = seq_along(along.with = wget_all) / 100))

dir.create(path = file.path(pth, "/wget/"),
           recursive = TRUE,
           showWarnings = FALSE)

out <- lapply(
  X = wget_all_l,
  function(x) {
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
                              x = x))

    md_wget <- multi_download(urls = x,
                              destfiles = file.path(pth, "/wget/",
                                                    paste0(wget_nms,
                                                           ".sh")),
                              timeout = server_timeout,
                              resume = TRUE)

    fls <- list.files(path = file.path(pth, "wget"),
                      full.names = TRUE)

    nfo <- as.data.table(x = file.info(fls))

    file.remove(fls[which(x = nfo$size <= 42)])

    return(md_wget)
  }
)


# while (any(!chck,
#            na.rm = TRUE)) {
#
#   ndx <- which(!chck)
#   md_aux <- multi_download(urls = unlist(x = wget_all)[ndx],
#                            destfiles = file.path(pth, "/wget/",
#                                                  paste0(wget_nms[ndx],
#                                                         ".sh")),
#                            timeout = server_timeout,
#                            resume = TRUE)
#   chck <- md_aux$success
#   chck[is.na(x = chck)] <- FALSE
#   aux <- aux + 1
#
#   if (aux > download_tries) {
#
#     break
#   }
# }
