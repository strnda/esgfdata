lop <- c("data.table", "curl", "digest")

to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

if(length(to_instal) != 0) {

  install.packages(to_instal)
}

temp <- lapply(X = lop,
               FUN = library,
               character.only = T)
rm(temp)

##### #####

pth <- "~/Desktop/dl"
download_tries <- 15
server_timeout <- 20000
user_creds <- "strnda:Nkrn!3Yyzm7rLDG"

##### #####

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

  # i <- 10

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
                       userpwd = user_creds,
                       timeout = server_timeout,
                       resume = TRUE)

  chck <- md$success
  chck[is.na(x = chck)] <- FALSE
  aux <- 0

  while (any(!chck,
             na.rm = TRUE)) {

    ndx <- which(!chck)
    md_aux <- multi_download(urls = dl$url[ndx],
                             destfiles = file.path(pth, dr, dl$name[ndx]),
                             userpwd = user_creds,
                             timeout = server_timeout,
                             resume = TRUE)
    chck <- md_aux$success
    chck[is.na(x = chck)] <- FALSE
    aux <- aux + 1

    if (aux > download_tries) {

      break
    }
  }

  md_aux <- multi_download(urls = dl$url,
                           destfiles = file.path(pth, dr, dl$name),
                           userpwd = user_creds,
                           timeout = server_timeout,
                           resume = TRUE)

  ifelse(test = aux > download_tries,
         yes = assign(x = "log_entry",
                      value = paste(Sys.time(), "file failed to download:", md_aux$url)),
         no = assign(x = "log_entry",
                     value = paste(Sys.time(), "sim downloaded correctly:", dr)))

  write(x = log_entry,
        file = log_file,
        append = TRUE)

}

write(x = timestamp(),
      file = log_file,
      append = TRUE)

# digest(object = paste(pth, dr, dl$name[1],
#                       sep = "/"),
#        algo = "sha256")
# dl[1,]
