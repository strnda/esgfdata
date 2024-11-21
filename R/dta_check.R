library(CoSMoS)

dta <- fread(input = "~/Desktop/aux/mx/pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1_1hr_mx.csv")

ndx <- sample(x = 3:dim(x = dta)[2],
              size = 10)

dta_plot <- dta[, c(1:2, ndx), with = FALSE]
dta_plot <- melt(data = dta_plot,
                 id.vars = 1:2)
dta_plot[, p := (rank(x = value) - .3 )/( .N + .4),
         by = .(variable, dur)]

ggplot(data = dta_plot) +
  geom_point(mapping = aes(x = p,
                           y = value,
                           colour = as.factor(x = dur))) +
  facet_wrap(facets = ~variable,
             ncol = 5,
             scales = "free")

# idf <- function(x,
#                 ndx,
#                 rp = c(2, 5, 10, 25, 50, 100),
#                 aggfun = "mean",
#                 dist = "gev", ...) {
#   # x <- dta
#
#   dur <- unique(x = x$dur)
#
#   agg <- split(x = x,
#                f = x$dur)
#
#   quant <- lapply(
#     X = agg,
#     FUN = function(a) {
#
#       para <- fitDist(data = unlist(x = a[, ndx + 2,
#                                           with = FALSE]),
#                       dist = dist,
#                       n.points = 10,
#                       norm = "N4",
#                       constrain = FALSE)
#
#       prob <- 1 - 1/rp
#
#       q <- qgev(p = prob,
#                 loc = para$loc,
#                 scale = para$scale,
#                 shape = para$shape)
#
#       names(x = q) <- rp
#
#       as.list(x = q)
#     }
#   )
#
#   names(x = quant) <- c(dur)
#
#   quant_all <- rbindlist(l = quant,
#                          idcol = "dur")
#   quant_idf <- melt(data = quant_all,
#                     id.vars = "dur",
#                     variable.name = "rp")
#
#   return(quant_idf)
# }
#
# test <- idf(x = dta,
#             ndx = 2000)
#
# ggplot(data = test,
#        mapping = aes(x = as.numeric(x = dur),
#                      y = value,
#                      colour = rp)) +
#   geom_line() +
#   geom_point() +
#   scale_colour_manual(name = "Return\nperion",
#                       values = c("yellow4", "steelblue", "red4",
#                                  "darkgreen", "pink", "magenta4")) +
#   labs(x = "Duration (hours)",
#        y = "Intensity (mm/h)",
#        title = "IDF curve") +
#   theme_bw()
