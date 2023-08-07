wd <- if (Sys.info()["sysname"] == "Windows") "." else
  file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
source(file.path(wd,"code","include.R"), encoding = "UTF-8")

msg("------------------ BEGIN run.R ------------------")

option_list <- list(
  optparse::make_option(c("-g", "--dg"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Init dat geo? [default= %default]"),
  optparse::make_option(c("-a", "--da"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Init dat amb? [default= %default]"),
  optparse::make_option(c("-r", "--dr"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Update/download dat riego? [default= %default]"),
  optparse::make_option(c("-v", "--dv"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Update/download dat verdor? [default= %default]"),
  optparse::make_option(c("-f", "--from"), default=toString(Sys.Date()-7),
                        type="character", metavar="character (\"YYYY-MM-DD\")",
                        help="Date range start [default= %default (1 year ago)]"),
  optparse::make_option(c("-t", "--to"), default=toString(Sys.Date()),
                        type="character", metavar="character (\"YYYY-MM-DD\")",
                        help="Date range end [default= %default (today)]"),
  optparse::make_option(c("-n", "--accounts"), default=1,
                        type="integer", metavar="integer (>= 1, <= 3)",
                        help="Number of accounts to use [default= %default]"),
  optparse::make_option(c("-i", "--di"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Init dat int? [default= %default]")
  optparse::make_option(c("-c", "--cl"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Init clusters? [default= %default]"),
  optparse::make_option(c("-m", "--md"), action="store_true", default=F,
                        metavar="logical (T or F)",
                        help="Init models? [default= %default]"),
)
opt_parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(opt_parser)

if (opt$dg) {
  DatGeo$initJardines()
}

if (opt$da) {
  DatAmb$initDatAmb()
}

if (opt$dr) {
  if (Sys.info()["nodename"] == Riego$nodename)
    Riego$initRiego()
    # Riego$updateRiego()
  else
    Riego$downloadRiego()
}

if (opt$dv) {
  if (Sys.info()["nodename"] == Verdor$nodename) {
    Verdor$updateImgInd(opt$from,opt$to,opt$accounts)
    Verdor$updateSumInd()
  } else
    Verdor$downloadSumInd()
}

if (opt$di) {
  DatInt$initDatInt()
}

if (opt$cl) {
  # Clust$initClusts()

  # Clust$initClusts(series = dflt$series, pkgs = "pdc")
  Clust$initClusts(series = dflt$serie, pkgs = "TSclust")
}

if (opt$md) {
  # ML$initResultsNone()

  ML$initResultsNone(masks = dflt$mask, ints = dflt$int, byPCA = 0L)
}

if (opt$cl && opt$md) {
  # Clust$initResultsClusts()

  # Clust$initResultsClusts(series = dflt$series, pkgs = "pdc")
  Clust$initResultsClusts(series = dflt$serie, pkgs = "TSclust")
}

msg("------------------ END run.R ------------------")
