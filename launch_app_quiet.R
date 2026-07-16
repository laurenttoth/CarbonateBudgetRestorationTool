# Call the Carbonate Budget Restoration Tool app without noisy file.info warngings

script_dir <- dirname(sys.frame(1)$ofile)

setwd(script_dir)

withCallingHandlers(
  {
    shiny::runApp()
  },
  warning = function(w) {
    if (grepl("(cannot resolve (owner|group))|(masked from)|(built under)", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  }
)