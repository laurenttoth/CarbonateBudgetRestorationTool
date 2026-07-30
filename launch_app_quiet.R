# Call the Carbonate Budget Restoration Tool app without noisy file.info warnings

script_dir <- dirname(sys.frame(1)$ofile)

setwd(script_dir)

withCallingHandlers(
  {
    shiny::runApp()
  },
  warning = function(w) {
    if (grepl("(cannot resolve (owner|group))|(unknown aesthetics)", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  }
)