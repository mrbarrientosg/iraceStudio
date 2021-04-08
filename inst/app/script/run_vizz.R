args <- commandArgs(trailingOnly = TRUE)
library(iraceStudio)

iraceStudio::run_irace_vizz(data = args[1], port = NULL)
q(save = "no", status = 0, runLast = TRUE)
