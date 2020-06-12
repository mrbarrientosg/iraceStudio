args <- commandArgs(trailingOnly = TRUE)

if (args[1] != "") {
  library(irace, lib.loc = args[1])
} else {
  library(irace)
}

setwd(args[2])
sink(file = args[3])
parameters <- irace::readParameters(file = "parameters.txt")
scenario <- irace::readScenario(filename = "scenario.txt")
irace::irace(scenario = scenario, parameters = parameters)
q(save = "no", status = 0, runLast = TRUE)
sink()
