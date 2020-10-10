args <- commandArgs(trailingOnly = TRUE)

if (args[1] != "") {
  library(irace, lib.loc = args[1])
} else {
  library(irace)
}

parameters_path <- file.path(args[2], "parameters.txt")
scenario_path <- file.path(args[2], "scenario.txt")

file.create(args[3])
output <- file(args[3], open = "wt")
sink(file = output)
parameters <- irace::readParameters(file = parameters_path)
scenario <- irace::readScenario(filename = scenario_path)
irace::irace(scenario = scenario, parameters = parameters)
q(save = "no", status = 0, runLast = TRUE)
sink(NULL)
