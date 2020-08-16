make_pdf_report <- function(store, input, volum) {
  shinybusy::show_modal_spinner(text = "Making pdf...")

  data <- environment()
  data$iraceResults <- store$iraceResults

  files <- c()

  report <- store$currentExecution$get_report()
  userData <- report$get_data()

  for (id in names(userData)) {
    tempFiles <- tempfile(fileext = c(".html", ".pdf"))
    files <- c(files, tempFiles[2])
    cat(sprintf("<head><title>%s</title></head>", userData[[id]]$title), file = tempFiles[1], fill = TRUE)
    cat(userData[[id]]$content, file = tempFiles[1], fill = TRUE, append = TRUE)
    rmarkdown::pandoc_convert(
      tempFiles[1],
      output = tempFiles[2],
      options = c(
        "-V", "pagestyle=empty",
        "-V", "geometry=margin=0.8in",
        paste0("--template=", appSys("inst/app/static/template.tex"))
      ),
      verbose = TRUE, wd = getwd()
    )
    file.remove(tempFiles[1])
  }

  reportFolder <- parseDirPath(roots = volum, input$pdf)

  # TODO: Obtener el time y el numero de ejecucion del nombre del archivo si es posible, caso contrario ocupar time.
  time <- format(Sys.time(), "%d%m%Y%H%M%S")

  if (!dir.exists(reportFolder)) {
    dir.create(reportFolder)
  }

  data$output <- reportFolder

  knit2pdf(
    input = appSys("inst/app/static/report.Rnw"),
    envir = data,
    quiet = T,
    output = paste0(reportFolder, "/irace_report.tex")
  )

  pdf_combine(
    input = c(paste0(reportFolder, "/irace_report.pdf"), files),
    output = sprintf("%s/irace_report_%s.pdf", reportFolder, time)
  )

  if (length(files) > 0) {
    file.remove(files)
  }

  unlink(paste0(reportFolder, "/figure"), recursive = TRUE)
  file.remove(paste0(reportFolder, "/irace_report.tex"))
  file.remove(paste0(reportFolder, "/irace_report.pdf"))
  shinybusy::remove_modal_spinner()
  shinyalert("Done!", "", type = "success")
}