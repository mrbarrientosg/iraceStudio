# Set options here
# TRUE = production mode, FALSE = development mode
options(golem.app.prod = TRUE)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list = ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# reactlog::reactlog_enable()

# Run the application
run_app(debug = TRUE)
# run_irace_vizz(debug = TRUE)