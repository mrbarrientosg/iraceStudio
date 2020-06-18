Body <- R6::R6Class(
  classname = "Body",
  public = list(
    uiOptionsView = NULL,
    playgroundView = NULL,
    iraceOptionsView = NULL,
    parametersView = NULL,
    trainInstancesView = NULL,
    targetScriptsView = NULL,
    initialConfigurationsView = NULL,
    forbiddenView = NULL,
    testingView = NULL,
    
    iraceOutputView = NULL,
    executionsHistoryView = NULL,
    
    sandboxView = NULL,
    performanceConfig = NULL,
    
    reportView = NULL,
    userSectionView = NULL,
    
    initialize = function() {
      self$uiOptionsView <- UIOptionsView$new("ui_options")
      self$playgroundView <- PlaygroundView$new("playground")
      self$iraceOptionsView <- IraceOptionsView$new("scenario_irace_options")
      self$parametersView <- ParametersView$new("scenario_parameters")
      self$trainInstancesView <- TrainInstancesView$new("scenario_train_instances")
      self$targetScriptsView <- TargetScriptsView$new("scenario_target_scripts")
      self$initialConfigurationsView <- InitialConfigurationsView$new("scenario_initial_configurations")
      self$forbiddenView <- ForbiddenView$new("scenario_forbidden")
      self$testingView <- TestingView$new("scenario_testing")
      self$iraceOutputView <- IraceOutputView$new("execution_irace_output")
      self$executionsHistoryView <- ExecutionsHistoryView$new("execution_history")
      
      self$sandboxView <- SandboxView$new("visualization_sandbox")
      # self$performanceConfig <- PerformanceConfig$new("visualization_perf_config")
      
      self$reportView <- ReportView$new("report")
      self$userSectionView <- UserSectionView$new("report_user_section")
    },
    
    ui = function() {
      bs4Dash::bs4DashBody(
        add_external_resources(),
        bs4TabItems(
          bs4TabItem(
            tabName = "ui_options",
            self$uiOptionsView$ui()
          ),
          bs4TabItem(
            tabName = "playground",
            self$playgroundView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_irace_options",
            self$iraceOptionsView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_parameters",
            self$parametersView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_train_instances",
            self$trainInstancesView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_target_scripts",
            self$targetScriptsView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_initial_configurations",
            self$initialConfigurationsView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_forbidden",
            self$forbiddenView$ui()
          ),
          bs4TabItem(
            tabName = "scenario_testing",
            self$testingView$ui()
          ),
          bs4TabItem(
            tabName = "execution_irace_output",
            self$iraceOutputView$ui()
          ),
          bs4TabItem(
            tabName = "execution_history",
            self$executionsHistoryView$ui()
          ),
          bs4TabItem(
            tabName = "visualization_sandbox",
            self$sandboxView$ui()
          ),
          bs4TabItem(
            tabName = "visualization_perf_config",
            # self$performanceConfig$ui()
          ),
          bs4TabItem(
            tabName = "report",
            self$reportView$ui()
          ),
          bs4TabItem(
            tabName = "report_user_section",
            self$userSectionView$ui()
          )
        )
      )
    },
    
    setupModules = function(store) {
      self$uiOptionsView$call(store = store)
      self$playgroundView$call(store = store)
      self$iraceOptionsView$call(store = store)
      self$parametersView$call(store = store)
      self$trainInstancesView$call(store = store)
      self$targetScriptsView$call(store = store)
      self$initialConfigurationsView$call(store = store)
      self$forbiddenView$call(store = store)
      self$testingView$call(store = store)
      
      self$iraceOutputView$call(store = store)
      self$executionsHistoryView$call(store = store)
      
      self$sandboxView$call(store = store)
      #self$performanceConfig$call(store = store)
      
      self$reportView$call(store = store)
      self$userSectionView$call(store = store)
    }
  )
)