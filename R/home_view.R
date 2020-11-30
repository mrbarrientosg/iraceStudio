HomeView <- R6::R6Class(
  classname = "HomeView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header", h2("Home")),
        fluidRow(
          bs4Card(
            title = NULL,
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            strong("Welcome to Irace Studio!"),
            p("This software is an interface to execute and visualize irace data."),
            p("The irace package is an algorithm configuration tool currently available as an R package:"),
            HTML("<center><a href= \"https://iridia.ulb.ac.be/irace/\" target=\"_blank\">https://iridia.ulb.ac.be/irace/</a></center>"),
            p(""),
            HTML("<b>Important</b>: This is development version. If you have any problem, comment, or suggestion about his software, 
                 please contact us! we appreacite your input.<br>"),
            HTML("<center><a href= \"https://groups.google.com/d/forum/irace-package\" target=\"_blank\">The irace package Google group</a></center>"),
            HTML("<center><a href= \"https://github.com/mrbarrientosg/iraceStudio\" target=\"_blank\">Irace Studio Github repository</a></center>"),
            HTML("<br>How to start?<br>
                 <ol>
                 <li> Create a new scenario or load it from scenario files: go to the <b>scenario menu</b> to start the set up<br>
                    For a basic set up:
                    <ul>
                    <li>set the irace options for your scenario</li>
                    <li>add the definition for the parameters of your algorithm</li>
                    <li>add/create a target runner script for your algorithm
                    <li>add a set of training instances</li>

                    </ul>
                 </li>
                 <li> Execute your scenario: go to the <b>execution menu</b> to start and monitor an execution</li>
                 <li> Check your execution general report: go to the <b>report menu</b> to have access to the report </li>
                 <li> Explore the configurations results: go to the <b> visualization menu</b> to explore the configuration data</li>
                 </ol>")
          )
        )
      )
    },

    server = function(input, output, session, store) {


    }
  )
)