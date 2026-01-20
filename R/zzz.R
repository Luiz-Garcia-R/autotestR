.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    crayon::green("autotestR "), "loaded successfully!\n",
    "----------------------------------------------------------\n",
    "A package designed to simplify statistical analyses\n",
    "with accessible language, automatic diagnostics, and clear graphics.\n",
    "\n",
    "Type ", crayon::green("?autotestR"), " to see a general introduction\n",
    "Type a test function name without parameters (", crayon::bold("e.g. test.t()"),
    ") to see a brief description of the test\n",
    "Official website and updates: https://github.com/Luiz-Garcia-R/autotestR.git\n",
    "----------------------------------------------------------\n"
  )
}
