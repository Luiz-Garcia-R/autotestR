# Declare global variables to avoid NOTE: no visible binding
# ---------------------------------------------------------------------------------
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # Internal variables
    "grupo", "id", "ind", "letra", "n", "p adj", "p.adj", "P.adj", "p_adj",
    "prop", "valor", "values",

    # Standard variables
    "group", "value"
  ))
}

