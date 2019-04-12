library(shinydashboard)
library(plotly)
library(ggplot2)

p_list <- readRDS("p_list.rds")

sw     <- c("PyLith", "Virtual California", "CIG", "SPECFEM", "Citcom", 
            "AxiSEM", "GMT", "ASPECT", "MATLAB", "SELEN", "Ellipsis3d", "SW4", 
            "ObsPy", "Instaseis", "ConMan", "GPlate", "Mineos", " SAC ", 
            "GMSH", "FLEXWIN", "RELAX", "SEISMIC_CPML", "Burnman", " Gale ", 
            "Zenodo", "Calypso", " MAG ", "SNAC")

names(p_list) <- sw

buttons_to_remove <- list("zoom2d", "select2d", "lasso2d", "resetScale2d",
                          "hoverClosestCartesian", "hoverCompareCartesian",
                          "zoom3d", "pan3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
                          "orbitRotation", "tableRotation",
                          "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                          "sendDataToCloud",
                          "hoverClosestGl2d",
                          "hoverClosestPie",
                          "toggleHover",
                          "resetViews",
                          "toggleSpikelines")
