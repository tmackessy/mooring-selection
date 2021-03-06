
#==========================================================
# class Element
#
# Represents a generic body with buoyancy, drag, a mooring
# line at angle phi and a leading line at angle theta.
#
#    buoyancy  lineUpLoad             |  /
#          |   /                      |-/ theta
#          |  /                       |/
#         XXXXXX---> drag            /|
#           /                  phi  /-|
#          /                       /  |
#     lineDownLoad
#
#==========================================================

library(methods)

#==========================================================
# TODO(Mackessy-Lloyd): Get more specific details on the kinds of line
#   currently in use. Cross-sections and materials.
# TODO(Mackessy-Lloyd): What additional components should be assessed?
#   Shackles, swivels, etc.?
# TODO(Mackessy-Lloyd): Convert to R Markdown file and develop Shiny front-end.
# TODO(Mackessy-Lloyd): Migrate from plot to ggplot2. Enhance plots.
# TODO(Mackessy-Lloyd): Review class validity checks.

#==========================================================
# Generic class containing slots common to all parts of a SUBS mooring line
setClass("Element",
         slots = c(name         = "character", # descriptive name
                   buoyancy     = "numeric",   # (N)
                   lineUpLoad   = "numeric",   # line load above this Element
                   theta        = "numeric",   # vertical angle of lineUpLoad
                   lineDownLoad = "numeric",   # line load below this Element
                   phi          = "numeric",   # vertical angle of lineDownLoad
                   deltaY       = "numeric"))  # vertical depression

#==========================================================
# Subclasses for "Component" (A2, B3, etc.)
setClass("Component",
         slots = c(drag      = "numeric",   # (N)
                   dragCoeff = "numeric",
                   height    = "numeric"),  # (m)
         contains = "Element")

#----------------------------------------------------------
setClass("A2",
         contains = "Component",
         prototype = list(buoyancy  =  320.0,
                          dragCoeff =   75.3,
                          height    =    0.7))
setClass("A2+",
         contains = "Component",
         prototype = list(buoyancy  =  490.0,
                          dragCoeff =   75.3,
                          height    =    0.7))
setClass("B3",
         contains = "Component",
         prototype = list(buoyancy  =  512.0,
                          dragCoeff =   63.5,
                          height    =    0.7))
setClass("CART",
         contains = "Component",
         prototype = list(buoyancy  = -111.0,
                          dragCoeff =   33.4,
                          height    =    0.8))
setClass("CROM",
         contains = "Component",
         prototype = list(buoyancy  =  320.0,
                          dragCoeff =    0.47,
                          height    =    0.58))
setClass("D2",
         contains = "Component",
         prototype = list(buoyancy  =  900.0,
                          dragCoeff =  100.0,
                          height    =    0.8))
setClass("Glass Float",
         contains = "Component",
         prototype = list(buoyancy  =  169.0,
                          dragCoeff =   25.2,
                          height    =    0.6))
setClass("SBE37-SMP",
         contains = "Component",
         prototype = list(buoyancy  =  -35.0,
                          dragCoeff =    3.17,
                          height    =    0.0))

#==========================================================
# Subclasses for "Line" (chain or wire)
setClass("Line",
         slots = c(drag      = "numeric",   # (N)
                   dragCoeff = "numeric",
                   length    = "numeric"),  # (m)
         contains = "Element")

#----------------------------------------------------------
setClass("1/4 In Wire",
         contains = "Line",
         prototype = list(buoyancy  =  -2.0,
                          dragCoeff =   3.17))
setClass("5/16 In Wire",
         contains = "Line",
         prototype = list(buoyancy  =  -3.0,
                          dragCoeff =   4.0))
setClass("Chain",
         contains = "Line",
         prototype = list(buoyancy  = -23.0,
                          dragCoeff =   4.0))

#==========================================================
# Subclasses for "Anchor" (railroad wheels, etc.)
setClass("Anchor",
         slots = c(height = "numeric"),  # (m)
         contains = "Element")

#----------------------------------------------------------
setClass("Single Railroad",
         contains = "Anchor",
         prototype = list(buoyancy = -2890.0,
                          height   =     0.3))
setClass("Dual Railroad",
         contains = "Anchor",
         prototype = list(buoyancy = -5785.0,
                          height   =     0.3))
setClass("50 LBS",
         contains = "Anchor",
         prototype = list(buoyancy =  -222.0,
                          height   =     0.2))
setClass("100 LBS",
         contains = "Anchor",
         prototype = list(buoyancy =  -444.0,
                          height   =     0.2))
setClass("200 LBS",
         contains = "Anchor",
         prototype = list(buoyancy =  -889.0,
                          height   =     0.2))
setClass("300 LBS",
         contains = "Anchor",
         prototype = list(buoyancy = -1334.0,
                          height   =     0.2))
setClass("500 LBS",
         contains = "Anchor",
         prototype = list(buoyancy = -2224.0,
                          height   =     0.2))

#==========================================================
# Class containing slots describing environmental conditions and a list of
#   elements comprising the entire mooring system.
setClass("Mooring",
         slots = c(name         = "character", # descriptive name
                   depth        = "numeric",   # (m)
                   friction     = "numeric",   #
                   currentSpeed = "numeric",   # (knots)
                   elements     = "list"),
         prototype = list(name         = "Example SUBS System",
                          depth        = 50.0,
                          friction     = 0.3,
                          currentSpeed = seq(from = 0, to = 10, by = 0.5),
                          elements     = list(
                            new("A2", name = "A2"),
                            new("1/4 In Wire", name = "Cable", length = 11.2),
                            new("Glass Float", name = "Float"),
                            new("CART", name = "CART"),
                            new("Chain", name = "Anchor Chain", length = 4.5),
                            new("Single Railroad", name = "Anchor")
                          )
         ),
         validity = function(object) {
           errors <- character()
           
           if (length(object@name) != 1) {
             msg <- paste("Mooring must have a name.")
             errors <- c(errors, msg)
           }
           
           if (object@depth <= 0) {
             msg <- paste("Deployment depth is ",
                          object@depth,
                          ". Must be greater than zero.",
                          sep = "")
             errors <- c(errors, msg)
           }
           
           if (object@friction <= 0) {
             msg <- paste("Bottom friction is ",
                          object@friction,
                          ". Must be greater than zero.",
                          sep = "")
             errors <- c(errors, msg)
           }
           
           if_component <- c()
           if_line <- c()
           if_anchor <- c()
           for (i in 1:length(object@elements)) {
             if_component[[i]] <- is(object@elements[[i]], "Component")
             if_line[[i]]      <- is(object@elements[[i]], "Line")
             if_anchor[[i]]    <- is(object@elements[[i]], "Anchor")
           }
           if (any(if_component)) TRUE else {
             msg <- paste("Mooring must contain at least one component.")
             errors <- c(errors, msg)
           }
           if (any(if_line)) TRUE else {
             msg <- paste("Mooring must contain at least one line.")
             errors <- c(errors, msg)
           }
           if (any(if_anchor)) TRUE else {
             msg <- paste("Mooring must contain at least one anchor.")
             errors <- c(errors, msg)
           }
           
           if (length(errors) == 0) TRUE else errors
         }
)


#==========================================================
# Drag Calculation
setGeneric("drag",
           valueClass = "numeric",
           function(self) {
             standardGeneric("drag")
             }
           )
setMethod("drag",
          signature = "Component",
          function(self) {
            currentSpeed <- self@currentSpeed * 1852 / 3600 # Knots to m/s
            drag <- self@dragCoeff * currentSpeed ^ 2
            return(drag)
            }
          )
setMethod("drag",
          signature = "Line",
          function(self) {
            currentSpeed <- self@currentSpeed * 1852 / 3600 # Knots to m/s
            drag <- self@length * self@dragCoeff * currentSpeed ^ 2
            return(drag)
            }
          )

#==========================================================
# Loading & Resultant Angle Calculation
setGeneric("lineUpLoad",
           valueClass = "numeric",
           function(self) {
             standardGeneric("lineUpLoad")
             }
           )
setMethod("lineUpLoad",
          signature = "Element",
          function(self) {
            
          })
setGeneric("lineTheta",
           valueClass = "numeric",
           function(self) {
             standardGeneric("lineTheta")
             }
           )
setGeneric("lineDownLoad",
           valueClass = "numeric",
           function(self) {
             standardGeneric("lineDownLoad")
             }
           )
setGeneric("linePhi",
           valueClass = "numeric",
           function(self) {
             standardGeneric("linePhi")
             }
           )

#==========================================================
# Depression Calculation
setGeneric("deltaY",
           valueClass = "numeric",
           function(self) {
             standardGeneric("deltaY")
             }
           )
