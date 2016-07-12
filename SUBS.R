
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

setGeneric("name", valueClass = "character", function(self) {
    standardGeneric("name")
})
setGeneric("currentSpeed", valueClass = "numeric", function(self) {
    standardGeneric("currentSpeed")
})
setGeneric("buoyancy", valueClass = "numeric", function(self) {
    standardGeneric("buoyancy")
})
setGeneric("dragCoeff", valueClass = "numeric", function(object) {
    standardGeneric("dragCoeff")
})
setGeneric("height", valueClass = "numeric", function(object) {
    standardGeneric("height")
})
setGeneric("lineLength", valueClass = "numeric", function(object) {
    standardGeneric("lineLength")
})
setGeneric("lineUpLoad", valueClass = "numeric", function(self) {
    standardGeneric("lineUpLoad")
})
setGeneric("lineTheta", valueClass = "numeric", function(self) {
    standardGeneric("lineTheta")
})
setGeneric("lineDownLoad", valueClass = "numeric", function(self) {
    standardGeneric("lineDownLoad")
})
setGeneric("linePhi", valueClass = "numeric", function(self) {
    standardGeneric("linePhi")
})
setGeneric("deltaY", valueClass = "numeric", function(self) {
    standardGeneric("deltaY")
})


# Generic class containing slots common to all parts of a SUBS mooring line
setClass("Element",
         slots = list(
             name         = "character", # descriptive name
             buoyancy     = "numeric",   # (N)
             currentSpeed = "numeric",   # (knots)
             lineUpLoad   = "numeric",   # mooring line load above this Element
             theta        = "numeric",   # vertical angle of lineUpLoad
             lineDownLoad = "numeric",   # mooring line load below this Element
             phi          = "numeric",   # vertical angle of lineDownLoad
             deltaY       = "numeric")   # vertical depression
)

#==================================================
# Subclasses for "Component" (A2, B3, etc.)
setClass("Component",
         slots = list(
             drag      = "numeric",   # (N)
             dragCoeff = "numeric",
             height    = "numeric"),
         contains = "Element")

setClass("A2",
         contains = "Component",
         prototype = list(
           buoyancy  = 320.00,
           dragCoeff = 75.30,
           height    = 0.70))
setClass("A2+",
         contains = "Component",
         prototype = list(
           buoyancy  = 490.00,
           dragCoeff = 75.30,
           height    = 0.70))
setClass("B3",
         contains = "Component",
         prototype = list(
           buoyancy  = 512.00,
           dragCoeff = 63.50,
           height    = 0.70))
setClass("CART",
         contains = "Component",
         prototype = list(
           buoyancy  = -111.00,
           dragCoeff = 33.40,
           height    = 0.80))
setClass("CROM",
         contains = "Component",
         prototype = list(
           buoyancy  = 320.00,
           dragCoeff = 0.47,
           height    = 0.58))
setClass("D2",
         contains = "Component",
         prototype = list(
           buoyancy  = 900.00,
           dragCoeff = 100.0,
           height    = 0.80))
setClass("Glass Float",
         contains = "Component",
         prototype = list(
           buoyancy  = 169.00,
           dragCoeff = 25.20,
           height    = 0.60))
setClass("SBE37-SMP",
         contains = "Component",
         prototype = list(
           buoyancy  = -35.00,
           dragCoeff = 3.170,
           height    = 0.00))

#==================================================
# Subclass for "Line" (chain or wire)
setClass("Line",
         slots = list(
             drag      = "numeric",   # (N)
             dragCoeff = "numeric",
             length    = "numeric"),
         contains = "Element")

setClass("1/4 In Wire",  contains = "Line")
setClass("5/16 In Wire", contains = "Line")
setClass("Chain",        contains = "Line")

setMethod("buoyancy", signature("1/4 In Wire"),  function(self)  -2.00)
setMethod("buoyancy", signature("5/16 In Wire"), function(self)  -3.00)
setMethod("buoyancy", signature("Chain"),        function(self) -23.00)

setMethod("dragCoeff", signature("1/4 In Wire"),  function(object) 3.17)
setMethod("dragCoeff", signature("5/16 In Wire"), function(object) 4.00)
setMethod("dragCoeff", signature("Chain"),        function(object) 4.00)

#==================================================
# Subclass for "Anchor" (railroad wheels, etc.)
setClass("Anchor",
         slots = list(height = "numeric"),
         contains = "Element")

setClass("Single Railroad", contains = "Component")
setClass("Dual Railroad",   contains = "Component")
setClass("50 LBS",          contains = "Component")
setClass("100 LBS",         contains = "Component")
setClass("200 LBS",         contains = "Component")
setClass("300 LBS",         contains = "Component")
setClass("500 LBS",         contains = "Component")

setMethod("buoyancy", signature("Single Railroad"), function(self) -2890.00)
setMethod("buoyancy", signature("Dual Railroad"),   function(self) -5785.00)
setMethod("buoyancy", signature("50 LBS"),          function(self)  -222.00)
setMethod("buoyancy", signature("100 LBS"),         function(self)  -444.00)
setMethod("buoyancy", signature("200 LBS"),         function(self)  -889.00)
setMethod("buoyancy", signature("300 LBS"),         function(self) -1334.00)
setMethod("buoyancy", signature("500 LBS"),         function(self) -2224.00)

setMethod("height", signature("Single Railroad"), function(object) 0.30)
setMethod("height", signature("Dual Railroad"),   function(object) 0.30)
setMethod("height", signature("50 LBS"),          function(object) 0.20)
setMethod("height", signature("100 LBS"),         function(object) 0.20)
setMethod("height", signature("200 LBS"),         function(object) 0.20)
setMethod("height", signature("300 LBS"),         function(object) 0.20)
setMethod("height", signature("500 LBS"),         function(object) 0.20)

#==================================================
# Drag calculations
setGeneric("drag", valueClass = "numeric", function(self) {
    standardGeneric("drag")
})

setMethod("drag", signature = "Component", function(self) {
    currentSpeed = self@currentSpeed * 0.514
    drag = self@dragCoeff * currentSpeed ^ 2
    return(drag)
})
setMethod("drag", signature = "Line", function(self) {
    currentSpeed = self@currentSpeed * 0.514
    drag = self@length * self@dragCoeff * currentSpeed ^ 2
    return(drag)
})
