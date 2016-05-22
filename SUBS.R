#==========================================================
# Credit to Joseph Park for his work on the original
# TRBM and SUBS classes.
# I am hoping this is a worthy "2.0".
# ~ Trevor Mackessy-Lloyd
#==========================================================

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
setGeneric("buoyancy", valueClass = "numeric", function(object) {
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

setClass("A2",          contains = "Component")
setClass("A2+",         contains = "Component")
setClass("B3",          contains = "Component")
setClass("CART",        contains = "Component")
setClass("CROM",        contains = "Component")
setClass("D2",          contains = "Component")
setClass("Glass Float", contains = "Component")
setClass("SBE37-SMP",   contains = "Component")

# Set up buoyancies for each subclass
setMethod("buoyancy", signature("A2"),          function(object)  320.00)
setMethod("buoyancy", signature("A2+"),         function(object)  490.00)
setMethod("buoyancy", signature("B3"),          function(object)  512.00)
setMethod("buoyancy", signature("CART"),        function(object) -111.00)
setMethod("buoyancy", signature("CROM"),        function(object)  320.00)
setMethod("buoyancy", signature("D2"),          function(object)  900.00)
setMethod("buoyancy", signature("Glass Float"), function(object)  169.00)
setMethod("buoyancy", signature("SBE37-SMP"),   function(object)  -35.00)

# Set up drag coefficients for each subclass
setMethod("dragCoeff", signature("A2"),          function(object)  75.30)
setMethod("dragCoeff", signature("A2+"),         function(object)  75.30)
setMethod("dragCoeff", signature("B3"),          function(object)  63.50)
setMethod("dragCoeff", signature("CART"),        function(object)  33.40)
setMethod("dragCoeff", signature("CROM"),        function(object)   0.47)
setMethod("dragCoeff", signature("D2"),          function(object) 100.00)
setMethod("dragCoeff", signature("Glass Float"), function(object)  25.20)
setMethod("dragCoeff", signature("SBE37-SMP"),   function(object)   3.17)

# Set up heights for each subclass
setMethod("height", signature("A2"),          function(object) 0.70)
setMethod("height", signature("A2+"),         function(object) 0.70)
setMethod("height", signature("B3"),          function(object) 0.70)
setMethod("height", signature("CART"),        function(object) 0.80)
setMethod("height", signature("CROM"),        function(object) 0.58)
setMethod("height", signature("D2"),          function(object) 0.80)
setMethod("height", signature("Glass Float"), function(object) 0.60)
setMethod("height", signature("SBE37-SMP"),   function(object) 0.00)

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

setMethod("buoyancy", signature("1/4 In Wire"),  function(object)  -2.00)
setMethod("buoyancy", signature("5/16 In Wire"), function(object)  -3.00)
setMethod("buoyancy", signature("Chain"),        function(object) -23.00)

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

setMethod("buoyancy", signature("Single Railroad"), function(object) -2890.00)
setMethod("buoyancy", signature("Dual Railroad"),   function(object) -5785.00)
setMethod("buoyancy", signature("50 LBS"),          function(object)  -222.00)
setMethod("buoyancy", signature("100 LBS"),         function(object)  -444.00)
setMethod("buoyancy", signature("200 LBS"),         function(object)  -889.00)
setMethod("buoyancy", signature("300 LBS"),         function(object) -1334.00)
setMethod("buoyancy", signature("500 LBS"),         function(object) -2224.00)

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
