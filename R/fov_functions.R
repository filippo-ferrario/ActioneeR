# ===============================================================================
# Name   	: Field of View	
# Author 	: Filippo Ferrario
# Date   	: 12-07-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: functions based on Rende et al 2015 to calculate a camera field of view and footprint on the bottom of an image given a distance to it.
# 			  Based on Rende SF, Irving AD, Bacci T, Parlagreco L, Bruno F, De Filippo F, Montefalcone M, Penna M, Trabucco B, Di Mento R, et al. 2015. Advances in micro-cartography: a two-dimensional photo mosaicing technique for seagrass monitoring. Estuarine, Coastal and Shelf Science 167(November 2015): 475–486. Elsevier Ltd. doi: 10.1016/j.ecss.2015.10.029.
# ===============================================================================


#' Camera properties
#'
#' Functions to calculate a camera field of view and footprint on the bottom of an image given a distance to it.
#'
#' @description
#' 
#' `FOV()` is the eq 1 in Rende et al. 2015. It calculates the Field of View at a given distance from the bottom based on the observed (or estimated) horizontal distance in the images.  
#' `WID()` is the eq 5 in Rende et al. 2015 (the inverse of eq. 1). Given a FOV and a distance from the bottom (or a desired plane) it calculates the horizontal footprint of an image on the bottom.  
#' `rad2deg()` convert radiants to decimal degree.  
#'
#'
#' @param C Horizontal distance of the observable extent (either estimated or measured) in the scene. Numeric
#' @param d Distance of the camera from the bottom (or plane of interest)
#' @param fov Field of view in radiants, estimated.
#' @param x an angle expressed in radiants.
#' 
#'
#' @return
#'
#' All return a numerical vector.
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @references
#'
#' Rende SF, Irving AD, Bacci T, Parlagreco L, Bruno F, De Filippo F, Montefalcone M, Penna M, Trabucco B, Di Mento R, et al. 2015. Advances in micro-cartography: a two-dimensional photo mosaicing technique for seagrass monitoring. Estuarine, Coastal and Shelf Science 167(November 2015): 475–486. Elsevier Ltd. doi: 10.1016/j.ecss.2015.10.029.
#'
#' @export

# FOV is the eq 1 in Rende et al.
# C extent observable in the scene
# d distance from the camera

FOV<-function(C,d){
             fov<- 2*atan((C/(2*d)))
             return(fov)
            }

#' @export
#' @rdname FOV 
# WID is the inverse of the eq 1 and is the width in lenght of the image
# fov: field of view (radiants) estimated
# d distance from the camera
WID<- function(fov,d){
           C <- 2*d*tan(fov/2)      # http://www.youmath.it/lezioni/analisi-matematica/le-funzioni-elementari-e-le-loro-proprieta/377-arcotangente.html
           return(C)
           }

#' @export
#' @rdname FOV
# rad2g convert the radiants in degree (360°).
rad2deg<- function(x){ g<-x*(180/pi)}


