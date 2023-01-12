# ===============================================================================
# Name   	: 
# Author 	: 
# Date   	:  [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: 
# ===============================================================================

#' Calculate the overlap between rig swaths
#' 
#' Calculate the overlap between rig swaths at a given distance between swaths centers.  
#' The overlap is expressed relative to the swath of the external camera on the rig: if >= 1 then the external camera is completly overalpped (not necessairily by only one camera on the opposite rig swath).
#' 
#' @param swath_width swath of a rig. E.g., as given by [swath_rig] 
#' @param dist_centers distance between the centers of the two swaths
#' @param external_cam_swath swath of the external camera of the rig (i.e., the one that will overlap with the other swath). E.g., as given by [WID] 
#' 
#' @details
#' Overlap as the ratio of the $(width of ovelap between rig swaths)/(swath of the external camera of the rig)
#' Thus the overlap is expressed relative to the swath of the external camera on the rig: 
#' * if >= 1 then the external camera is completly overalpped (not necessairily by only one camera on the opposite rig swath).
#' * if <0 then NA is returned because No overlap possible; a warning is given 
#' * if 0< <=1 that is the proportion of the esternal camea to overlap with the other swath.
#' 
#' @export  

rig_swaths_overlap<- function(swath_width, dist_centers, external_cam_swath ){
				cam_swath <-external_cam_swath
				hlf_sw<-swath_width/2
				# complementary distance of the half swath  
				c_hlf_sw<-dist_centers-hlf_sw
				# overalp relative to the camera swath: if >= 1 then the distal camera is completly overalpped (not necessairily by only one camera on the opposite rig swath)
				out<-(hlf_sw-c_hlf_sw)/cam_swath
				if(out<0) {
					out<-NA
					warning('No overlap possible!')
					}
				out

		}
	
