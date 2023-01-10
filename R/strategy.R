# ===============================================================================
# Name      :  
# Author    : Filippo Ferrario
# Date      : 19-07-2022 [dd-mm-yyyy]
# Version   : 0.1
# URL    : 
# Aim       : 
# ===============================================================================


#' Define the imaging strategy
#'
#' Help define the imaging strategy depending on the camera and rig properties, distance from the bottom and position of reference transects.
#' 
#' @inheritParams swath_rig
#' @param fov_L a single numeric value expressing FOV in RADIANTS. 
#' @param cam_spacing a single numeric value expressing the spacing between two cameras
#' @param dist  a single numeric value expressing the distance between the camera and the plane of interest (e.g., the bottom, the ground)
#' @param min_camera_overlap Minimum desired overalp, expressed as a percentage, between frames of the (distal) camera during opposed filming passages (see details). It specifically refers to the overlap in swath of the one single camera that is at the distal end of a rig.
#' @param transect_pos the posistion on the bottom of the a reference transect (if used) relative to the center of the movement of the swath of the rig. Options are "side" or "center".
#' @param corridor_width Distance between two consecutive reference transect lines. Defalult to NULL.
#' 
#'
#'  
#' @details
#'
#' A filming passage consists in one leg of the lawn mower path, so that two adjacent passages are usually filmed moving in opposit directions and are meant to have an overlap when they cross.
#' When `transect_pos` is 'side', the imaging strategy consists in running one passage on one side of a reference transect, and one opposite passage on the other side of the transect taking care of having a deired degree of verlap.
#' When `transect_pos` is 'center', the imaging strategy consists in running one passage on the transect. This strategy is most meaningful if used with corridors.    
#'
#' `min_camera_overlap` is the _minimum_ desired overlap between opposites passages. When `transect_pos` is 'side', it defines the _maximum_ distance to keep from the transect to avoid obtaining an overlap _less than_ the `min_camera_overlap`.
#' When `transect_pos` is 'center', `min_camera_overlap` is considered the minimum desired overlap between opposites extra passages: the overlap is used a threshold to define spacing of passages, so the actual overlap among passages could be higher.
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @seealso
#' 
#' [swath_rig], [overlap_pc_cam], [FOV], [WID]
#' 
#' @examples
#' 
#' imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='side',min_camera_overlap=50, n_cams=3, corridor_width=NULL)
#' imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='side',min_camera_overlap=50, n_cams=3, corridor_width=600)
#' 	
#' imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='center',min_camera_overlap=50, n_cams=3, corridor_width=NULL)
#' imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='center',min_camera_overlap=50, n_cams=3, corridor_width=600)
#' 	
#' 	
#' 
#' @export

imaging_strategy<-function(fov_L, dist, cam_spacing, min_camera_overlap=NULL, n_cams, transect_pos=c('side'), corridor_width=NULL){

	# args check
	if (!is.numeric(fov_L)) stop ('fov_L must be numeric expressing FOV in RADIANTS')
	if (!is.numeric(dist)) stop ('dist must be numeric')
	if (!is.numeric(cam_spacing)) stop ('cam_spacing must be numeric')
	if (!is.numeric(n_cams)) stop ('n_cams must be numeric')
	if (!is.numeric(min_camera_overlap)) stop ('min_camera_overlap must be numeric and expressed in %')
	if (! transect_pos %in% c('side','center') ) stop ('transect_pos must be either "side" or "center"')
	arg_len<-c(length(fov_L),length(dist),length(cam_spacing),length(n_cams))!=1
	if (sum(arg_len)>0 ) stop (paste0('arguments "fov_L", "dist", "cam_spacing", "n_cams" need to be a single value' ))


	# A) Solving distance of the center of the rig from the reference transect 
	cam_ov<-min_camera_overlap/100
	# calculate thw rig and camera swath
	cam_swath<-WID(fov_L,dist) 
	rig_swath<-as.numeric(swath_rig(fov_L=fov_L, n_cams=n_cams, dist=dist, cam_spacing=cam_spacing))
	hlf_sw<- rig_swath/2
	
	if (transect_pos=='side'){
		# calclulate size of the frame corresponding to the overalp 
		cam_ov_size<-cam_ov*cam_swath
		# calculate distance of the transect from bord of the frame (the transect should lay in the middle of the overlap)
		dist_transect<- cam_ov_size/2
		# calculate the distance of the center of the rig from the reference transect 
		Xswcnt<-hlf_sw-dist_transect
	} else { # case in which the center of the swath is alligned on the trancect
		Xswcnt<-0
	}

	# report answers to A	
	res2<-res<-data.frame(info=NA, val=NA)		
	if (transect_pos=='side') {
		res[1,]<- c('overlap desired on the transect :', min_camera_overlap )
		} else {
		res[1,]<- c('minimum overlap desired between passages :', min_camera_overlap )	
		}
	res[2,]<- c('distance between swaths centers :', ifelse(transect_pos=='side',2*Xswcnt, 'N/A'))
	res[3,]<- c('maximum horizontal distance to keep from transect :', Xswcnt)
	

	# Scenario with corridor
	if (!is.null(corridor_width))  {

	cam2rig_overlap_fun<- function( dist_centers){
				# complementary distance of the half swath  
				c_hlf_sw<-dist_centers-hlf_sw
				# overalp relative to the camera swath: if >= 1 then the distal camera is completly overalpped (not necessairily by only one camera on the opposite rig swath)
				(hlf_sw-c_hlf_sw)/cam_swath 
		}
	
	# B) How much overlap between the swath of the camera at the distal end of the rig pointing towards the interior of the corridor AND the swath of the rig of the opposite passage (i.e., when both the rig swaths include the transect lines delimiting the corridor)
	#  calculate the distance between the 2 swaths centers
	Dc1c2<- corridor_width - (2*Xswcnt) 	
	empty<- Dc1c2-(2*hlf_sw)
	#  the swaths do not touch: there is space between them
	if (empty > 0 ) cam2rig_overlap<-NA 
	#  the swaths do touch but the overlap is zero
	if (empty == 0 ) cam2rig_overlap<-0
	#  swaths do overlap
	if (empty < 0 ) {
		# overalp relative to the camera swath: if >= 1 then the distal camera is completly overalpped (not necessairily by only one camera on the opposite rig swath)
		# print('ok')
		cam2rig_overlap<- cam2rig_overlap_fun(Dc1c2)	
		# # complementary distance of the half swath  
		# c_hlf_sw<-Dc1c2-hlf_sw
		# # overalp relative to the camera swath: if >= 1 then the distal camera is completly overalpped (not necessairily by only one camera on the opposite rig swath)
		# cam2rig_overlap<-(hlf_sw-c_hlf_sw)/cam_swath 
	}
		
	res2[1,]<-c('corridor width :',corridor_width)
	res2[2,]<-c('distance from the bottom :',dist)
	res2[3,]<-c('rig swath :',rig_swath)
	res2[4,]<-c('% overlap with no extra passages :',cam2rig_overlap*100)

	# C) Scenario with Not enough overlap (or not at all) between opposite passages.
	# C1) how many extra passages to have desidered overlap (i.e., min_camera_overlap)
	if (is.na(cam2rig_overlap) | (cam2rig_overlap < cam_ov) ) {
		new_cam2rig_ov<-0
		extra_pass<-0
		div<-1
		while(new_cam2rig_ov<cam_ov){
				div<-div+1
				# at least one extra pass is required
				extra_pass<-extra_pass+1
				# distance of the new center from the reference transect 
				centers_spacing<-(Dc1c2/div)
				new_c<-centers_spacing + Xswcnt 
				Dcc<-new_c-Xswcnt	
				new_empty<- Dcc-(2*hlf_sw)
				if (new_empty < 0 ) 
				new_cam2rig_ov<-cam2rig_overlap_fun(Dcc)	

			}
		dists_to_trans<-Xswcnt + centers_spacing*(1:extra_pass)
		ind<-dists_to_trans>(corridor_width/2)
		dists_to_trans[ind]<-corridor_width-dists_to_trans[ind]
		names(dists_to_trans)<- rep("side 1", length(dists_to_trans))
		names(dists_to_trans)[ind]<-'side 2'
		
		res2[5,]<-c('required extra passages :',extra_pass)
		res2[6,]<-c('% overlap between extra passages :',new_cam2rig_ov*100)
		dist_labs<- paste(names(dists_to_trans),round(dists_to_trans,2), sep='=' )
		res2[7,]<-c('distance of extra passages from transect :',paste(dist_labs, collapse=' | '))

		}


	return(list(res,res2))


	}
	return(res)

}


# # bench
# # --------


# imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='side',min_camera_overlap=50, n_cams=3, corridor_width=NULL)
# debug(imaging_strategy)
# imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='side',min_camera_overlap=50, n_cams=3, corridor_width=600)
	
	
	

# imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='center',min_camera_overlap=50, n_cams=3, corridor_width=NULL)
# # debug(imaging_strategy)
# imaging_strategy(fov_L=1.42, dist=100, cam_spacing=41, transect_pos='center',min_camera_overlap=50, n_cams=3, corridor_width=600)
	
	
	

