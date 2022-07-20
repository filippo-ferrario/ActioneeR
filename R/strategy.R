# ===============================================================================
# Name      :  
# Author    : Filippo Ferrario
# Date      : 19-07-2022 [dd-mm-yyyy]
# Version   : 0.1
# URL    : 
# Aim       : 
# ===============================================================================


#'
#'
#' Help define filming strategy
#'
#' @param min_camera_overlap Minimum desired overalp, expressed as a percentage, between frames of the (distal) camera during opposed filming passages. It specifically refers to the overlap in swath of the one single camera that is at the distal end of a rig.
#' @param corridor_length Distance between two consecutive reference transect lines. Defalult to NULL.
#'
#' @details
#'
#' A filming passage consists in one leg of the lawn mower path, so that two adjacent passages are usually filmed moving in opposit directions and are meant to have an overlap when they cross.
#'
#'

strategy_side_line<-function(fov_L, dist, cam_spacing, min_camera_overlap=NULL, n_cams, corridor_length=NULL){

	# args check

	# A) Solving distance of the center of the rig from the reference transect 
	cam_ov<-min_camera_overlap/100
	# calculate thw rig and camera swath
	cam_swath<-WID(fov_L,dist) 
	rig_swath<-as.numeric(swath_rig(fov_L=fov_L, n_cams=n_cams, dist=dist, cam_spacing=cam_spacing))
	hlf_sw<- rig_swath/2
	# calclulate size of the frame corresponding to the overalp 
	cam_ov_size<-cam_ov*cam_swath
	# calculate distance of the transect from bord of the frame (the transect should lay in the middle of the overlap)
	dist_transect<- cam_ov_size/2
	# calculate the distance of the center of the rig from the reference transect 
	Xswcnt<-hlf_sw-dist_transect

	# report answers to A	
	res2<-res<-data.frame(info=NA, val=NA)		
	res[1,]<- c('overlap desired on the transect :', min_camera_overlap )
	res[2,]<- c('swim distance from transect :', Xswcnt)

	# Scenario with corridor
	if (!is.null(corridor_length))  {

	cam2rig_overlap_fun<- function( dist_centers){
				# complementary distance of the half swath  
				c_hlf_sw<-dist_centers-hlf_sw
				# overalp relative to the camera swath: if >= 1 then the distal camera is completly overalpped (not necessairily by only one camera on the opposite rig swath)
				(hlf_sw-c_hlf_sw)/cam_swath 
		}
	
	# B) How much overlap between the swath of the camera at the distal end of the rig pointing towards the interior of the corridor AND the swath of the rig of the opposite passage (i.e., when both the rig swaths include the transect lines delmiting the corridor)
	#  calculate the distance between the 2 swaths centers
	Dc1c2<- corridor_length - (2*Xswcnt) 	
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
		
	res2[1,]<-c('corridor length :',corridor_length)
	res2[2,]<-c('distance from the bottom :',dist)
	res2[3,]<-c('rig swath :',rig_swath)
	res2[4,]<-c('overlap with now extra passages :',cam2rig_overlap)

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
		ind<-dists_to_trans>(corridor_length/2)
		dists_to_trans[ind]<-corridor_length-dists_to_trans[ind]
		names(dists_to_trans)<-'side 1'
		names(dists_to_trans)[ind]<-'side 2'
		}

	res2[5,]<-c('required extra passages :',extra_pass)
	res2[6,]<-c('overlap beteween extra passages :',new_cam2rig_ov)
	dist_labs<- paste(names(dists_to_trans),round(dists_to_trans,2), sep='=' )
	res2[7,]<-c('distance of extra passages from transect :',paste(dist_labs, collapse=' | '))

	return(list(res,res2))


	}
	return(res)

}


# bench
# --------


strategy_side_line(fov_L=1.42, dist=100, cam_spacing=41, min_camera_overlap=50, n_cams=3, corridor_length=NULL)
debug(strategy_side_line)
strategy_side_line(fov_L=1.42, dist=100, cam_spacing=41, min_camera_overlap=50, n_cams=3, corridor_length=600)
	
	
	
	
	

