# ===============================================================================
# Name   	: Estimate imagery syncronization
# Author 	: Filippo Ferrario
# Date   	: 10-08-2022 [dd-mm-yyyy]
# Version	: 0.2
# URL		: 
# Aim    	: Given a set of reference points in a sequence of images taken at regular intervals (i.e., the file name of an image corresponding to a known scene, such as the beginning or the end of a chunk of images of interest),
#			  the function should estimate the file name of corresponding images in a different image sequence taken at the same regular interval.
#			   
# ===============================================================================


#' Estimate reference points in images sequences 
#' 
#'
#' Given a set of reference points in a sequence of images taken at regular intervals (i.e., the file name of an image corresponding to a known scene, such as the beginning or the end of a chunk of images of interest),
#' the function estimates the file name of corresponding images in a different image sequence taken at the same regular interval.
#'			   
#'
#' @param source absolute or relative path to the folder/drive where the subfolders with the image files of the individual cameras are stored. 
#' @param ref_data dataframe with information about reference points in the sequence for Master and initial ref points for slaves.
#' @param output_path absolute or relative path to the output file (.csv). It must end with the file name
#' @param shooting_group name of the column in the dataset specifying the group of picture taken in the same shooting session 
#' @param cameraID name of the column in the dataset grouping the picture by camera 
#' @param camera_role name of the column in the dataset specifying the role of the camera. That is 'master' for the camera used to retrive reference points, 'slave' for the other cameras that need to be synchronized with the 'master'
#' @param sequence name of the column in the dataset grouping the images into a continuous sequence within the shooting session (e.g., a transect from start to end)
#' @param ref_point name of the column in the dataset specifying if the reference point is the 'start' or the 'end' of the sequence 
#' @param file_name name of the column in the dataset specifying the file name of the image taken as reference point
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export



estimate_sync<-function(source=NULL, ref_data=NULL, output_path=NULL,shooting_group='shooting_group', cameraID='cameraID', camera_role='camera_role', sequence='sequence', ref_point='ref_point', file_name='file_name')
{		
		# check args
		if (class(ref_data)!='data.frame' | is.null(ref_data)) stop('ref_data must be a data.frame')
		if (sum(ref_data[,camera_role] %in% c('master', 'slave'))!= length(ref_data[,camera_role])) stop('camera_role in ref_data must be either "master" or "slave"')
		if (sum(!ref_data[,ref_point] %in% c('start', 'end'))>0) stop('ref_point in ref_data must be either "start" or "end"')
		if (!is.character(source)) stop('source must be a character')
		if (!is.character(output_path)) stop('output_path must be a character')
		if (!is.character(shooting_group)) stop('shooting_group must be a character')
		if (!is.character(cameraID)) stop('cameraID must be a character')
		if (!is.character(camera_role)) stop('camera_role must be a character')
		if (!is.character(sequence)) stop('sequence must be a character')
		if (!is.character(ref_point)) stop('ref_point must be a character')
		if (!is.character(file_name)) stop('file_name must be a character')

		# obtain file list with relative path
		rel_path<-list.files(source,recursive=T)

		# # create unique annotations
		# ref_data$annot<-paste(ref_data[,camera_role],ref_data[,cameraID],ref_data[,sequence],ref_data[,ref_point], sep=' ' ) 
		
		# define combinations of masters and slaves
		# ID_role<-paste0(ref_data[,cameraID],ref_data[,camera_role])
		ID_role<-paste(ref_data[,shooting_group],ref_data[,cameraID],ref_data[,camera_role], ref_data[,sequence])
		uni_ID_role<-unique(ID_role)
		masters<-grep(uni_ID_role, pattern='master', value=TRUE)
		slaves<-grep(uni_ID_role, pattern='slave', value=TRUE)
		
		def_slaves<-lapply(masters, function(x) { #browser()
										ms_cam<-unlist(strsplit(x, split='master'))
										# sl_cam<-slaves[-grep(slaves,pattern=ms_cam)]
										sl_cam<-slaves[grep(slaves,pattern=ms_cam[2])]
										sl_cam
										})
		names(def_slaves)<-masters
		def_slaves<-def_slaves[!lengths(def_slaves)==0]

		# order ref_data to make sure to define the correct pairing of starts and ends refpoint in a sequence.

		ref_data_split<-split(ref_data, f=paste(ref_data[,shooting_group], ref_data[,cameraID],ref_data[,camera_role]))
		ord_ref_data_split<-lapply(ref_data_split, function(x) x[order(x[,file_name]),])
		ref_data_master<-ord_ref_data_split[grepl(ord_ref_data_split, pattern='master')]
		

		# isolate the starting points for salves
		# ref_data_slave<-ord_ref_data_split[grepl(ord_ref_data_split, pattern='slave')]
		ref_data_slave<-ref_data[ref_data[camera_role]=='slave',]
		ref_data_slave<-split(ref_data_slave, f=paste(ref_data_slave[,shooting_group], ref_data_slave[,cameraID],ref_data_slave[,camera_role],ref_data_slave[,sequence]))


		# ref_data_split<-split(ref_data, f=ref_data[,shooting_group])
		# ord_ref_data_master<-lapply(ref_data_master, function(x) x[order(x[,cameraID],x[,file_name]),])

		

		# define lengths intervals of files in master(s)
		master_starts<-lapply(ref_data_master, function(x) x[ x[,camera_role] =='master' & x[,ref_point] =='start',]	)
		master_ends<-lapply(ref_data_master, function(x)x[ x[,camera_role] =='master' & x[,ref_point] =='end',]	)
		
		mst_start_pathID<-lapply(master_starts, function(x) grep(rel_path, pattern=paste0(x[,file_name], collapse='|')))
		mst_end_pathID<-lapply(master_ends, function(x) grep(rel_path, pattern=paste0(x[,file_name], collapse='|')))

	
		# find lengths of good and junk sequences
		mst_seq_len<-mapply(x=mst_end_pathID,y=mst_start_pathID, function(x,y) x-y)

		mst_junk_len<-mapply(x=mst_start_pathID,y=mst_end_pathID, function(x,y) { #browser()
															x<-x[-1]
															y<-y[-length(y)]
															jnk_len<-x-(y+1)
															jnk_len
															})
		# check that lengths are plausible
		invisible(mapply(x=mst_junk_len,z=mst_end_pathID, function(x,z){ # browser()
										checkID<-x<0
										sum_check<-sum(checkID)
										if (sum_check>0) {
												fls<-unlist( strsplit(rel_path[z[which(checkID)]], split='/'))
												fls<-fls[length(fls)]
												print(paste(fls ,'is a bad reference point'))
																	stop('Check input data')
																	} 
										})
		)


		# estimate reference points for slaves
		lsnam<-names(mst_seq_len) 

		# lsnam<-lsnam[2]
		# for( i in 1:length(lsnam)){ 
		out3<-lapply(lsnam, function(x) { #browser()
		 	
		 	slvs<-def_slaves[[ grep(names(def_slaves), pattern=x) ]]
		 	
		 	# error check
		 	if (sum(lengths(ref_data_slave[slvs])==0)>0) stop('Unspecified slaves: For each master, make sure to have defined the first starts point of each slave!')
		 	# estimate reference points from starting position of each slave and save it as a dataframe list
		 	out2<-lapply(ref_data_slave[slvs] , function(k) { #browser()
		 			# file position of start ref point
		 			pathID<-grep(rel_path, pattern=k[,file_name] )
		 			# lenght of good and junk sequences for the master in use
		 			seq_len<-unlist(mst_seq_len[x])
		 			jnks<-unlist(mst_junk_len[x])
					new_ref<-NULL
					tmp_start<-pathID
					# find pathID for new start and ends for a slave
		 			for (i in 1:length(seq_len)){
		 							temp_ref1<-tmp_start+seq_len[i]
		 							if (i<length(seq_len)){
		 									 		temp_ref2<-temp_ref1+(jnks[i]+1)
		 									 		tmp<-c(temp_ref1,temp_ref2)
		 									 		tmp_start<-temp_ref2
		 									 		} else {
		 									 			tmp<-temp_ref1
		 									 		}
		 							new_ref	<- c(new_ref, tmp)	
		 									}
		 			# initialize datafrem output using master dataframe as template						
		 			out<- ord_ref_data_split[x][[1]]
		 			out<-out[2:nrow(out),]
		 			# find files of reference points
		 			ref_fls<-sapply(rel_path[new_ref], function(x) {
		 										j<-unlist(strsplit(x, split='/'))
		 										j<-j[length(j)]
		 										j<-unlist(strsplit(j, split='\\.'))
		 										j[1]
		 										})

		 			out[,file_name]<- ref_fls
		 			out[,c(cameraID,camera_role)]<-k[,c(cameraID,camera_role)]
		 			# out<-rbind(k,out)
		 			out

		 		})

		 	out2


			} )

		out4<-lapply(out3, function(x) data.table::rbindlist(x))
		out4<-data.table::rbindlist(out4)

		ref_data$type<-'manual'
		ref_data$validate<-'validated'
		out4$type<-'estimate'
		out4$validate<-''

		out5<-rbind(ref_data,out4)
		out5<-out5[order(out5[,shooting_group],out5[,sequence],out5[,camera_role],out5[,cameraID],out5[,file_name]),]
		# out5[1:20,]
		write.csv(out5, file=output_path, quote=FALSE, row.names=FALSE)
		out5
}



# Bench
# =========

# ref_annBAD<-read.csv('C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-image_sync-BAD.csv')

ref_ann<-read.csv('C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-image_sync.csv')
# 
# source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s'
# estimate_sync(source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s' ,output_path='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-image_sync-esitmated_ref.csv',  ref_data=ref_ann , shooting_group='shooting_group' , cameraID='cameraID' , camera_role='camera_role' , sequence='sequence' , ref_point='ref_point' , file_name='file_name')


# ref_data=ref_ann
# shooting_group='shooting_group'
# cameraID='cameraID'
# camera_role='camera_role'
# sequence='sequence'
# ref_point='ref_point'
# file_name='file_name'
