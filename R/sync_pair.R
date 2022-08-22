# ===============================================================================
# Name   	: Pair syncronized images
# Author 	: Filippo Ferrario
# Date   	: 19-08-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	:  
# ===============================================================================


#' Pair syncronized images
#' 
#' 
#'			   
#'
#' @inheritParams estimate_sync
#' @param ref_data dataset with estimated reference points.
#' @param use_validated logical. Wheter using validated reference points or not (the default). Not used yet
#' 
#' @details
#' 
#' The reference point could be validated or not.
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export



pair_synced<-function(source=NULL, ref_data=NULL, use_validated=F, output_path=NULL,shooting_group='shooting_group', cameraID='cameraID', camera_role='camera_role', sequence='sequence', ref_point='ref_point', file_name='file_name')
{
	# Check args
		if (class(ref_data)!='data.frame' | is.null(ref_data)) stop('ref_data must be a data.frame')
		if (sum(!ref_data[,ref_point] %in% c('start', 'end'))>0) stop('ref_point in ref_data must be either "start" or "end"')
		if (!is.character(source)) stop('source must be a character')
		if (!is.character(output_path)) stop('output_path must be a character')
		if (!is.character(shooting_group)) stop('shooting_group must be a character')
		if (!is.character(cameraID)) stop('cameraID must be a character')
		if (!is.character(camera_role)) stop('camera_role must be a character')
		if (!is.character(sequence)) stop('sequence must be a character')
		if (!is.character(ref_point)) stop('ref_point must be a character')
		if (!is.character(file_name)) stop('file_name must be a character')

	 
	#
	# obtain file list with relative path
	rel_path<-list.files(source,recursive=T)

	data_split<- split(ref_data, f=paste(ref_data[,shooting_group],ref_data[,cameraID],ref_data[,sequence]))
	pos_lst<-lapply(data_split, function(x){ #browser()
				strt<-grep(rel_path, pattern=x[x[,ref_point]=='start',file_name], value=F)
				end<-grep(rel_path, pattern=x[x[,ref_point]=='end',file_name], value=F)
				pos<-rel_path[strt:end]
				out_df<-cbind(x[1,!names(x) %in% c(ref_point, file_name,'type','validate')],file_name=pos, pic=1:length(pos))
				out_df
		})
	long<-as.data.frame(data.table::rbindlist(pos_lst))
	# long<-long[,!names(long)==camera_role]
	wide<-tidyr::pivot_wider(long, id_cols=c(shooting_group,sequence,pic),names_from=cameraID,values_from=file_name)
	write.csv(wide, file=output_path, quote=FALSE, row.names=FALSE)
		# wide
}

# BENCH
# ===================================


# ref_pts<-read.csv('C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-image_sync-esitmated_ref.csv')

# source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s'

# pair_synced(source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s' ,output_path='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-paired_synced.csv',  ref_data=ref_pts , shooting_group='shooting_group' , cameraID='cameraID' , camera_role='camera_role' , sequence='sequence' , ref_point='ref_point' , file_name='file_name')


# ref_data=ref_pts
# shooting_group='shooting_group'
# cameraID='cameraID'
# camera_role='camera_role'
# sequence='sequence'
# ref_point='ref_point'
# file_name='file_name'
