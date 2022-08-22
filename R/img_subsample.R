# ===============================================================================
# Name   	: Subsample image sequence	
# Author 	: Filippo Ferrario
# Date   	: 22-08-2022 [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: Subsample a sequence of images by copying files at a specified interval
# ===============================================================================


#' Subsample image sequence	
#' 
#' Subsample a sequence of images by copying files at a specified interval
#' 
#' @inheritParams estimate_sync
#' @param paired_synced csv created by `pair_synced` with path of synced files.
#' @param interval interval used for subsampling. 
#' @param dest absolute path to where the files need to be copied
#' @param copy Logical. If TRUE (Default) it copies the subsampled files in the `dest` folder keeping the folder structure. If FALSE, only a .csv list of the subsampled files is saved in `dest`.
#' 
#' @details
#' 
#' The 'interval' at which frequency files are selected: if interval = 3 then the 3rd, 6th, 9th, 12th... are selected, along with the first and last file of the sequence. 
#' 
#' If  
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export


img_subsample<-function(source=NULL, paired_synced=NULL, dest=NULL, interval=1, copy=TRUE, shooting_group='shooting_group' , cameraID='cameraID' , sequence='sequence')
{
	# Check args
		if (class(paired_synced)!='data.frame' | is.null(paired_synced)) stop('ref_data must be a data.frame')
		if (!is.character(source)) stop('source must be a character')
		if (!is.character(dest)) stop('dest must be a character')
		if (!is.character(shooting_group)) stop('shooting_group must be a character')
		if (!is.character(cameraID)) stop('cameraID must be a character')
		if (!is.character(sequence)) stop('sequence must be a character')
		if (!is.numeric(interval)) stop('interval must be a whole number')
		if (!is.logical(copy)) stop('copy must be TRUE or FALSE')


		# avoid that dest is a subfolder of source
		source_pth<-normalizePath(source,winslash='/')
		if (grepl(dest, pattern='\\./') ) {
			dest_pth<-sub(dest,pattern='\\./', replacement='~/')
			dest_pth<-path.expand(dest_pth)
			dest_pth<-gsub(dest_pth, pattern='\\\\',replacement='/')
			} else {dest_pth<-dest}  
		if(source_pth==dest_pth)  stop('dest should not be the same as source, nor a subfolder of source')
		dest_base<-basename(dest_pth)
		dest_parent<-sub(dest_pth, pattern=paste0('/',dest_base), replacement='')
		if(source_pth==dest_parent)  stop('dest should not be the same as source, nor a subfolder of source')



	#
	seq_split<-split(paired_synced, f=paste(paired_synced[,shooting_group], paired_synced[,sequence]))

	sub_split<-lapply(seq_split, function(x)
			{	
				# browser()
				# define which file to keep : first, last and those depending on the interval. Remove 0 from seq
				ids<-c(1,seq(0,nrow(x), interval)[-1], nrow(x))  
				# remove duplicate if last file is also the last from seq
				ids<-ids[!duplicated(ids)]
				x[ids,]
			}
			)
	sub_split_df<- as.data.frame(data.table::rbindlist(sub_split))
	
	# create dest path and copy file to destination
	dir.create(dest, recursive=TRUE)

	if (copy==F){
		write.csv(sub_split_df, file=paste0(dest,'/subsample_list.csv'), quote=FALSE, row.names=FALSE)
	} else {
		write.csv(sub_split_df, file=paste0(dest,'/subsample_list.csv'), quote=FALSE, row.names=FALSE)
		long<-tidyr::pivot_longer(sub_split_df,cols=c(-shooting_group, -sequence,-"pic"), names_to=cameraID, values_to='file_name')
		# recover folder structure from path in file_name
		dirs<-sub(long$file_name, pattern='/[^/]+\\..{3}', replacement='')
		dirs<-unique(dirs)
		lapply(paste0(dest,'/',dirs), function(x) dir.create(x, recursive=TRUE))
		file.copy(from=paste0(source,'/',long$file_name),to=paste0(dest,'/',long$file_name))
	}

	sub_split_df
	alarm()

}





# BENCH
# ===================================


# paired<-read.csv('C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-paired_synced.csv')

# source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s'
# dest='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/sub-int_5'

# img_subsample(source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s' ,output_path='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/data/BIC-PIL-paired_synced.csv',  ref_data=ref_pts , shooting_group='shooting_group' , cameraID='cameraID' , camera_role='camera_role' , sequence='sequence' , ref_point='ref_point' , file_name='file_name')


# paired_synced=paired
# interval=5
# shooting_group='shooting_group'
# sequence='sequence'
# cameraID='cameraID'
# copy=FALSE
# # camera_role='camera_role'
# # ref_point='ref_point'
# file_name='file_name'



img_subsample(source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/TL_1s' ,
				dest='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-BIC-mosaicing/PIL/sub100', 
				 paired_synced=paired, 
				 interval=300,
				 shooting_group='shooting_group' , 
				cameraID='cameraID' 
				)