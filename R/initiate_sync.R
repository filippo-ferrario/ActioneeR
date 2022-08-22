# ===============================================================================
# Name   	: Initiate Synchronization dataframe		
# Author 	: Filippo Ferrario	
# Date   	: 22-08-2022 [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: Create a template dataframe to save the information of reference points to be used in the synchronization of the images.
# ===============================================================================

#' Initiate Synchronization dataframe
#' 
#' Create a template dataframe to save the information of reference points to be used in the synchronization of the images
#' 
#' 
#' @param output_path absolute or relative path to the output file (.csv). It must end with the file name!
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export 


initiate_sync<-function(output_path=NULL)
{
	# check args
	if (!is.character(output_path)) stop('output_path must be a character')
	if( grepl(output_path, pattern='\\.csv')==FALSE) stop('output_path must be a path to a .csv file')

	# initiat dataframe

	init<-data.frame(shooting_group=NA, cameraID=NA,camera_role=NA,sequence=NA,ref_point=NA,file_name=NA)

	if (file.exists(output_path)) {
		alarm()
		message(paste0('=========================================================================\nThe file ',output_path,' is already existing! \n========================================================================='))
		repeat { 
			# print('==========================================')
			resp<-readline(prompt="Do you want to overwrite? (Y/N): ")
			if (resp %in% c('Y','N')) { break } 
		}
		
		if (resp=='Y' ){
			write.csv(init, file=output_path, quote=FALSE, row.names=FALSE)
			print(paste0('Dataset initiated and saved here:', normalizePath(output_path, winslash='/')))

			} else {
				print('change name/path and try again')
			}
	} else {
		write.csv(init, file=output_path, quote=FALSE, row.names=FALSE)
		print(paste0('Dataset initiated and saved here: ', normalizePath(output_path, winslash='/')))

	}

	


}

# initiate_sync(output_path='./data/BIC-PROVA-image_sync.csv')